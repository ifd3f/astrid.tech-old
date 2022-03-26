{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing where

import System.FilePath
import Seams.InputSchema
import Data.ByteString (ByteString)
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Frontmatter
import Data.Yaml
import qualified Data.Yaml as Yaml
import Control.Monad.Trans.Except
import Control.Arrow
import Data.Either.Combinators
import Data.Either.Utils
import Data.Functor.Identity 

newtype ReadError = ReadError String
type ReadResult = Either ReadError

newtype ReadFileT m a = ReadFileT {
  runReadFileT :: (FilePath -> m ByteString) -> m a
}

type ReadFile = ReadFileT Identity

envReadFile :: FilePath -> ReadFileT m ByteString
envReadFile path = ReadFileT $ \rf -> rf path

instance Functor m => Functor (ReadFileT m) where
  fmap f rft = ReadFileT $ fmap f . runReadFileT rft

instance Applicative m => Applicative (ReadFileT m) where
  pure a = ReadFileT $ const (pure a)
  f <*> a = ReadFileT $ \rf ->
    runReadFileT f rf <*> runReadFileT a rf 

instance Monad m => Monad (ReadFileT m) where
  rft >>= f = ReadFileT $ \rf ->
    runReadFileT rft rf >>= (\x -> runReadFileT (f x) rf)
  l >> r = r -- if we did not need a previous result, we don't run it

instance MonadTrans ReadFileT where
  lift m = ReadFileT $ const m

instance MonadFail m => MonadFail (ReadFileT m) where
  fail x = ReadFileT $ const (fail x)

data Document m = Document FilePath m Content

instance Functor Document where
  fmap f (Document l m c) = Document l (f m) c

data Content = Content FilePath ContentType ByteString

newtype LoadError = LoadError String

-- | Representation of a document file that references some content.
data DocumentFile a = DocumentFile a Content

loadDocument path = case extensionToDocumentType $ takeExtension path of
  Just dType -> loadDocAsType dType path
  Nothing -> fail $ "unknown doctype at path " ++ path

loadDocAsType YAML path =
  let
    contentPath = takeBaseName path
    contentExt = takeExtension contentPath
  in do
    dContent <- envReadFile path

    (doc, contentType) <- either fail pure $ do
      doc <- mapLeft show $ decodeEither' dContent
      contentType <- maybeToEither ("unsupported content type at " ++ contentPath) $ extensionToContentType contentExt
      pure (doc, contentType)

    contentBody <- envReadFile contentPath
    pure $ Document path doc (Content contentPath contentType contentBody)

loadDocAsType FrontmatterMarkdown path = do
  dContent <- envReadFile path
  case parseYamlFrontmatter dContent of
    Fail _ _ err -> fail err
    Partial _ -> fail "Incomplete input"
    Done rest fm -> pure $ Document path fm (Content path Markdown rest)

