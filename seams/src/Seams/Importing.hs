{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Seams.Importing where

import System.FilePath
import Seams.InputSchema
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Frontmatter

newtype ReadError = ReadError String
type ReadResult = Either ReadError

newtype ReadFileT m a = ReadFileT {
  runReadFileT :: (FilePath -> m (ReadResult ByteString)) -> m a
}

envReadFile :: FilePath -> ReadFileT m (ReadResult ByteString)
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
  l >> r = r

instance MonadTrans ReadFileT where
  lift m = ReadFileT $ const m

data Document m = Document FilePath m Content

instance Functor Document where
  fmap f (Document l m c) = Document l (f m) c

data Content = Content ContentType ByteString

newtype LoadError = LoadError String

-- loadDocument :: (FromJSON h, Monad m) => FilePath -> ReadFileT m (Document h)
loadDocument path = do
  pure 3
  where contentFilePath = dropExtension path

loadContentFile :: LoaderEither String a -> LoaderEither String Content
loadContentFile path
  = let cType = extensionToContentType path
      in do
          file <- envReadFile path
          file


-- | Representation of a document file that references some content.
data DocumentFile a = DocumentFile a ContentRef 
data ContentRef = Embedded ContentType ByteString | ExternalFile FilePath

parseDocumentFile ::
  (FromJSON m) =>
  FilePath ->
  Either String ByteString ->
  Either String (DocumentFile m)
parseDocumentFile path docContent = do
  dType <- extensionToDocumentType $ takeExtension path
  docContent' <- docContent
  case dType of
      FrontmatterMarkdown ->
        case parseYamlFrontmatter (BL.toStrict docContent') of
          Done rest fm -> Right $ DocumentFile fm (Embedded Markdown (toLazyByteString rest))
          Fail _ _ err -> Left err
          Partial _ -> Left "Incomplete input"
      YAML -> do
        docContent'' <- decodeEither docContent'
        return $ DocumentFile docContent'' (ExternalFile $ takeBaseName path)

