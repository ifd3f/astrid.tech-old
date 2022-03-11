{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing where

import Seams.InputSchema
import System.FilePath
import Data.Frontmatter hiding ( Result )
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Aeson hiding ( Result )
import Data.Functor.Compose
import Data.ByteString.Builder


newtype Error = Error String
data WithPath a = WithPath FilePath a

data FileTree a
  = Dir { name :: String, children :: FileTree a }
  | File { name :: String, value :: a }

instance Functor FileTree where
  fmap f (Dir n cs) = Dir n (fmap f cs)
  fmap f (File n v) = File n (f v)

instance Foldable FileTree where
  foldMap f (Dir n cs) = foldMap f cs
  foldMap f (File n v) = f v

type FileResult e a = FileTree (Either e a)

readTree :: FilePath -> IO (FileResult String ByteString)
readTree = undefined

-- | A container for data associated with importing from a specific file path.
newtype Loader a = Loader {
  -- | If it returns Right, then consume that file.
  -- | If it returns Left, then redirect to a different file.
  useFile :: FilePath -> Either (FilePath, Loader a) (ReadResult -> a)
}

runLoader loader path = case useFile loader path of
  Right consume -> (path, consume)
  Left (redirect, next) -> runLoader next redirect

type ReadError = String
type LoaderEither e a = Compose Loader (Either e) a
type ReadResult = Either ReadError ByteString
type Result a = LoaderEither ReadError a

toResult :: Loader a -> LoaderEither e a
toResult a = Compose (fmap Right a)

contents :: LoaderEither String ByteString
contents = Compose $ Loader (\_ -> Right id)

liftRedirect :: Either (FilePath, Loader a) (ReadResult -> a) -> Loader a
liftRedirect r = Loader $ const r

redirectOnce path consume = liftRedirect $ Left (path, consumer consume)

consumer :: (ReadResult -> a) -> Loader a
consumer f = Loader $ \_ -> Right f

wrapFR :: a -> Loader a
wrapFR a = consumer (const a)

path :: Loader FilePath
path = Loader $ Right . const

instance Functor Loader where
  fmap f (Loader makeA)
    = Loader (\p -> case makeA p of
                        Right consume -> Right (f . consume)
                        Left (redirect, next) -> Left (redirect, fmap f next))

instance Applicative Loader where
  pure = wrapFR

  (Loader makeF) <*> (Loader makeA)
    = Loader (\p -> case (makeF p, makeA p) of
                          (Right consumeF, Right consumeA) -> Right (\t -> consumeF t $ consumeA t)
                          (f, Left (redirectA, nextA)) ->
                            Left (redirectA, liftRedirect f <*> nextA)
                          (Left (redirectF, nextF), a) ->
                            Left (redirectF, nextF <*> liftRedirect a))

-- | Redirect to a different directory relative to current.
cd :: FilePath -> Loader a -> Loader a
cd p' (Loader use) = Loader $ \p -> case use p of
  Right consume -> Left (p </> p', Loader use)
  Left (redirect, next) -> Left (redirect </> p', next)

-- | cd, but on the Result level.
-- cdr :: FilePath -> Result a -> Result a
-- cdr p' ld = Compose $ Right <$> cd p' ld

data Document m = Document FilePath m Content

instance Functor Document where
  fmap f (Document l m c) = Document l (f m) c

data Content = Content {
  cType :: ContentType,
  body :: ByteString
}

-- loadDocument :: FromJSON m => LoaderEither String (Document m)
-- loadDocument = contents <* cdr
--   where contentFilePath = dropExtension <$> path

-- loadContentFile :: FilePath -> LoaderEither String Content
-- loadContentFile path
--   = let cType = Compose $ pure $ extensionToContentType $ takeExtension path
--         body = contents <* fmap pure cd path
--       in Content <$> cType <*> body

-- resolveContentA :: LoaderEither String UnresolvedContent -> LoaderEither String Content

-- parseDocument :: (FromJSON m) => FilePath -> Either String ByteString -> Either String Content -> Either String (Document m)
-- parseDocument path docContent bodyContent = do
--   dType <- extensionToDocumentType $ takeExtension path
--   docContent' <- docContent
-- 
--   case dType of
--       FrontmatterMarkdown ->
--         case parseYamlFrontmatter (BL.toStrict docContent') of
--           Done rest fm -> Right $ DocumentFile fm (Embedded Markdown (toLazyByteString rest))
--           Fail _ _ err -> Left err
--           Partial _ -> Left "Incomplete input"
--       YAML -> do
--         bodyContent' <- bodyContent
--         docContent'' <- decodeEither docContent'
--         return $ Document path docContent'' bodyContent'

