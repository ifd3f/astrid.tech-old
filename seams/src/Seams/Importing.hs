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
import Control.Applicative
import Data.Foldable
import Control.Monad.Trans.Reader
import qualified Data.Bifunctor
import Control.Monad.Trans.Class
import Control.Monad


newtype Error = Error String
data WithPath a = WithPath FilePath a

data FileTreeT m a
  = Dir { filename :: String, ls :: m [FileTreeT m a] }
  | File { filename :: String, value :: m a }

instance Functor m => Functor (FileTreeT m) where
  fmap f (Dir n cs) = Dir n (map (f <$>) <$> cs)
  fmap f (File n v) = File n (f <$> v)

instance Applicative m => Semigroup (FileTreeT m a) where
  Dir dname ls <> Dir "" rs = Dir dname $ liftA2 (++) ls rs
  Dir "" ls <> Dir dname rs = Dir dname $ liftA2 (++) ls rs

  File fname val <> Dir dname cs = Dir dname ((File fname val :) <$> cs)
  Dir dname cs <> File fname val = File fname val <> Dir dname cs

  l <> r = Dir "" $ pure [l, r]

instance Applicative m => Monoid (FileTreeT m a) where
  mempty = Dir "" $ pure []

instance Applicative m => Applicative (FileTreeT m) where
  pure = File "" . pure

  File pf f <*> File pa a = File (pf </> pa) (f <*> a)
  Dir n cs <*> a = Dir n (map (<*> a) <$> cs)
  f <*> Dir n cs = Dir n (map (f <*>) <$> cs)

instance Alternative m => Alternative (FileTreeT m) where
  empty = mempty
  (<|>) = (<>)

instance Foldable m => Foldable (FileTreeT m) where
  foldMap f (File _ a) = foldMap f a
  foldMap f (Dir _ cs) = mconcat $ map (foldMap f) (fold cs)

newtype RedirectFileT m a = RedirectFileT {
  runFindFile :: FilePath -> m (FilePath, a)
}

cd :: Applicative m => FilePath -> RedirectFileT m ()
cd p' = RedirectFileT $ \p -> pure (p </> p', ())

cd' :: Applicative m => FilePath -> RedirectFileT m ()
cd' p = RedirectFileT $ const $ pure (p, ())

pwd :: Applicative m => RedirectFileT m FilePath
pwd = RedirectFileT $ \p -> pure (p, p)

instance Functor m => Functor (RedirectFileT m) where
  fmap f redir = RedirectFileT $ fmap (Data.Bifunctor.second f) . runFindFile redir

instance Applicative m => Applicative (RedirectFileT m) where
  pure a = RedirectFileT $ \p -> pure (p, a)
  f <*> a = RedirectFileT $ \p ->
    (\(pf, f') (pa, a') -> (pa, f' a')) <$> runFindFile f p <*> runFindFile a p

instance Monad m => Monad (RedirectFileT m) where
  return a = RedirectFileT $ \p -> return (p, a)

  redir >>= f = RedirectFileT $ \p0 -> do
    (p1, r1) <- runFindFile redir p0
    runFindFile (f r1) p1

instance MonadTrans RedirectFileT where
  lift c = RedirectFileT $ \p -> do
    result <- c
    return (p, result)

-- readFile :: ReadFileT m 

type ReadError = String
type ReadResult = Either ReadError ByteString

newtype ReadFileT m a = ReadFileT {
  runReadFileT :: (FilePath -> m ReadResult) -> m a
}

envReadFile :: FilePath -> ReadFileT m ReadResult
envReadFile path = ReadFileT $ \rf -> rf path

instance Functor m => Functor (ReadFileT m) where
  fmap f a = ReadFileT $ fmap f . runReadFileT a

instance Applicative m => Applicative (ReadFileT m) where
  pure a = ReadFileT $ const $ pure a
  f <*> a = ReadFileT $ \rf ->
    runReadFileT f rf <*> runReadFileT a rf

instance Monad m => Monad (ReadFileT m) where
  return = pure
  a >>= f = ReadFileT $ \rf ->
    runReadFileT a rf >>= (\result -> runReadFileT (f result) rf)

instance MonadTrans ReadFileT where
  lift c = ReadFileT $ const c

type TransformFileT = ReaderT ReadResult

runTransformFileT :: ReaderT ReadResult m a -> ReadResult -> m a
runTransformFileT = runReaderT

fileContents :: Monad m => TransformFileT m ReadResult
fileContents = ask

type LoaderT m = RedirectFileT (TransformFileT m)

-- runLoaderT :: Monad m => LoaderT m a -> FilePath -> (FilePath -> m ReadResult) -> m a
-- runLoaderT l path readFileImpl = do
--   (path', x) <- runFindFile l path
--   pure $ runTransformFileT x file

  -- let readFileResult = runReadFileT redirectResult
  -- runTransformFileT readFileResult


-- type LoaderEither m e a = Compose (Loader m) (Either e) a
-- type Result m a = LoaderEither m ReadError a

-- insertEither :: Loader m a -> LoaderEither m e a
-- insertEither a = Compose (fmap Right a)
-- 
-- contents :: LoaderEither m String ByteString
-- contents = Compose $ Loader (\_ -> Right id)
-- 
-- alwaysConsume :: (env ReadResult -> env a) -> Loader m a
-- alwaysConsume c = Loader $ const (Right c)
-- 
-- constLoad :: a -> Loader m a
-- constLoad a = consumer (const a)
-- 
-- path :: Loader m FilePath
-- path = Loader $ Right . const

-- -- | Redirect to a different directory relative to current.
-- cd :: FilePath -> Loader a -> Loader a
-- cd p' (Loader use) = Loader $ \p -> case use p of
--   Right consume -> Left (p </> p', Loader use)
--   Left (redirect, next) -> Left (redirect </> p', next)

-- -- | cd, but on the Result level.
-- cdr :: FilePath -> Result a -> Result a
-- cdr p' ld = Compose $ cd p' (getCompose ld)
-- 
-- data Document m = Document FilePath m Content
-- 
-- instance Functor Document where
--   fmap f (Document l m c) = Document l (f m) c
-- 
-- data Content = Content {
--   cType :: ContentType,
--   body :: ByteString
-- }
-- 
-- -- loadDocument :: FromJSON m => LoaderEither String (Document m)
-- loadDocument = cdr
--   where contentFilePath = dropExtension <$> path
-- 
-- -- loadContentFile :: LoaderEither String a -> LoaderEither String Content
-- loadContentFile path
--   = let cType = Compose $ pure $ extensionToContentType $ takeExtension path
--         body = cd path
--       in Content <$> cType <*> body
-- 
-- -- resolveContentA :: LoaderEither String UnresolvedContent -> LoaderEither String Content
-- 
-- -- parseDocument :: (FromJSON m) => FilePath -> Either String ByteString -> Either String Content -> Either String (Document m)
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

