module Seams.Importing.ReadFile where

import Control.Exception
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import System.IO.Error

newtype ReadError =
  ReadError String

data ReadResult content
  = File content
  | Dir [String]
  | Empty
  deriving (Show, Eq)

instance Functor ReadResult where
  fmap f (File c) = File (f c)
  fmap _ (Dir children) = Dir children
  fmap _ Empty = Empty

newtype ReadFileT f m a =
  ReadFileT
    { runReadFileT :: (FilePath -> m (ReadResult f)) -> m a
    }

type ReadFileT' = ReadFileT ByteString

envRead :: FilePath -> ReadFileT f m (ReadResult f)
envRead path = ReadFileT $ \rf -> rf path

getReadFile :: Applicative m => ReadFileT f m (FilePath -> m (ReadResult f))
getReadFile = ReadFileT $ \rf -> pure rf

newtype NonexistentFile =
  NonexistentFile FilePath

-- | Returns a list of all files under this.
envWalkFiles :: (Monad m) => FilePath -> ReadFileT f m [FilePath]
envWalkFiles path = do
  result <- envRead path
  case result of
    Empty -> return []
    File _ -> return [path]
    Dir children -> do
      childPaths <-
        let childPaths = (map (path </>) $ filter (/= "..") children)
         in traverse envWalkFiles childPaths
      return $ concat childPaths

instance Functor m => Functor (ReadFileT f m) where
  fmap f rft = ReadFileT $ fmap f . runReadFileT rft

instance Applicative m => Applicative (ReadFileT f m) where
  pure a = ReadFileT $ const (pure a)
  f <*> a = ReadFileT $ \rf -> runReadFileT f rf <*> runReadFileT a rf

instance Monad m => Monad (ReadFileT f m) where
  rft >>= f =
    ReadFileT $ \rf -> runReadFileT rft rf >>= (\x -> runReadFileT (f x) rf)
  _ >> r = r -- if we did not need a previous result, we don't run it

instance MonadTrans (ReadFileT f) where
  lift m = ReadFileT $ const m

ioReadFile :: FilePath -> ExceptT IOError IO (ReadResult ByteString)
ioReadFile path = ExceptT $ file `catchIO` const dir `catchIO` emptyHandler
  where
    catchIO = catch -- cute trick to catch IOErrors without type system complaining
    file = Right . File <$> BS.readFile path
    dir = Right . Dir <$> listDirectory path
    emptyHandler e
      | isDoesNotExistError e = pure $ Right Empty
      | otherwise = pure $ Left e
