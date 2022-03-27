module Seams.Importing.ReadFile where
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Data.ByteString ( ByteString )
import System.FilePath
import Data.Yaml

newtype ReadError = ReadError String
data ReadResult f = File f | Dir [String] deriving Show

newtype ReadFileT f m a = ReadFileT {
  runReadFileT :: (FilePath -> m (ReadResult f)) -> m a
}

type ReadFileT' = ReadFileT ByteString
type ReadFile f = ReadFileT f Identity

envRead :: FilePath -> ReadFileT f m (ReadResult f)
envRead path = ReadFileT $ \rf -> rf path

envReadFile :: (MonadFail m) => FilePath -> ReadFileT b m b
envReadFile path = do
  result <- envRead path
  case result of
    File f -> pure f
    Dir _ -> fail $ "Expected file, got dir at " ++ path

envReadDir :: (MonadFail m) => FilePath -> ReadFileT f m [String]
envReadDir path = do
  result <- envRead path
  case result of
    Dir d -> pure d
    File _ -> fail $ "Expected dir, got file at " ++ path

-- | Returns a list of all files under this.
envWalkFiles :: (Monad m) => FilePath -> ReadFileT f m [FilePath]
envWalkFiles path = do
  result <- envRead path
  case result of
    File _ -> return [path]
    Dir children -> do
      childPaths <-
        let childPaths = (map (path </>) $ filter (/= "..") children)
        in traverse envWalkFiles childPaths
      return $ concat childPaths

envReadYAML :: (MonadFail m, FromJSON b) => FilePath -> ReadFileT' m b
envReadYAML path = do
  content <- envReadFile path
  either (fail . show) pure $ decodeEither' content

instance Functor m => Functor (ReadFileT f m) where
  fmap f rft = ReadFileT $ fmap f . runReadFileT rft

instance Applicative m => Applicative (ReadFileT f m) where
  pure a = ReadFileT $ const (pure a)
  f <*> a = ReadFileT $ \rf ->
    runReadFileT f rf <*> runReadFileT a rf 

instance Monad m => Monad (ReadFileT f m) where
  rft >>= f = ReadFileT $ \rf ->
    runReadFileT rft rf >>= (\x -> runReadFileT (f x) rf)
  _ >> r = r -- if we did not need a previous result, we don't run it

instance MonadTrans (ReadFileT f) where
  lift m = ReadFileT $ const m

instance MonadFail m => MonadFail (ReadFileT f m) where
  fail x = ReadFileT $ const (fail x)

