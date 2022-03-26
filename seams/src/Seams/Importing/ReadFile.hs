module Seams.Importing.ReadFile where
import Data.ByteString
import Data.Functor.Identity
import Control.Monad.Trans.Class

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
  _ >> r = r -- if we did not need a previous result, we don't run it

instance MonadTrans ReadFileT where
  lift m = ReadFileT $ const m

instance MonadFail m => MonadFail (ReadFileT m) where
  fail x = ReadFileT $ const (fail x)

