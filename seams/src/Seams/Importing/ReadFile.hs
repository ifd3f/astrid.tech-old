module Seams.Importing.ReadFile where
import Data.Functor.Identity
import Data.ByteString ( ByteString )
import System.FilePath
import Data.Yaml
import Control.Exception
import qualified Data.ByteString as BS
import System.Directory
import System.IO.Error
import Data.Functor
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Either.Utils
import Data.Either.Combinators

newtype ReadError = ReadError String
data ReadResult content = File content | Dir [String] | Empty deriving Show

instance Functor ReadResult where
  fmap f (File c) = File (f c)
  fmap _ (Dir children) = Dir children
  fmap _ Empty = Empty

newtype ReadFileT f m a = ReadFileT {
  runReadFileT :: (FilePath -> m (ReadResult f)) -> m a
}

type ReadFileT' = ReadFileT ByteString
type ReadFile f = ReadFileT f Identity

envRead :: FilePath -> ReadFileT f m (ReadResult f)
envRead path = ReadFileT $ \rf -> rf path

getReadFile :: Applicative m => ReadFileT f m (FilePath -> m (ReadResult f))
getReadFile = ReadFileT $ \rf -> pure rf

newtype NonexistentFile = NonexistentFile FilePath

asFile :: ReadResult a -> Maybe a
asFile (File f) = Just f
asFile _ = Nothing

asDir :: ReadResult a -> Maybe [String]
asDir (Dir c) = Just c
asDir _ = Nothing

envRead' :: Functor m => (ReadResult f -> Maybe a) -> e -> FilePath -> ExceptT e (ReadFileT f m) a
envRead' sel err path = ExceptT $ envRead path <&> sel <&> maybeToEither err

envReadYAML :: (FromJSON a, Monad m) => FilePath -> ReadFileT' m (Either (Maybe ParseException) a)
envReadYAML path = runExceptT $ do
  file <- envRead' asFile Nothing path
  liftEither $ mapLeft Just $ decodeEither' file

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
  f <*> a = ReadFileT $ \rf ->
    runReadFileT f rf <*> runReadFileT a rf

instance Monad m => Monad (ReadFileT f m) where
  rft >>= f = ReadFileT $ \rf ->
    runReadFileT rft rf >>= (\x -> runReadFileT (f x) rf)
  _ >> r = r -- if we did not need a previous result, we don't run it

instance MonadTrans (ReadFileT f) where
  lift m = ReadFileT $ const m

ioReadFile :: FilePath -> ExceptT IOError IO (ReadResult ByteString)
ioReadFile path = ExceptT $ file `catchIO` const dir `catchIO` emptyHandler
  where
    catchIO = catch  -- cute trick to catch IOErrors without type system complaining
    file = Right . File <$> BS.readFile path
    dir = Right . Dir <$> listDirectory path
    emptyHandler e
      | isDoesNotExistError e = pure $ Right Empty
      | otherwise = pure $ Left e

