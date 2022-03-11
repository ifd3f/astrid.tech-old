{-# LANGUAGE DuplicateRecordFields #-}
module Seams.Importing where


import Seams.InputSchema
import System.FilePath
import Data.Text.Lazy(Text)
import Data.Functor
import qualified Data.Text.Lazy.IO as TLIO
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Aeson
import Data.Functor.Compose


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

readTree :: FilePath -> IO (FileResult String Text)
readTree = undefined

-- | A container for data associated with importing from a specific file path.
newtype FileRead a = FileRead {
  -- | If it returns Right, then consume that file.
  -- | If it returns Left, then redirect to a different file.
  useFile :: FilePath -> Either (FilePath, FileRead a) (ReadResult -> a)
}

type FileReadResult e a = Compose FileRead (Either e) a
type ReadResult = Either String Text

contents :: FileRead ReadResult
contents = FileRead (\_ -> Right id)

liftRedirect :: Either (FilePath, FileRead a) (ReadResult -> a) -> FileRead a
liftRedirect r = FileRead $ const r

consumer :: (ReadResult -> a) -> FileRead a
consumer f = FileRead $ \_ -> Right f

path :: FileRead FilePath
path = FileRead $ Right . const

instance Functor FileRead where
  fmap f (FileRead makeA)
    = FileRead (\p -> case makeA p of
                        Right consume -> Right (f . consume)
                        Left (redirect, next) -> Left (redirect, fmap f next))

instance Applicative FileRead where
  pure a = consumer (const a)

  (FileRead makeF) <*> (FileRead makeA)
    = FileRead (\p -> case (makeF p, makeA p) of
                          (Right consumeF, Right consumeA) -> Right (\t -> consumeF t $ consumeA t)
                          (f, Left (redirectA, nextA)) ->
                            Left (redirectA, liftRedirect f <*> nextA)
                          (Left (redirectF, nextF), a) ->
                            Left (redirectF, nextF <*> liftRedirect a))

cdAbs :: FilePath -> FileRead FilePath
cdAbs p' = liftRedirect (Left (p', pure p'))

cd :: FilePath -> FileRead FilePath
cd p' = FileRead $ \p -> Left (p </> p', pure p')

data Document m = Document {
  location :: FilePath,
  meta :: m,
  content :: Content
}

instance Functor Document where
  fmap f (Document l m c) = Document l (f m) c

data Content = Content {
  cType :: ContentType,
  body :: Text
}

loadDocument :: FromJSON m => FileRead (Document m)
loadDocument = undefined

loadGenericDocument :: FileRead (Either String (Document Value))
loadGenericDocument = undefined

data UnresolvedContent
  = Embedded ContentType Text
  | AnotherFile FilePath

resolveContent :: UnresolvedContent -> FileReadResult String Content
resolveContent (Embedded cType body) = Compose $ pure (Right $ Content cType body)
resolveContent (AnotherFile path)
  = let cType = Compose $ traverse pure $ extensionToContentType $ takeExtension path
        body = Compose $ contents <* cd path
      in Content <$> cType <*> body

