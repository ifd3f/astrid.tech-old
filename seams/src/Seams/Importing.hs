{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Seams.Importing where


import Seams.InputSchema
import System.FilePath
import Data.Frontmatter
import Data.ByteString.Lazy(ByteString)
import Data.Functor
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

readTree :: FilePath -> IO (FileResult String ByteString)
readTree = undefined

-- | A container for data associated with importing from a specific file path.
newtype FileRead a = FileRead {
  -- | If it returns Right, then consume that file.
  -- | If it returns Left, then redirect to a different file.
  useFile :: FilePath -> Either (FilePath, FileRead a) (ReadResult -> a)
}

type FileReadResult e a = Compose FileRead (Either e) a
type ReadResult = Either String ByteString

toResult :: FileRead a -> FileReadResult e a
toResult a = Compose (fmap Right a)

contents :: FileReadResult String ByteString
contents = Compose $ FileRead (\_ -> Right id)

liftRedirect :: Either (FilePath, FileRead a) (ReadResult -> a) -> FileRead a
liftRedirect r = FileRead $ const r

consumer :: (ReadResult -> a) -> FileRead a
consumer f = FileRead $ \_ -> Right f

wrapFR :: a -> FileRead a
wrapFR a = consumer (const a)

path :: FileRead FilePath
path = FileRead $ Right . const

instance Functor FileRead where
  fmap f (FileRead makeA)
    = FileRead (\p -> case makeA p of
                        Right consume -> Right (f . consume)
                        Left (redirect, next) -> Left (redirect, fmap f next))

instance Applicative FileRead where
  pure = wrapFR

  (FileRead makeF) <*> (FileRead makeA)
    = FileRead (\p -> case (makeF p, makeA p) of
                          (Right consumeF, Right consumeA) -> Right (\t -> consumeF t $ consumeA t)
                          (f, Left (redirectA, nextA)) ->
                            Left (redirectA, liftRedirect f <*> nextA)
                          (Left (redirectF, nextF), a) ->
                            Left (redirectF, nextF <*> liftRedirect a))

instance Monad FileRead where
  (FileRead make) >>= f
    = FileRead (\p -> case make p of
                          Right consume -> Left (p, next $ \t -> make t)
                          Left (redirect, next) -> Left (redirect, next >>= f))

cdAbs :: FilePath -> FileRead FilePath
cdAbs p' = liftRedirect (Left (p', pure p'))

cd :: FilePath -> FileRead FilePath
cd p' = FileRead $ \p -> Left (p </> p', pure p')

data Document m = Document FilePath m Content

instance Functor Document where
  fmap f (Document l m c) = Document l (f m) c

data Content = Content {
  cType :: ContentType,
  body :: ByteString
}

-- loadDocument :: FromJSON m => FileReadResult String (Document m)
loadDocument
  = let cont = fmap documentFileContent loadDocumentFile
        meta = documentFileMeta <$> loadDocumentFile
      in cont -- Document <$> Compose (fmap pure path) <*> meta <*> cont

data DocumentFile m = DocumentFile {
  documentFileMeta :: m,
  documentFileContent :: UnresolvedContent
}

data UnresolvedContent
  = Embedded ContentType ByteString
  | AnotherFile FilePath

resolveContent :: UnresolvedContent -> FileReadResult String Content
resolveContent (Embedded cType body) = Compose $ pure (Right $ Content cType body)
resolveContent (AnotherFile path)
  = let cType = Compose $ pure $ extensionToContentType $ takeExtension path
        body = contents <* fmap pure cd path
      in Content <$> cType <*> body

loadDocumentFile :: (FromJSON m) => FileReadResult String (DocumentFile m)
loadDocumentFile = Compose $ fmap parseDocumentFile path <*> getCompose contents

parseDocumentFile :: (FromJSON m) => FilePath -> Either String ByteString -> Either String (DocumentFile m)
parseDocumentFile path content = do
  dType <- extensionToDocumentType $ takeExtension path
  content' <- content
  case dType of
      FrontmatterMarkdown ->
        case parseYamlFrontmatter (toStrict content') of
          Done rest fm -> Right $ DocumentFile fm (Embedded Markdown (toLazyByteString rest))
          Fail _ _ err -> Left err
          Partial _ -> Left "Incomplete input"
      YAML -> DocumentFile <$> decodeEither content' <*> pure (AnotherFile $ dropExtension path)

