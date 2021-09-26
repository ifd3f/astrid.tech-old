{-# LANGUAGE LambdaCase #-}

module LATG.Importer.LoadContent where

import Data.Frontmatter
import Data.Char(toLower)
import Data.List(isPrefixOf)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Yaml as Yaml
import qualified LATG.Importer.FileSchema as FSch
import qualified LATG.Importer.InsertSchema as ISch
import qualified Text.Toml as Toml
import System.FilePath

data EncodedDocument a
  = DocumentWithMarkdown a BS.ByteString
  | DocumentOnly a
  deriving (Show, Eq)

data ContentSourceType
  = EmbeddedMarkdown BS.ByteString
  | EmbeddedPlaintext T.Text
  | FileRef FilePath
  deriving (Show, Eq)

data ContentType
  = PlaintextType
  | MarkdownType
  | HTMLType
  deriving (Show, Eq)

loadInsertableDocument :: FilePath -> IO ISch.DbDocument
loadInsertableDocument = undefined

data ReadDocumentResult a = NotADocument | InvalidDocument String | ValidDocument a
  deriving (Show)

readDocument :: Aeson.FromJSON a => String -> BL.ByteString -> ReadDocumentResult (EncodedDocument a)
readDocument extension content = 
  case extension of
    ".md" -> markdown
    ".json" -> json
    ".yml" -> yaml
    ".yaml" -> yaml
    ".toml" -> toml
  where
    json = case Aeson.eitherDecode content of 
      Left err -> InvalidDocument err
      Right x -> ValidDocument $ DocumentOnly x

    markdown = case parseFrontmatter $ BL.toStrict content of
      Done body front -> case Yaml.decodeEither' front of
        Left err -> InvalidDocument $ show err
        Right x -> ValidDocument $ DocumentWithMarkdown x body
      _ -> NotADocument

    yaml = case Yaml.decodeEither' $ BL.toStrict content of
      Left err -> InvalidDocument $ show err
      Right x -> ValidDocument $ DocumentOnly x

    toml = case Toml.parseTomlDoc "" $ TE.decodeUtf8 $ BL.toStrict content of
      Left err -> InvalidDocument $ show err
      Right table -> case Aeson.fromJSON $ Aeson.toJSON table of
        Aeson.Error err -> InvalidDocument $ show err
        Aeson.Success x -> ValidDocument $ DocumentOnly x

extractContentSource :: FilePath -> EncodedDocument (Maybe FSch.Content) -> Either String ContentSourceType
extractContentSource _ (DocumentWithMarkdown content md) = case content of
  Nothing -> Right $ EmbeddedMarkdown md
  Just _ -> Left "Embedded markdown cannot reference other sources"
extractContentSource path (DocumentOnly content) = case content of
  Nothing -> withoutExtension
  Just (FSch.EmbeddedPlaintext text) -> Right $ EmbeddedPlaintext text
  Just (FSch.FileRef srcMaybe _) -> case srcMaybe of
    Nothing -> withoutExtension
    Just src -> Right $ FileRef $ takeDirectory path </> src
  where 
    withoutExtension = Right $ FileRef $ dropExtension path

loadContentSource :: ContentSourceType -> IO (Either String (ContentType, BS.ByteString))
loadContentSource (EmbeddedMarkdown md) = pure $ Right (MarkdownType, md)
loadContentSource (EmbeddedPlaintext txt) = pure $ Right (PlaintextType, TE.encodeUtf8 txt)
loadContentSource (FileRef path) = do
  content <- BS.readFile path

  return $ case map toLower $ takeExtension path of
    ".html" -> Right (HTMLType, content)
    ".htm" -> Right (HTMLType, content)
    ".md" -> Right (MarkdownType, content)
    ".txt" -> Right (PlaintextType, content)
    ext -> Left $ "Unsupported content file " ++ path

transformContent :: ContentType -> BL.ByteString -> TL.Text
transformContent contentType raw = undefined

createInsertableDocument :: TL.Text -> EncodedDocument a -> ISch.DbDocument
createInsertableDocument contentHtml document = undefined