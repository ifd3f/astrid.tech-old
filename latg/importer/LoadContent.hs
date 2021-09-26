{-# LANGUAGE LambdaCase #-}

module LATG.Importer.LoadContent where

import System.FilePath 
import qualified LATG.Importer.FileSchema as FSch
import qualified LATG.Importer.InsertSchema as ISch
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml as Yaml
import qualified Text.Toml as Toml
import qualified Data.Aeson as Aeson
import Data.Frontmatter
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

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
  = PlainType
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
    Just src -> Right $ FileRef src
    Nothing -> withoutExtension
  where withoutExtension = Right $ FileRef $ dropExtension path

loadContentSource :: ContentSourceType -> (ContentType, IO BL.ByteString)
loadContentSource source = undefined

transformContent :: ContentType -> BL.ByteString -> TL.Text
transformContent contentType raw = undefined

createInsertableDocument :: TL.Text -> EncodedDocument a -> ISch.DbDocument
createInsertableDocument contentHtml document = undefined