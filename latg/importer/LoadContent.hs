{-# LANGUAGE LambdaCase #-}

module LATG.Importer.LoadContent where

import System.FilePath 
import LATG.Importer.FileSchema
import LATG.Importer.InsertSchema
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
  = DocumentWithContent a BS.ByteString
  | DocumentOnly a
  deriving (Show)

data ContentSourceType
  = EmbeddedMarkdown TL.Text
  | FileRef FilePath
  | EmbeddedPlaintext TL.Text
  | SameName FilePath
  deriving (Show)

data ContentType
  = PlainType
  | MarkdownType
  | HTMLType
  deriving (Show)

loadInsertableDocument :: FilePath -> IO DbDocument
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

    markdown = case parseYamlFrontmatter $ BL.toStrict content of
      Done body front -> case front of 
        Nothing -> NotADocument
        Just x -> ValidDocument $ DocumentWithContent x body
      _ -> InvalidDocument $ "Failure while parsing markdown"

    yaml = case Yaml.decodeEither' $ BL.toStrict content of
      Left err -> InvalidDocument $ show err
      Right x -> ValidDocument $ DocumentOnly x

    toml = case Toml.parseTomlDoc "" $ TE.decodeUtf8 $ BL.toStrict content of
      Left err -> InvalidDocument $ show err
      Right table -> case Aeson.fromJSON $ Aeson.toJSON table of
        Aeson.Error err -> InvalidDocument $ show err
        Aeson.Success x -> ValidDocument $ DocumentOnly x

extractContentSourceType :: FilePath -> EncodedDocument a -> ContentSourceType
extractContentSourceType path document = undefined

loadContentSource :: ContentSourceType -> (ContentType, IO BL.ByteString)
loadContentSource source = undefined

transformContent :: ContentType -> BL.ByteString -> TL.Text
transformContent contentType raw = undefined

createInsertableDocument :: TL.Text -> EncodedDocument a -> DbDocument
createInsertableDocument contentHtml document = undefined