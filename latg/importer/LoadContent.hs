{-# LANGUAGE LambdaCase #-}

module LATG.Importer.LoadContent where

import Data.Bifunctor
import Data.Char(toLower)
import qualified Data.Frontmatter as FM
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

instance Functor EncodedDocument where
  fmap f (DocumentWithMarkdown doc content) = DocumentWithMarkdown (f doc) content
  fmap f (DocumentOnly doc) = DocumentOnly (f doc)

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

type Result = Either String

readDocument :: Aeson.FromJSON a => String -> BL.ByteString -> Result (Maybe (EncodedDocument a))
readDocument extension content = 
  case extension of
    ".md" -> markdown
    ".json" -> json
    ".yml" -> yaml
    ".yaml" -> yaml
    ".toml" -> toml
    _ -> Right Nothing
  where
    rj = Right . Just

    json = Just <$> DocumentOnly <$> Aeson.eitherDecode content 

    markdown = case FM.parseFrontmatter $ BL.toStrict content of
      FM.Done body front -> case Yaml.decodeEither' front of
        Left err -> Left $ show err
        Right x -> rj $ DocumentWithMarkdown x body
      _ -> Right Nothing

    yaml = Just <$> DocumentOnly <$>
      (first show $ Yaml.decodeEither' $ BL.toStrict content)

    toml = do
      table <- first show $ Toml.parseTomlDoc "" $ TE.decodeUtf8 $ BL.toStrict content 
      case Aeson.fromJSON $ Aeson.toJSON table of
        Aeson.Error err -> Left $ show err
        Aeson.Success x -> rj $ DocumentOnly x

extractContentSource :: FilePath -> EncodedDocument (Maybe FSch.Content) -> Result ContentSourceType
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

loadContentSource :: ContentSourceType -> IO (Result (ContentType, BS.ByteString))
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

transformContent :: ContentType -> BS.ByteString -> T.Text
transformContent contentType raw = TE.decodeUtf8 raw  -- TODO use pandoc

createInsertableDocument :: TL.Text -> EncodedDocument a -> ISch.DbDocument
createInsertableDocument contentHtml document = undefined