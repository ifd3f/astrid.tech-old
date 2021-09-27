{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LATG.Importer.LoadContent where

import Data.Bifunctor ( Bifunctor(first) )
import Data.Char(toLower)
import qualified Data.Frontmatter as FM
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import qualified LATG.Importer.FileSchema as FSch
import qualified Text.Toml as Toml
import System.FilePath
    ( dropExtension, (</>), takeDirectory, takeExtension )

data EncodedDocument a = EncodedDocument
  { attachedContent :: Maybe (ContentType, BS.ByteString)
  , documentData :: a
  }
  deriving (Show, Eq)

documentOnly :: a -> EncodedDocument a
documentOnly = EncodedDocument Nothing

instance Functor EncodedDocument where
  fmap f (EncodedDocument content doc) = EncodedDocument content (f doc)

data ContentSourceType
  = EmbeddedContent ContentType BS.ByteString
  | FileRef FilePath
  deriving (Show, Eq)

data ContentType
  = PlaintextType
  | MarkdownType
  | HTMLType
  deriving (Show, Eq)

type Result = Either String

data NonDocument = Invalid String | NonDocument 
  deriving (Show, Eq)
type ReadDocumentResult = Either NonDocument

fromEither :: Result a -> ReadDocumentResult a
fromEither (Left err) = Left $ Invalid err
fromEither (Right x) = Right x

toEither :: ReadDocumentResult a -> Result (Maybe a)
toEither (Left (Invalid err)) = Left err
toEither (Left NonDocument) = Right Nothing
toEither (Right x) = Right $ Just x

invalid :: String -> ReadDocumentResult a
invalid err = Left $ Invalid err

load :: FilePath -> IO (ReadDocumentResult (FSch.GenericDocument, T.Text))
load path = do
  rawDoc <- BL.readFile path

  case readDocument (takeExtension path) rawDoc of 
    Left e -> return $ Left e
    Right doc -> case extractContentSource path $ fmap FSch.content doc of
      Left e -> return $ invalid e
      Right cs -> do
        result <- loadContentSource cs
        case result of 
          Left e -> return $ invalid e
          Right (contentType, contentRaw) -> do
            transformed <- transformContent contentType contentRaw 
            return $ case transformed of
              Left e -> invalid e
              Right transformed' -> Right (documentData doc, transformed')

readDocument :: Aeson.FromJSON a => String -> BL.ByteString -> ReadDocumentResult (EncodedDocument a)
readDocument extension content = 
  case extension of
    ".md" -> markdown
    ".json" -> fromEither json
    ".yml" -> fromEither yaml
    ".yaml" -> fromEither yaml
    ".toml" -> fromEither toml
    _ -> Left NonDocument
  where
    json = documentOnly <$> Aeson.eitherDecode content 

    markdown = case FM.parseFrontmatter $ BL.toStrict content of
      FM.Done body front -> case Yaml.decodeEither' front of
        Left err -> invalid $ show err
        Right x -> Right $ EncodedDocument (Just (MarkdownType, body)) x
      _ -> Left NonDocument

    yaml = documentOnly <$> first show (Yaml.decodeEither' $ BL.toStrict content)

    toml = do
      table <- first show $ Toml.parseTomlDoc "" $ TE.decodeUtf8 $ BL.toStrict content 
      case Aeson.fromJSON $ Aeson.toJSON table of
        Aeson.Error err -> Left $ show err
        Aeson.Success x -> Right $ documentOnly x

extractContentSource :: FilePath -> EncodedDocument (Maybe FSch.Content) -> Result ContentSourceType
extractContentSource _ (EncodedDocument (Just (contentType, contentRaw)) contentData) = case contentData of
  Nothing -> Right $ EmbeddedContent contentType contentRaw
  Just _ -> Left "Embedded content cannot reference other sources"
extractContentSource path (EncodedDocument Nothing contentData) = case contentData of
  Nothing -> withoutExtension
  Just (FSch.EmbeddedPlaintext text) -> Right $ EmbeddedContent PlaintextType $ TE.encodeUtf8 text
  Just (FSch.FileRef srcMaybe _) -> case srcMaybe of
    Nothing -> withoutExtension
    Just src -> Right $ FileRef $ takeDirectory path </> src
  where 
    withoutExtension = Right $ FileRef $ dropExtension path

loadContentSource :: ContentSourceType -> IO (Result (ContentType, BS.ByteString))
loadContentSource (EmbeddedContent contentType body) = pure $ Right (contentType, body)
loadContentSource (FileRef path) = do
  content <- BS.readFile path

  return $ case map toLower $ takeExtension path of
    ".html" -> Right (HTMLType, content)
    ".htm" -> Right (HTMLType, content)
    ".md" -> Right (MarkdownType, content)
    ".txt" -> Right (PlaintextType, content)
    _ -> Left $ "Unsupported content file " ++ path

transformContent :: ContentType -> BS.ByteString -> IO (Result T.Text)
transformContent _ raw = pure $ Right $ TE.decodeUtf8 raw  -- TODO use pandoc
