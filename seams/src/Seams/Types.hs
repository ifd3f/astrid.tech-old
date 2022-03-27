{-# LANGUAGE OverloadedStrings #-}

module Seams.Types where
import Data.String
import Data.Maybe
import Data.List ( stripPrefix )
import Data.Char
import Database.Persist
import Database.Persist.Types
import Data.Either.Utils
import qualified Data.Text as T
import Database.Persist.Postgresql
-- | Supported types of documents. Each document corresponds to a 
-- | project or blog post or something like that.
data DocumentType = FrontmatterMarkdown | YAML
  deriving (Show)

extensionToDocumentType :: String -> Maybe DocumentType
extensionToDocumentType ext = case s' of
  ".md" -> Just FrontmatterMarkdown
  ".markdown" -> Just FrontmatterMarkdown
  ".yml" -> Just YAML
  ".yaml"  -> Just YAML
  _ -> Nothing
  where
    l = map toLower ext
    s' = fromMaybe l $ stripPrefix "." l

-- | Supported formats for the body of documents.
data ContentType = Markdown | HTML | Plaintext | Jupyter
  deriving (Show)

contentTypeToExtension :: IsString p => ContentType -> p
contentTypeToExtension ct = case ct of
  Markdown -> "md"
  HTML -> "html"
  Plaintext -> "txt"
  Jupyter -> "ipynb"
  
extensionToContentType :: String -> Maybe ContentType
extensionToContentType ext
  | s' == "html" = Just HTML
  | s' `elem` ["ipynb", "jupyter"] = Just Jupyter
  | s' `elem` ["md", "markdown"] = Just Markdown
  | s' `elem` ["txt", "text", "plaintext"] = Just Plaintext
  | otherwise = Nothing
  where
    l = map toLower ext
    s' = fromMaybe l $ stripPrefix "." l

instance PersistFieldSql ContentType where
  sqlType _ = SqlString

instance PersistField ContentType where
  toPersistValue = PersistText . contentTypeToExtension

  fromPersistValue (PersistText v) = maybeToEither ("Invalid text " `T.append` v) $ extensionToContentType $ T.unpack v
  fromPersistValue v = Left $ "Invalid type " `T.append` T.pack (show v)

