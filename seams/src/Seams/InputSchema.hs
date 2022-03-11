{-# LANGUAGE DuplicateRecordFields #-}

module Seams.InputSchema where
import Data.Text.Lazy(Text)
import Data.Time
import Data.Char
import Data.Aeson

data DocumentType = FrontmatterMarkdown | YAML

extensionToDocumentType :: String -> Either String DocumentType
extensionToDocumentType ext = case map toLower ext of
  ".md" -> Right FrontmatterMarkdown
  ".markdown" -> Right FrontmatterMarkdown
  ".yml" -> Right YAML
  ".yaml"  -> Right YAML
  other -> Left other

data ContentField = FileRef { ctype :: Maybe ContentType, file :: FilePath } | EmbeddedPlaintext { text :: Text }

data ContentType = Markdown | HTML | Plaintext

extensionToContentType :: String -> Either String ContentType
extensionToContentType ext = case map toLower ext of
  ".md" -> Right Markdown
  ".markdown" -> Right Markdown
  ".html" -> Right HTML
  ".htm"  -> Right HTML
  ".txt"  -> Right Plaintext
  other -> Left other

data PostMeta = PostMeta {
  title :: Maybe Text,
  slug :: Maybe Text,
  tagline :: Maybe Text,
  thumbnail :: Maybe Text,
  tags :: [Text],
  time :: Timestamps,
  ordinal :: Integer
}

data PostSlug = PostSlug Int Int Int Int (Maybe String)

data ProjectMeta = ProjectMeta {
  title :: Text,
  tagline :: Text,
  thumbnail :: Maybe Text,
  tags :: [Text],
  time :: Timestamps,
  started :: LocalTime,
  ended :: Maybe LocalTime,
  ordinal :: Integer
}

data Timestamps = Timestamps {
  created :: LocalTime,
  modified :: LocalTime,  
  published :: LocalTime
}

