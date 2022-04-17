{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Seams.Types where

import Control.Lens.TH
import Data.Char
import Data.Either.Utils
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql

-- | Supported types of documents. Each document corresponds to a 
-- | project or blog post or something like that.
data DocumentType
  = FrontmatterMarkdown
  | YAML
  deriving (Show)

extensionToDocumentType :: String -> Maybe DocumentType
extensionToDocumentType ext =
  case s' of
    ".md" -> Just FrontmatterMarkdown
    ".markdown" -> Just FrontmatterMarkdown
    ".yml" -> Just YAML
    ".yaml" -> Just YAML
    _ -> Nothing
  where
    l = map toLower ext
    s' = fromMaybe l $ stripPrefix "." l

-- | Supported formats for the body of documents.
data ContentType
  = Markdown
  | HTML
  | Plaintext
  | Jupyter
  deriving (Show)

contentTypeToExtension :: IsString p => ContentType -> p
contentTypeToExtension ct =
  case ct of
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
  fromPersistValue (PersistText v) =
    maybeToEither ("Invalid text " `T.append` v) $
    extensionToContentType $ T.unpack v
  fromPersistValue v = Left $ "Invalid type " `T.append` T.pack (show v)

type Color = Text

data TagColors =
  TagColors
    { _bg :: Maybe Color
    , _text :: Maybe Color
    }

makeLenses ''TagColors

instance Semigroup TagColors
  -- text and bg comes as a single unit, so rhs annihilates lhs
                                                                where
  _ <> x = x

instance Monoid TagColors where
  mempty = TagColors Nothing Nothing

type TagColorMap = Map Text TagColors

unionColorMaps :: Foldable m => m TagColorMap -> TagColorMap
unionColorMaps = foldl (M.unionWith (<>)) M.empty
