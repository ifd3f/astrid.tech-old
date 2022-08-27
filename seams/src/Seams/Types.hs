{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Seams.Types where

import Control.Lens.TH
import Data.Char
import Data.Either.Utils
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql

-- | Supported types of documents. Each document corresponds to a 
-- | project or blog post or something like that.
data DocumentType
  = FrontmatterMarkdown
  | YAML
  deriving (Show, Eq)

extensionToDocumentType :: String -> Maybe DocumentType
extensionToDocumentType ext
  | s' `elem` ["md", "markdown"] = Just FrontmatterMarkdown
  | s' `elem` ["json", "yml", "yaml"] = Just YAML
  | otherwise = Nothing
  where
    s' = normalizeExtension ext

-- | Supported formats for the body of documents.
data ContentType
  = Markdown
  | HTML
  | Plaintext
  | Jupyter
  deriving (Show, Eq)

contentTypeToExtension :: ContentType -> String
contentTypeToExtension =
  \case
    Markdown -> "md"
    HTML -> "html"
    Plaintext -> "txt"
    Jupyter -> "ipynb"

extensionToContentType :: String -> Maybe ContentType
extensionToContentType ext
  | s' `elem` ["htm", "html"] = Just HTML
  | s' == "ipynb" = Just Jupyter
  | s' `elem` ["md", "markdown"] = Just Markdown
  | s' == "txt" = Just Plaintext
  | otherwise = Nothing
  where
    s' = normalizeExtension ext

normalizeExtension :: String -> String
normalizeExtension ext = fromMaybe l $ stripPrefix "." l
  where
    l = map toLower ext

instance PersistFieldSql ContentType where
  sqlType _ = SqlString

instance PersistField ContentType where
  toPersistValue = PersistText . T.pack . contentTypeToExtension
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
