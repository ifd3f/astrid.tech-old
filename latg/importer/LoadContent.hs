module LATG.Importer.LoadContent where

import System.FilePath 
import qualified Data.Text as T

data DocumentType a =
    | Markdown a T.Text
    | YAML a  -- also includes JSON
    | TOML a

data ContentSourceType = 
    | EmbeddedMarkdown T.Text
    | Src FilePath
    | EmbeddedPlaintext T.Text
    | SameName FilePath

readContent :: DocumentType 
