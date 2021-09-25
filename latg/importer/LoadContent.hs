module LATG.Importer.LoadContent where

import System.FilePath 
import LATG.Importer.FileSchema
import LATG.Importer.InsertSchema
import Data.ByteString as BS
import qualified Data.Text as T

data EncodedDocument a
  = FMMarkdown a T.Text
  | YAML a  
  | JSON a  
  | TOML a

data ContentSourceType
  = EmbeddedMarkdown T.Text
  | FileRef FilePath
  | EmbeddedPlaintext T.Text
  | SameName FilePath

data ContentType
  = PlainType
  | MarkdownType
  | HTMLType
  -- | Jupyter

loadInsertableDocument :: FilePath -> ByteString -> IO DbDocument
loadInsertableDocument = undefined

readDocument :: FilePath -> BS.ByteString -> EncodedDocument a
readDocument path content = undefined

extractContentSourceType :: FilePath -> EncodedDocument a -> ContentSourceType
extractContentSourceType path document = undefined

loadContentSource :: ContentSourceType -> (ContentType, IO BS.ByteString)
loadContentSource source = undefined

transformContent :: ContentType -> BS.ByteString -> T.Text
transformContent contentType raw = undefined

createInsertableDocument :: T.Text -> EncodedDocument a -> DbDocument
createInsertableDocument contentHtml document = undefined