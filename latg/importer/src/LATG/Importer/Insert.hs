module LATG.Importer.Insert where

import qualified Data.Text as T
import Data.UUID
import qualified LATG.DB.Schema as DB
import qualified LATG.Importer.FileSchema as FSch

docToInsertable :: UUID -> FSch.GenericDocument -> T.Text -> a
docToInsertable uuid doc content = undefined