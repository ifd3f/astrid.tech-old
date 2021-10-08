module LATG.Importer.Insert where

import qualified Data.Text as T
import Data.UUID
import qualified LATG.DB.Schema as DB
import qualified LATG.Importer.FileSchema as FSch
import Opaleye (toFields)

docToInsertable :: UUID -> FSch.GenericDocument -> T.Text -> DB.DocumentW
docToInsertable uuid doc content =
  let tDoc =
        DB.Document
          { DB.docId = (),
            DB.uuid = uuid,
            DB.createdDate = FSch.createdDate doc,
            DB.publishedDate = FSch.publishedDate doc,
            DB.updatedDate = FSch.updatedDate doc,
            DB.canonicalUrl = "https://example.com",
            DB.docType = "h-entry",
            DB.content = Just content,
            DB.colophon = FSch.colophon doc,
            DB.pageSrcUrl = Just "https://example.com"
          }
   in toFields tDoc
