module LATG.Importer.InsertSchema where 

import Opaleye
    ( SqlArray, SqlInt4, SqlText, SqlTimestamp, Field, SqlUuid )
import qualified LATG.Importer.FileSchema as FSch
import qualified Data.Text as T

data DbDocument = DbDocument
  { uuid :: (Field SqlUuid)
  , createdDate :: (Field SqlTimestamp) 
  , publishedDate :: (Field SqlTimestamp) 
  , updatedDate :: (Field SqlTimestamp) 
  , canonicalUrl :: (Field SqlText)
  , docType :: (Field SqlText)
  , content :: (Field SqlText)
  }
  
data DbEntry = DbEntry
  { year :: (Field SqlInt4)
  , month :: (Field SqlInt4)
  , day :: (Field SqlInt4)
  , ordinal :: (Field SqlInt4)
  , shortName :: (Field SqlText)
  , name :: (Field SqlText)
  , summary :: (Field SqlText)
  , location :: (Field SqlText)
  , photos :: (Field (SqlArray SqlText))
  , replyTo :: (Field (SqlArray SqlText))
  , repostOf :: (Field SqlText)
  , rsvpTo :: (Field SqlText)
  , rsvpValue :: (Field SqlText)
  }
  
entryToInsertable :: FSch.Document FSch.Entry -> T.Text -> DbEntry
entryToInsertable doc@(slug) content =
  DbEntry {
    year = FSch.year slug,
    month = FSch.month slug,
    day = FSch.month slug,
    ordinal = FSch.month slug,
    shortName = FSch.month slug,
  }