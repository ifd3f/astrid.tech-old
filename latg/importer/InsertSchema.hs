module LATG.Importer.InsertSchema where 

import LATG.DB.Schema
import Opaleye

data DbEntry = DbEntry
  { uuid :: (Field SqlUuid)
  , year :: (Field SqlInt4)
  , month :: (Field SqlInt4)
  , day :: (Field SqlInt4)
  , ordinal :: (Field SqlInt4)
  , slug :: (Field SqlText)
  , createdDate :: (Field SqlTimestamp) 
  , publishedDate :: (Field SqlTimestamp) 
  , updatedDate :: (Field SqlTimestamp) 
  , name :: (Field SqlText)
  , summary :: (Field SqlText)
  , location :: (Field SqlText)
  , photos :: (Field (SqlArray SqlText))
  , replyTo :: (Field (SqlArray SqlText))
  , repostOf :: (Field SqlText)
  , rsvp :: (Field SqlText)
  , content :: (Field SqlText)
  , colophon :: (Field SqlText)
  , pageSrcUrl :: (Field SqlText)
  }
  