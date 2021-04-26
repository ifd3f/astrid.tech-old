module Astrid.Tech.InputSchema.Note where

import Astrid.Tech.Slug (DatedSlug (DatedSlug, day, month, ordinal, shortName, year))
import Data.Time
  ( ZonedTime,
  )
import GHC.Generics (Generic)

data NoteMeta = NoteMeta
  { date :: ZonedTime,
    images :: [String]
  }
  deriving (Generic)

data Note = Note
  { slug :: DatedSlug,
    meta :: NoteMeta,
    contentMarkdown :: String
  }
