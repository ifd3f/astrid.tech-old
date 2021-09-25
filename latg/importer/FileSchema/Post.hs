import GHC.Generics

import Data.UUID
import Data.Text

data Slug = Slug
  { name :: Maybe Text
  , uuid :: UUID
  , slug :: DatedSlug
  , summary :: Maybe Text
  , createdDate :: ZonedTime
  , publishedDate :: ZonedTime
  , updatedDate :: Maybe ZonedTime
  , tags :: [Text]
  }
  deriving (Generic, Show)

