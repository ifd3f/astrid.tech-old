module Astrid.Tech.InputSchema.Project () where

import Data.Aeson
import Data.Time.Clock
import GHC.Generics

data ProjectStatus = None | Early | WIP | Complete | Scrapped deriving (Show, Generic)

instance FromJSON ProjectStatus where
  parseJSON value =
    case value of
      String "early" -> return Early
      String "wip" -> return WIP
      String "complete" -> return Complete
      String "scrapped" -> return Scrapped
      Null -> return None
      _ -> invalid
    where
      invalid = fail "string is not one of known enum values"

data ProjectMeta = ProjectMeta
  { title :: String,
    assetRoot :: String,
    status :: ProjectStatus,
    startDate :: UTCTime,
    endDate :: Maybe UTCTime,
    slug :: String,
    url :: String,
    source :: [String],
    tags :: [String],
    thumbnail :: String,
    description :: Maybe String
  }
  deriving (Generic)

instance FromJSON ProjectMeta