module Astrid.Tech.InputSchema.Project
  ( Project,
    ProjectStatus,
    ProjectMeta,
  )
where

import Astrid.Tech.InputSchema.Page
import Data.Aeson
import Data.Time.Clock
import GHC.Generics

data ProjectStatus = NoStatus | Early | WIP | Complete | Scrapped deriving (Show, Generic)

instance FromJSON ProjectStatus where
  parseJSON value =
    case value of
      String "early" -> return Early
      String "wip" -> return WIP
      String "complete" -> return Complete
      String "scrapped" -> return Scrapped
      Null -> return NoStatus
      _ -> invalid
    where
      invalid = fail "string is not one of known enum values"

data ProjectMeta = ProjectMeta
  { title :: String,
    status :: ProjectStatus,
    startDate :: UTCTime,
    endDate :: Maybe UTCTime,
    url :: Maybe String,
    source :: Maybe [String],
    tags :: [String],
    thumbnail :: Maybe String,
    description :: Maybe String
  }
  deriving (Generic)

instance FromJSON ProjectMeta

data Project = Project
  { meta :: ProjectMeta,
    slug :: String,
    assetRoot :: String,
    rootPage :: Page,
    children :: [Page]
  }

-- parseProject :: FilePath -> IO Project
-- parseProject directory =
