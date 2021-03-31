module Astrid.Tech.InputSchema.Project
  ( Project,
    ProjectStatus,
    ProjectMeta,
    ProjectParseError (NoIndex, MultipleIndex),
    findIndex,
  )
where

import Astrid.Tech.InputSchema.Page (Page)
import Control.Exception (IOException, handle, throw)
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON (parseJSON), Value (Null, String))
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import GHC.IO.Exception
import System.Directory (listDirectory)
import System.FilePath (takeBaseName)
import System.FilePath.Posix

data ProjectStatus
  = NoStatus
  | Early
  | WIP
  | Complete
  | Scrapped
  deriving (Show, Generic, Eq)

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

data ProjectParseError
  = NoIndex
  | MultipleIndex
  deriving (Show, Eq)

findIndex :: FilePath -> IO (Either ProjectParseError FilePath)
findIndex directory = do
  paths <- listDirectory directory
  return
    ( case filter (\path -> takeBaseName path == "index") paths of
        [index] -> Right index
        [] -> Left NoIndex
        _ -> Left MultipleIndex
    )
