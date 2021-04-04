module Astrid.Tech.InputSchema.Project
  ( Project (..),
    ProjectStatus (..),
    ProjectMeta (..),
    ProjectParseError (..),
    ProjectDirectoryScanException (..),
    ProjectSlug,
    getProject,
    scanProjectDir,
  )
where

import Astrid.Tech.InputSchema.Page (Page, PageParseError, PageParseResult, detectFormatFromExtension, findIndex, parsePage)
import Astrid.Tech.Slug (ProjectSlug)
import Control.Concurrent.ParallelIO (parallelE, parallel_)
import Control.Exception (Exception, IOException, handle, throw)
import Control.Exception.Base (SomeException (SomeException))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Value (Null, String))
import qualified Data.ByteString as ByteString
import Data.Either (partitionEithers)
import Data.Frontmatter (IResult (Done), parseYamlFrontmatter)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (Day)
import GHC.Generics (Generic)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (</>))

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
    startDate :: Day,
    endDate :: Maybe Day,
    url :: Maybe String,
    source :: [String],
    tags :: [String],
    thumbnail :: Maybe String,
    description :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON ProjectMeta

data Project = Project
  { meta :: ProjectMeta,
    slug :: ProjectSlug,
    assetRoot :: FilePath,
    rootPage :: Page,
    children :: [Page]
  }

data ProjectParseError
  = FileReadError
  | UnsupportedIndexFormat
  | MetaParseFailure PageParseError
  deriving (Show, Eq)

instance Exception ProjectParseError

getProject :: FilePath -> IO Project
getProject directory = do
  (indexPath, name) <- findIndex directory
  contents <- ByteString.readFile indexPath
  case parsePage directory indexPath contents of
    Right (meta, page) ->
      return
        Project
          { meta = meta,
            slug = name,
            assetRoot = directory,
            rootPage = page,
            children = []
          }
    Left err -> throw $ MetaParseFailure err

newtype ProjectDirectoryScanException
  = FailedProjects [SomeException]
  deriving (Show)

instance Exception ProjectDirectoryScanException

scanProjectDir :: FilePath -> IO (Map ProjectSlug Project)
scanProjectDir projectsRoot = do
  children <- listDirectory projectsRoot
  let tasks = map (getProject . (projectsRoot </>)) children
  projects <- parallelE tasks

  results <- case partitionEithers projects of
    ([], success) -> return success
    (failed, _) -> throw $ FailedProjects failed

  let projectsMap = Map.fromList (map (\project -> (slug project, project)) results)
  return projectsMap
