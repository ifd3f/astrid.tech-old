module Astrid.Tech.InputSchema.Project
  ( Project (..),
    ProjectStatus (..),
    ProjectMeta (..),
    ProjectParseException (..),
    ProjectDirectoryScanException (..),
    ProjectSlug,
    readProject,
    readProjectDir,
  )
where

import Astrid.Tech.InputSchema.Page (FindIndexException, Page, PageParseException, RawPage, detectFormatFromExtension, findIndex, parseRawPage)
import qualified Astrid.Tech.InputSchema.Page as P
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
import System.Directory.Tree (DirTree (Dir), FileName)
import qualified System.Directory.Tree as DT
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
    shortName :: ProjectSlug,
    assetRoot :: FilePath,
    rootPage :: Page,
    children :: [Page]
  }

data ProjectParseException
  = PageParseFailure FileName PageParseException
  | NoProjectIndex FileName FindIndexException
  deriving (Show, Eq)

readProject :: DirTree ByteString.ByteString -> Either ProjectParseException Project
readProject tree = case findIndex tree of
  Left err -> Left $ NoProjectIndex (DT.name tree) err
  Right rawPage -> case parseRawPage rawPage of
    Left err -> Left $ PageParseFailure (DT.name tree) err
    Right (meta, page) ->
      Right $
        Project
          { meta = meta,
            shortName = P.name rawPage,
            assetRoot = P.assetRoot rawPage,
            rootPage = page,
            children = []
          }

newtype ProjectDirectoryScanException
  = FailedProjects [ProjectParseException]
  deriving (Show)

readProjectDir :: DirTree ByteString.ByteString -> Either ProjectDirectoryScanException (Map ProjectSlug Project)
readProjectDir projectsRoot = case projectsRoot of
  Dir name children ->
    let projects = map readProject children
     in case partitionEithers projects of
          ([], successful) -> Right $ Map.fromList (map (\project -> (shortName project, project)) successful)
          (failed, _) -> Left $ FailedProjects failed
