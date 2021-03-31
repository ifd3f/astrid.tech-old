module Astrid.Tech.InputSchema.Project
  ( Project (..),
    ProjectStatus (..),
    ProjectMeta (..),
    ProjectParseError (..),
    findIndex,
    getProject,
  )
where

import Astrid.Tech.InputSchema.Page (Page, PageParseError, PageParseResult, detectFormatFromExtension, parsePage)
import Control.Exception (Exception, IOException, handle, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Value (Null, String))
import qualified Data.ByteString as ByteString
import Data.Frontmatter (IResult (Done), parseYamlFrontmatter)
import Data.Time (Day)
import GHC.Generics (Generic)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, takeFileName)
import System.FilePath.Posix ((</>))

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
    slug :: String,
    assetRoot :: String,
    rootPage :: Page,
    children :: [Page]
  }

data ProjectParseError
  = NoIndex
  | MultipleIndex
  | FileReadError
  | UnsupportedIndexFormat
  | MetaParseFailure PageParseError
  deriving (Show, Eq)

instance Exception ProjectParseError

findIndex :: FilePath -> IO FilePath
findIndex directory = do
  paths <- listDirectory directory
  ( case filter (\path -> takeBaseName path == "index") paths of
      [index] -> return index
      [] -> throw NoIndex
      _ -> throw MultipleIndex
    )

getProject :: FilePath -> IO Project
getProject directory = do
  indexFileName <- findIndex directory
  contents <- ByteString.readFile (directory </> indexFileName)
  case parsePage directory indexFileName contents of
    Right (meta, page) ->
      return
        Project
          { meta = meta,
            slug = takeFileName directory,
            assetRoot = directory,
            rootPage = page,
            children = []
          }
    Left err -> throw $ MetaParseFailure err