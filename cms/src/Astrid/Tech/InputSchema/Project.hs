module Astrid.Tech.InputSchema.Project
  ( Project (..),
    ProjectStatus (..),
    ProjectMeta (..),
    ProjectSlug,
    readProject,
    readProjectDir,
  )
where

import qualified Astrid.Tech.InputSchema.Page as P
import Astrid.Tech.Slug (ProjectSlug)
import Control.Exception (Exception, IOException)
import Data.Aeson (FromJSON (parseJSON), Value (Null, String))
import qualified Data.ByteString as ByteString
import Data.Time (Day)
import GHC.Generics (Generic)
import System.Directory.Tree (DirTree (Dir, Failed, File), FileName)

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
    rootPage :: P.Page,
    children :: [P.Page]
  }
  deriving (Show, Eq)

readProject :: DirTree ByteString.ByteString -> Either P.PageException Project
readProject tree = do
  (rawPage, parsedMeta, page) <- P.findAndParseIndex tree
  Right $
    Project
      { meta = parsedMeta,
        shortName = P.name rawPage,
        assetRoot = P.assetRoot rawPage,
        rootPage = page,
        children = []
      }

data ProjectDirException
  = NotADirectory FileName
  | ReadError FileName IOException
  deriving (Show, Eq)

instance Exception ProjectDirException

readProjectDir :: DirTree ByteString.ByteString -> Either ProjectDirException [Either P.PageException Project]
readProjectDir projectsRoot = case projectsRoot of
  Dir _ dirChildren -> Right $ map readProject dirChildren
  File name _ -> Left $ NotADirectory name
  Failed name err -> Left $ ReadError name err