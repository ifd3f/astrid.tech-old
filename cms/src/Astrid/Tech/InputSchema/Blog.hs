module Astrid.Tech.InputSchema.Blog
  ( BlogPost (..),
    BlogPostMeta (..),
    readBlogPost,
  )
where

import qualified Astrid.Tech.InputSchema.Page as P
import Astrid.Tech.Slug (DatedSlug (DatedSlug, day, month, ordinal, shortName, year))
import Control.Concurrent.ParallelIO (parallel)
import Control.Exception (SomeException, throw)
import Control.Exception.Base (Exception)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as ByteString
import Data.Maybe (catMaybes)
import Data.Time (Day, UTCTime (utctDay), ZonedTime (zonedTimeToLocalTime), toGregorian, zonedTimeToUTC)
import GHC.Generics (Generic)
import System.Directory.Tree (DirTree (Dir), FileName)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import Text.Read (readMaybe)

data BlogPostMeta = BlogPostMeta
  { title :: String,
    description :: String,
    date :: ZonedTime,
    thumbnail :: Maybe String,
    tags :: [String]
  }
  deriving (Generic)

instance FromJSON BlogPostMeta

generateSlug :: Int -> String -> BlogPostMeta -> DatedSlug
generateSlug ordinal shortName meta =
  let utcTime = zonedTimeToUTC $ date meta
      day = utctDay utcTime
      (y, m, d) = toGregorian day
   in DatedSlug
        { year = y,
          month = m,
          day = d,
          ordinal = ordinal,
          shortName = shortName
        }

data BlogPost = BlogPost
  { slug :: DatedSlug,
    meta :: BlogPostMeta,
    page :: P.Page
  }

data BlogParseException
  = UnexpectedFileStructure FileName P.FindAndParseIndexException
  | InconsistentDayException FileName ((Integer, Int, Int), Day)
  deriving (Show, Eq)

readBlogPost :: Int -> DirTree ByteString.ByteString -> Either BlogParseException BlogPost
readBlogPost ordinal tree = case P.findAndParseIndex tree of
  Left err -> Left $ UnexpectedFileStructure fileName err
  Right (rawPage, meta, page) ->
    Right $
      let slug = generateSlug ordinal (P.name rawPage) meta
       in BlogPost
            { meta = meta,
              page = page,
              slug = slug
            }
  where
    fileName = DT.name tree

data ScanPostsException
  = MultiplePosts [FilePath]
  | BadYear String
  | BadMonth String
  | BadDay String
  | BadPost BlogParseException
  deriving (Show, Eq)

newtype InvalidPath = InvalidPath String

type BlogTreeResult = [Either ScanPostsException BlogPost]

parseTree :: String -> DirTree ByteString.ByteString -> BlogTreeResult
parseTree anchor tree =
  let yearDir = \case
        Dir yearDirName contents ->
          case readMaybe yearDirName :: Maybe Integer of
            Just y -> concatMap (monthDir y) contents
        other -> [Left $ BadYear $ DT.name other]

      monthDir :: Integer -> DirTree ByteString.ByteString -> BlogTreeResult
      monthDir year = \case
        Dir monthDirName contents ->
          if length monthDirName /= 2
            then [Left $ BadMonth monthDirName]
            else case readMaybe monthDirName :: Maybe Int of
              Just m -> concatMap (dayDir year m) contents

      readBlogE post = case readBlogPost 0 post of
        Left err -> Left $ BadPost err
        Right post -> Right post

      dayDir :: Integer -> Int -> DirTree ByteString.ByteString -> BlogTreeResult
      dayDir year month = \case
        Dir dayDirName contents -> case readMaybe dayDirName :: Maybe Int of
          Just d -> case contents of
            [] -> []
            [single] -> [readBlogE single] -- If there is a single child, then this is the only post today
            ordinals ->
              -- If there is more than one child, then attempt to parse with ordinals
              concatMap (ordinalDir year month d) contents

      ordinalDir year month day = \case
        Dir dayDirName contents -> case readMaybe dayDirName :: Maybe Int of
          Just d -> case contents of
            [] -> []
            [single] -> [readBlogE single]
            ordinals -> concatMap (ordinalDir year month d) contents
   in concatMap yearDir (DT.contents tree)