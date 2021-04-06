module Astrid.Tech.InputSchema.Blog
  ( BlogPost (..),
    BlogPostMeta (..),
    BlogPostReadException (..),
    readBlogPost,
  )
where

import Astrid.Tech.InputSchema.Page (Page, PageParseException, findIndex, parseRawPage)
import Astrid.Tech.Slug (DatedSlug (DatedSlug, day, month, ordinal, shortName, year))
import Control.Concurrent.ParallelIO (parallel)
import Control.Exception (SomeException, throw)
import Control.Exception.Base (Exception)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as ByteString
import Data.Maybe (catMaybes)
import Data.Time (Day, UTCTime (utctDay), ZonedTime (zonedTimeToLocalTime), toGregorian, zonedTimeToUTC)
import GHC.Generics (Generic)
import System.Directory.Tree
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
    page :: Page
  }

data BlogPostReadException
  = BlogPostMetaParseException PageParseException
  | InconsistentDayException ((Integer, Int, Int), Day)
  deriving (Show)

instance Exception BlogPostReadException

parseBlogPost :: Monad m => FilePath -> FilePath -> Int -> [Char] -> ByteString.ByteString -> m BlogPost
parseBlogPost directory file ordinal name contents = case parseRawPage directory file contents of
  Right (meta, page) ->
    return $
      let slug = generateSlug ordinal name meta
       in BlogPost
            { meta = meta,
              page = page,
              slug = slug
            }
  Left err -> throw $ BlogPostMetaParseException err

readBlogPost :: Int -> FilePath -> IO BlogPost
readBlogPost ordinal path = do
  (indexPath, name) <- findIndex path
  contents <- ByteString.readFile indexPath
  parseBlogPost path indexPath ordinal name contents

data ScanPostsException
  = MultiplePosts [FilePath]
  | EmptyDay
  deriving (Show)

instance Exception ScanPostsException

newtype InvalidPath = InvalidPath String

parseTree :: String -> DirTree ByteString.ByteString -> [Either FilePath BlogPost]
parseTree anchor tree =
  let yearDir = \case
        Dir yearDirName contents ->
          case readMaybe yearDirName :: Maybe Integer of
            Just y -> concatMap (monthDir y) contents
        other -> [Left $ name other]

      monthDir year = \case
        Dir monthDirName contents ->
          if length monthDirName /= 2
            then [Left $ show year </> monthDirName]
            else case readMaybe monthDirName :: Maybe Int of
              Just m -> concatMap (dayDir year m) contents

      dayDir :: Integer -> Int -> DirTree ByteString.ByteString
      dayDir year month = \case
        Dir dayDirName contents -> case readMaybe dayDirName :: Maybe Int of
          Just d -> case contents of
            [] -> []
            [single] ->
              let dir = show year </> show month </> show d
                  singleName = name single
                  path = dir </> singleName
               in [Right $ parseBlogPost dir single 0 singleName $ contents single]
            ordinals -> concatMap (ordinalDir year month d) contents

      ordinalDir year month day = \case
        Dir dayDirName contents -> case readMaybe dayDirName :: Maybe Int of
          Just d -> case contents of
            [] -> []
            [single] ->
              let dir = show year </> show month </> show d
                  path = dir </> name single
               in [Right $ parseBlogPost path]
            ordinals -> concatMap (ordinalDir year month d) contents
   in year $ contents tree