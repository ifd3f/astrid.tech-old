module Astrid.Tech.InputSchema.Blog
  ( BlogPost (..),
    BlogPostMeta (..),
    BlogPostReadException (..),
    readBlogPost,
  )
where

import Astrid.Tech.InputSchema.Page (Page, PageParseError, findIndex, parsePage)
import Astrid.Tech.Slug (DatedSlug (DatedSlug, day, month, ordinal, shortName, year))
import Control.Concurrent.ParallelIO (parallel)
import Control.Exception (SomeException, throw)
import Control.Exception.Base (Exception)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as ByteString
import Data.Time (Day, UTCTime (utctDay), ZonedTime (zonedTimeToLocalTime), toGregorian, zonedTimeToUTC)
import GHC.Generics (Generic)
import System.Directory (listDirectory)
import System.Directory.Tree

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
  = BlogPostMetaParseException PageParseError
  | InconsistentDayException ((Integer, Int, Int), Day)
  deriving (Show)

instance Exception BlogPostReadException

parseBlogPost :: Monad m => FilePath -> FilePath -> Int -> [Char] -> ByteString.ByteString -> m BlogPost
parseBlogPost directory file ordinal name contents = case parsePage directory file contents of
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
