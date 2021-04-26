module Astrid.Tech.InputSchema.Article
  ( Article (..),
    ArticleMeta (..),
    readBlogPost,
    readBlogDir,
  )
where

import qualified Astrid.Tech.InputSchema.Page as P
import Astrid.Tech.Slug (DatedSlug (DatedSlug, day, month, ordinal, shortName, year))
import Control.Exception (Exception)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as ByteString
import Data.Either.Combinators
import Data.Time
  ( Day,
    UTCTime (utctDay),
    ZonedTime,
    toGregorian,
    zonedTimeToUTC,
  )
import GHC.Generics (Generic)
import System.Directory.Tree (DirTree (Dir), FileName)
import qualified System.Directory.Tree as DT
import Text.Read (readMaybe)

data ArticleMeta = ArticleMeta
  { title :: String,
    description :: String,
    date :: ZonedTime,
    thumbnail :: Maybe String,
    tags :: [String]
  }
  deriving (Generic)

instance FromJSON ArticleMeta

generateSlug :: Int -> String -> ArticleMeta -> DatedSlug
generateSlug withOrdinal withShortName withMeta =
  let utcTime = zonedTimeToUTC $ date withMeta
      (y, m, d) = toGregorian $ utctDay utcTime
   in DatedSlug
        { year = y,
          month = m,
          day = d,
          ordinal = withOrdinal,
          shortName = withShortName
        }

data Article = Article
  { slug :: DatedSlug,
    meta :: ArticleMeta,
    page :: P.Page
  }

data BlogParseException
  = UnexpectedFileStructure FileName P.PageException
  | InconsistentDayException FileName ((Integer, Int, Int), Day)
  deriving (Show, Eq)

instance Exception BlogParseException

readBlogPost :: Int -> DirTree ByteString.ByteString -> Either BlogParseException Article
readBlogPost withOrdinal tree =
  mapLeft
    (UnexpectedFileStructure fileName)
    ( do
        (rawPage, parsedMeta, parsedPage) <- P.findAndParseIndex tree
        let newSlug = generateSlug withOrdinal (P.name rawPage) parsedMeta
        Right $
          Article
            { meta = parsedMeta,
              page = parsedPage,
              slug = newSlug
            }
    )
  where
    fileName = DT.name tree

data ScanPostsException
  = MultiplePosts [FilePath]
  | BadDir FilePath
  | BadPost BlogParseException
  deriving (Show, Eq)

instance Exception ScanPostsException

readBlogDir :: DirTree ByteString.ByteString -> [Either ScanPostsException Article]
readBlogDir tree =
  let validateIsDir (Dir name contents) = Right (name, contents)
      validateIsDir node = Left $ BadDir $ DT.name node

      validateIsType name =
        case readMaybe name of
          Just x -> Right x
          Nothing -> Left $ BadDir name

      validateIsLength expectedLength name =
        if length name == expectedLength
          then Right ()
          else Left $ BadDir name

      liftErrorToList (Left x) = [Left x]
      liftErrorToList (Right xs) = xs

      validateYear yDir = liftErrorToList $ do
        (name, contents) <- validateIsDir yDir
        _ :: Integer <- validateIsType name
        return $ concatMap validateMonth contents

      validateMonth mDir = liftErrorToList $ do
        (name, contents) <- validateIsDir mDir
        _ :: Int <- validateIsType name
        _ <- validateIsLength 2 name
        return $ concatMap validateDay contents

      validateDay dDir = liftErrorToList $ do
        (name, contents) <- validateIsDir dDir
        _ :: Integer <- validateIsType name
        _ <- validateIsLength 2 name
        case contents of
          [] -> Left $ BadDir $ DT.name dDir
          [single] -> Right [mapLeft BadPost $ readBlogPost 0 single]
          many -> Right $ concatMap validateOrdinal many

      validateOrdinal oDir = liftErrorToList $ do
        (name, contents) <- validateIsDir oDir
        _ :: Integer <- validateIsType name
        case contents of
          [] -> badDir
          [single] -> return [mapLeft BadPost $ readBlogPost 0 single]
          _ -> badDir
        where
          badDir = Left $ BadDir $ DT.name oDir
   in liftErrorToList $ do
        (_, contents) <- validateIsDir tree
        return $ concatMap validateYear contents
