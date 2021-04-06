module Astrid.Tech.InputSchema.Page
  ( Page (..),
    RawPage (..),
    PageFormat (..),
    PageParseException (..),
    FindIndexException (..),
    detectFormatFromExtension,
    parseRawPage,
    findIndex,
  )
where

import Control.Exception (Exception, IOException, throw)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Frontmatter (IResult (Done, Fail, Partial), parseYamlFrontmatter)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import qualified System.Directory.Tree as DT
import System.FilePath (takeBaseName, takeExtension, takeFileName, (</>))

data PageFormat = MarkdownFormat | JupyterFormat deriving (Show, Eq)

-- | Returns a 'PageFormat' based on the extension, or the extension if error.
detectFormatFromExtension :: String -> Either String PageFormat
detectFormatFromExtension ext = case ext of
  ".md" -> Right MarkdownFormat
  ".ipynb" -> Right JupyterFormat
  _ -> Left ext

data Page = Page
  { content :: Text,
    format :: PageFormat,
    directory :: String
  }
  deriving (Show, Eq)

data PageParseException
  = UnsupportedFormat String
  | ParseYamlFail String
  | UnexpectedEOF
  deriving (Show, Eq)

parseMarkdownPage :: FromJSON a => FilePath -> ByteString -> Either PageParseException (a, Page)
parseMarkdownPage directory contents =
  case parseYamlFrontmatter contents of
    Done rest front ->
      let page =
            Page
              { content = decodeUtf8 rest,
                format = MarkdownFormat,
                directory = directory
              }
       in Right (front, page)

data RawPage = RawPage
  { name :: String,
    assetRoot :: FilePath,
    file :: FilePath,
    contents :: ByteString
  }

withParentDir :: FilePath -> RawPage -> RawPage
withParentDir parent rp =
  rp
    { assetRoot = parent </> assetRoot rp,
      file = parent </> file rp
    }

parseRawPage :: FromJSON a => RawPage -> Either PageParseException (a, Page)
parseRawPage rp = case detectFormatFromExtension $ file rp of
  Right MarkdownFormat -> parseMarkdownPage (assetRoot rp) (contents rp)
  Right JupyterFormat -> error "Not yet implemented"
  Left ext -> Left $ UnsupportedFormat ext

data FindIndexException = NoIndex | MultipleIndex | TreeError FileName IOException
  deriving (Show, Eq)

-- | Finds the file representing this path. If this is a directory, its
-- only child with basename index represents this path.
findIndex :: DirTree ByteString -> Either FindIndexException RawPage
findIndex path = case path of
  File fileName content ->
    -- If it is a file, return that file
    Right $
      RawPage
        { name = takeBaseName $ takeFileName fileName,
          assetRoot = ".",
          contents = content,
          file = fileName
        }
  Dir dirName contents ->
    -- Of the files with basename "index" ...
    case filter (\node -> takeBaseName (DT.name node) == "index") contents of
      [] -> Left NoIndex -- If there are none, error
      [File fileName content] ->
        -- If there is exactly one, return
        Right $
          RawPage
            { name = fileName,
              assetRoot = dirName,
              file = dirName </> fileName,
              contents = content
            }
      _ -> Left MultipleIndex -- If there are many, error
  Failed name err -> Left $ TreeError name err