module Astrid.Tech.InputSchema.Page
  ( Page (..),
    RawPage (..),
    PageFormat (..),
    PageException (..),
    findAndParseIndex,
    detectFormatFromExtension,
    parseRawPage,
    findIndex,
  )
where

import Control.Exception (IOException)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Frontmatter (IResult (Done, Fail, Partial), parseYamlFrontmatter)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import qualified System.Directory.Tree as DT
import System.FilePath (takeBaseName, takeFileName, (</>))

data PageFormat = MarkdownFormat | JupyterFormat deriving (Show, Eq)

-- | Returns a 'PageFormat' based on the extension, or the extension if error.
detectFormatFromExtension :: String -> Maybe PageFormat
detectFormatFromExtension ext = case ext of
  ".md" -> Just MarkdownFormat
  ".ipynb" -> Just JupyterFormat
  _ -> Nothing

data Page = Page
  { content :: Text,
    format :: PageFormat,
    directory :: String
  }
  deriving (Show, Eq)

data PageException
  = UnsupportedFormat String
  | NoIndex
  | MultipleIndex
  | TreeError FileName IOException
  | ParseYamlFail
  | UnexpectedEOF
  deriving (Show, Eq)

parseMarkdownPage :: FromJSON a => FilePath -> ByteString -> Either PageException (a, Page)
parseMarkdownPage withDirectory document =
  case parseYamlFrontmatter document of
    Done rest front ->
      let page =
            Page
              { content = decodeUtf8 rest,
                format = MarkdownFormat,
                directory = withDirectory
              }
       in Right (front, page)
    Partial _ -> Left UnexpectedEOF
    Fail {} -> Left ParseYamlFail

data RawPage = RawPage
  { name :: String,
    assetRoot :: FilePath,
    file :: FilePath,
    contents :: ByteString
  }

parseRawPage :: FromJSON a => RawPage -> Either PageException (a, Page)
parseRawPage rp =
  let ext = file rp
   in case detectFormatFromExtension ext of
        Just MarkdownFormat -> parseMarkdownPage (assetRoot rp) (contents rp)
        Just JupyterFormat -> error "Not yet implemented"
        Nothing -> Left $ UnsupportedFormat ext

-- | Finds the file representing this path. If this is a directory, its
-- only child with basename index represents this path.
findIndex :: DirTree ByteString -> Either PageException RawPage
findIndex path = case path of
  Failed fileName err -> Left $ TreeError fileName err
  File fileName fileContent ->
    -- If it is a file, return that file
    Right $
      RawPage
        { name = takeBaseName $ takeFileName fileName,
          assetRoot = ".",
          contents = fileContent,
          file = fileName
        }
  Dir dirName children ->
    -- Of the files with basename "index" ...
    case filter (\node -> takeBaseName (DT.name node) == "index") children of
      [] -> Left NoIndex -- If there are none, error
      [File fileName fileContent] ->
        -- If there is exactly one, return
        Right $
          RawPage
            { name = fileName,
              assetRoot = dirName,
              file = dirName </> fileName,
              contents = fileContent
            }
      _ -> Left MultipleIndex -- If there are many, error

-- case findIndex tree of
--   Left err -> Left $ MissingIndex err
--   Right rawPage -> case parseRawPage rawPage of
--     Left err -> Left $ ParseIndex err
--     Right (meta, page) -> Right (rawPage, meta, page)
findAndParseIndex :: FromJSON a => DirTree ByteString -> Either PageException (RawPage, a, Page)
findAndParseIndex tree = do
  rawPage <- findIndex tree
  (meta, page) <- parseRawPage rawPage
  return (rawPage, meta, page)