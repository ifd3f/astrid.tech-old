module Astrid.Tech.InputSchema.Page
  ( Page (..),
    PageFormat (..),
    PageParseError (..),
    PageParseResult,
    detectFormatFromExtension,
    parsePage,
  )
where

import Control.Exception (Exception, throw)
import Data.Aeson (FromJSON)
import Data.ByteString
import Data.Frontmatter (IResult (Done, Fail, Partial), parseYamlFrontmatter)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath (takeExtension)

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

data PageParseError
  = UnsupportedFormat String
  | ParseYamlFail String
  | UnexpectedEOF
  deriving (Show, Eq)

type PageParseResult a = Either PageParseError (a, Page)

parsePage :: FromJSON a => FilePath -> FilePath -> ByteString -> PageParseResult a
parsePage directory filename contents =
  case detectFormatFromExtension $ takeExtension filename of
    Right MarkdownFormat ->
      case parseYamlFrontmatter contents of
        Done rest front ->
          let page =
                Page
                  { content = decodeUtf8 rest,
                    format = MarkdownFormat,
                    directory = directory
                  }
           in Right (front, page)
        Fail _ _ desc -> Left $ ParseYamlFail desc
        Partial _ -> Left UnexpectedEOF
    Right JupyterFormat -> error "Not yet implemented"
    Left ext -> Left $ UnsupportedFormat ext