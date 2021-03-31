module Astrid.Tech.InputSchema.Page
  ( Page,
    PageFormat (..),
    detectFormatFromExtension,
    parsePage,
  )
where

import Control.Exception (Exception, throw)
import Data.Aeson (FromJSON)
import Data.ByteString
import Data.Frontmatter (IResult (Done), parseYamlFrontmatter)
import System.FilePath (takeExtension)

data PageFormat = MarkdownFormat | JupyterFormat deriving (Show, Eq)

-- | Returns a 'PageFormat', or the extension if error.
detectFormatFromExtension :: String -> Either String PageFormat
detectFormatFromExtension ext = case ext of
  ".md" -> Right MarkdownFormat
  ".ipynb" -> Right JupyterFormat
  _ -> Left ext

data Page = Page
  { content :: ByteString,
    format :: PageFormat,
    directory :: String
  }
  deriving (Show, Eq)

data PageParseError = UnsupportedFormat String | ParseYamlError deriving (Show, Eq)

instance Exception PageParseError

parsePage :: FromJSON a => FilePath -> FilePath -> ByteString -> (a, Page)
parsePage directory filename contents =
  case detectFormatFromExtension $ takeExtension filename of
    Right MarkdownFormat ->
      case parseYamlFrontmatter contents of
        Done rest front ->
          let page =
                Page
                  { content = rest,
                    format = MarkdownFormat,
                    directory = directory
                  }
           in (front, page)
        _ -> throw ParseYamlError
    Right JupyterFormat -> error "Not yet implemented"
    Left ext -> throw $ UnsupportedFormat ext