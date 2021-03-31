module Astrid.Tech.InputSchema.Page
  ( Page,
    PageFormat (MarkdownFormat, JupyterFormat),
    detectFormatFromExtension,
    parsePage,
  )
where

import Data.Aeson (FromJSON)
import Data.ByteString
import Data.Frontmatter (IResult (Done), parseYamlFrontmatter)

data PageFormat = MarkdownFormat | JupyterFormat deriving (Show, Eq)

detectFormatFromExtension :: String -> Maybe PageFormat
detectFormatFromExtension ext = case ext of
  "md" -> Just MarkdownFormat
  "ipynb" -> Just JupyterFormat
  _ -> Nothing

data Page = Page
  { content :: ByteString,
    format :: PageFormat,
    directory :: String
  }
  deriving (Show, Eq)

parsePage :: FromJSON a => FilePath -> FilePath -> ByteString -> Maybe (a, Page)
parsePage directory name contents =
  case detectFormatFromExtension name of
    Just MarkdownFormat ->
      case parseYamlFrontmatter contents of
        Done rest front ->
          let page =
                Page
                  { content = rest,
                    format = MarkdownFormat,
                    directory = directory
                  }
           in Just (front, page)
        _ -> Nothing
    Just JupyterFormat -> error "Not yet implemented"
    Nothing -> Nothing