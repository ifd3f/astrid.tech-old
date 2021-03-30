module Astrid.Tech.InputSchema.Page
  ( Page,
    PageFormat,
    detectFormatFromExtension,
  )
where

data PageFormat = MarkdownFormat | JupyterFormat

detectFormatFromExtension :: String -> Maybe PageFormat
detectFormatFromExtension ext = case ext of
  "md" -> Just MarkdownFormat
  "ipynb" -> Just JupyterFormat
  _ -> Nothing

data Page = Page
  { content :: String,
    format :: PageFormat,
    directory :: String
  }
