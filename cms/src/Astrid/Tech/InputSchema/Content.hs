module Astrid.Tech.InputSchema.Content
  ( InputContent,
  )
where

import qualified Astrid.Tech.InputSchema.Blog as Blog
import qualified Astrid.Tech.InputSchema.Project as Project
import qualified Astrid.Tech.InputSchema.Tag as Tag

data InputContent = InputContent
  { blogs :: [Blog.BlogPost],
    projects :: [Project.Project],
    tagOverrides :: [Tag.TagOverrideSheet]
  }

-- getInputContent :: FilePath -> IO Project
