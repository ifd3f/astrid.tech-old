module Astrid.Tech.InputSchema.Content () where

import qualified Astrid.Tech.InputSchema.Tag as Tag
import qualified Astrid.Tech.InputSchema.Blog as Blog
import qualified Astrid.Tech.InputSchema.Project as Project

data Content = Content
  { blogs :: [Blog.BlogPost],
    projects :: [Project.Project],
    Tag :: [TagOverrideSheet]
  }
