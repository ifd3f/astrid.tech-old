module Astrid.Tech.InputSchema.Content
  ( InputContent,
  )
where

import qualified Astrid.Tech.InputSchema.Blog as Blog
import qualified Astrid.Tech.InputSchema.Project as Project
import qualified Astrid.Tech.InputSchema.Tag as Tag
import Control.Concurrent.ParallelIO (parallelE)
import Control.Concurrent.ParallelIO.Global (parallel_)
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath ((</>))

data InputContent = InputContent
  { blogs :: [Blog.BlogPost],
    projects :: Map Project.ProjectSlug Project.Project,
    tagOverrides :: [Tag.TagOverrideSheet]
  }

scanContentDir contentRootDir =
  let readProjects = Project.readProjectDir $ contentRootDir </> "projects"
   in do
        projects <- readProjects
        return
          InputContent
            { blogs = [],
              projects = projects,
              tagOverrides = []
            }