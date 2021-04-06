module Astrid.Tech.InputSchema.Content
  ( InputContent,
  )
where

import qualified Astrid.Tech.InputSchema.Blog as Blog
import qualified Astrid.Tech.InputSchema.Project as Project
import qualified Astrid.Tech.InputSchema.Tag as Tag
import Control.Concurrent.ParallelIO (parallelE)
import Control.Concurrent.ParallelIO.Global (parallel_)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory.Tree (AnchoredDirTree, DirTree)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))

data InputContent = InputContent
  { blogs :: [Blog.BlogPost],
    projects :: Map Project.ProjectSlug Project.Project,
    tagOverrides :: [Tag.TagOverrideSheet]
  }

scanContentDir :: AnchoredDirTree BS.ByteString -> InputContent
scanContentDir dir =
  let blog = do
        x <- DT.dropTo "blog" dir
        y <- Blog.readBlogDir x
        x
   in InputContent
        { blogs = readBlogDir $ DT.dropTo "blog" dir,
          projects = readProjectDir $ DT.dropTo "projects" dir,
          tagOverrides = []
        }