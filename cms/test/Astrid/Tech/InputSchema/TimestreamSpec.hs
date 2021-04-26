module Astrid.Tech.InputSchema.TimestreamSpec (spec) where

import Astrid.Tech.InputSchema.Timestream
import Astrid.Tech.InputSchema.TestUtil (rootResourcesPath)
import Astrid.Tech.InputSchema.Util (readDirectoryBS)
import Astrid.Tech.Slug (DatedSlug (..))
import Control.Exception (throwIO)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import Test.Hspec

expectedSlug :: DatedSlug
expectedSlug =
  DatedSlug
    { year = 2020,
      month = 07,
      day = 10,
      ordinal = 32,
      shortName = "site-release"
    }

resources :: FilePath
resources = rootResourcesPath </> "blog-posts"

spec :: Spec
spec = do
  describe "readBlog" $ do
    it "returns valid blog for index-style" $ do
      tree <- readDirectoryBS $ resources </> "site-release"
      
      readTimestream (id) tree