module Astrid.Tech.InputSchema.BlogSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Blog
import Astrid.Tech.InputSchema.TestUtil (rootResourcesPath)
import Astrid.Tech.InputSchema.Util (readDirectoryBS)
import Astrid.Tech.Slug ( DatedSlug(..) )
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

      let blog =
            ( case readBlogPost 32 (DT.dirTree tree) of
                Right b -> b
                Left err -> error $ show err
            )

      slug blog `shouldBe` expectedSlug
      let meta' = meta blog
      title meta' `shouldBe` "Finally live!"

    it "returns valid blog for bare file" $ do
      tree <- readDirectoryBS $ resources </> "site-release.md"

      let blog =
            ( case readBlogPost 32 (DT.dirTree tree) of
                Right b -> b
                Left err -> error $ show err
            )

      slug blog `shouldBe` expectedSlug
      let meta' = meta blog
      title meta' `shouldBe` "Finally live!"