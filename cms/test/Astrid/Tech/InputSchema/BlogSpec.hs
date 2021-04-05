module Astrid.Tech.InputSchema.BlogSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Blog
import Astrid.Tech.Slug
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck

expectedSlug =
  DatedSlug
    { year = 2020,
      month = 07,
      day = 10,
      ordinal = 32,
      shortName = "site-release"
    }

spec :: Spec
spec = do
  describe "readBlog" $ do
    it "returns valid blog for index-style" $ do
      blog <- liftIO $ readBlogPost 32 "resources/test/blog-posts/site-release"

      slug blog `shouldBe` expectedSlug

      let meta' = meta blog
      title meta' `shouldBe` "Finally live!"

    it "returns valid blog for bare file" $ do
      blog <- liftIO $ readBlogPost 32 "resources/test/blog-posts/site-release.md"

      slug blog `shouldBe` expectedSlug

      let meta' = meta blog
      title meta' `shouldBe` "Finally live!"