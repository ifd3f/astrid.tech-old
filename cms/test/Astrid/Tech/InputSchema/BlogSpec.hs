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

spec :: Spec
spec = do
  describe "getProject" $ do
    it "returns blog for valid blog" $ do
      blog <- liftIO $ readBlogPost 32 "resources/test/blog-posts/site-release"

      slug blog
        `shouldBe` DatedSlug
          { year = 2020,
            month = 07,
            day = 10,
            ordinal = 32,
            shortName = "site-release"
          }

      let meta' = meta blog
      title meta' `shouldBe` "Finally live!"