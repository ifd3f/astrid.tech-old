module Astrid.Tech.InputSchema.ProjectSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Project
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Calendar
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "getProject" $ do
    it "returns project for valid project" $ do
      project <- liftIO $ getProject "resources/test/projects/collision-zone"
      slug project `shouldBe` "collision-zone"

      let pMeta = meta project
      showGregorian (startDate pMeta) `shouldBe` "2019-06-01"
      endDate pMeta `shouldBe` Nothing
      status pMeta `shouldBe` Complete

    it "throws MetaParseFailure for metaless file" $ do
      getProject "resources/test/projects/metaless"
        `shouldThrow` (\case MetaParseFailure _ -> True)

    it "throws MetaParseFailure for file with non-conformant meta" $
      getProject "resources/test/projects/bad-meta"
        `shouldThrow` (\case MetaParseFailure _ -> True)
