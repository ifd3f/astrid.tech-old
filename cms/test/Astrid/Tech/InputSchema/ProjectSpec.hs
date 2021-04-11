module Astrid.Tech.InputSchema.ProjectSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Page (PageException (ParseYamlFail))
import Astrid.Tech.InputSchema.Project
import Astrid.Tech.InputSchema.Util (readDirTreeBS)
import Control.Exception (throw)
import Data.Time.Calendar
import Test.Hspec

spec :: Spec
spec = do
  describe "readProject" $ do
    it "returns project for valid project" $ do
      tree <- readDirTreeBS "resources/test/projects/collision-zone"
      project <- either throw pure $ readProject tree
      shortName project `shouldBe` "collision-zone"

      let pMeta = meta project
      showGregorian (startDate pMeta) `shouldBe` "2019-06-01"
      endDate pMeta `shouldBe` Nothing
      status pMeta `shouldBe` Complete

    it "throws MetaParseFailure for metaless file" $ do
      tree <- readDirTreeBS "resources/test/projects/metaless"
      readProject tree `shouldBe` Left ParseYamlFail

    it "throws MetaParseFailure for file with non-conformant meta" $ do
      tree <- readDirTreeBS "resources/test/projects/bad-meta"
      readProject tree `shouldBe` Left ParseYamlFail
