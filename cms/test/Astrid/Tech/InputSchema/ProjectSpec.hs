module Astrid.Tech.InputSchema.ProjectSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Project
import Control.Monad.IO.Class (MonadIO (liftIO))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "findIndex" $ do
    it "returns error when no index" $ do
      findIndex "resources/test/findIndex/no" `shouldThrow` (== NoIndex)

    it "returns error when multiple index" $ do
      findIndex "resources/test/findIndex/many" `shouldThrow` (== MultipleIndex)

    it "returns path for only index" $ do
      x <- liftIO $ findIndex "resources/test/findIndex/one"
      x `shouldBe` "index.md"

  describe "getProject" $ do
    it "returns project for valid project" $ do
      x <- liftIO $ getProject "resources/test/projects/collision-zone"
      slug x `shouldBe` "collision-zone"