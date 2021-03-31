module Astrid.Tech.InputSchema.ProjectSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Project
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Project.findIndex" $ do
    it "returns error when no index" $ do
      x <- liftIO $ findIndex "resources/test/findIndex/no"
      x `shouldBe` Left NoIndex

    it "returns error when multiple index" $ do
      x <- liftIO $ findIndex "resources/test/findIndex/many"
      x `shouldBe` Left MultipleIndex

    it "returns path for only index" $ do
      x <- liftIO $ findIndex "resources/test/findIndex/one"
      x `shouldBe` Right "index.md"