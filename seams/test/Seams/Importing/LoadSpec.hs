{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing.LoadSpec where

import Control.Lens
import Control.Monad.Except
import Data.Validation
import Seams.Importing.Load
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.TestUtils
import Test.Hspec
import System.FilePath
import Seams.Importing.FileSchema

spec :: Spec
spec = do
  describe "loadContentFolder" $ do
    it "loads example folder correctly" $ do
      result <-
        runExceptT $ runReadFileT (loadContentFolder exampleDir) ioReadFile
      let dat =
            case result of
              Left x -> error $ show x
              Right v ->
                case v of
                  Failure x -> error $ show x
                  Success y -> y
      length (dat ^. lcPosts) `shouldBe` 3

  describe "loadMergeableDir" $ do
    context "when loading tags" $ do
      it "loads example folder tags" $ do
        result <-
          runExceptT $ runReadFileT (loadMergeableDir (exampleDir </> "tags")) ioReadFile
        let dat :: TagConfig =
              case result of
                Left x -> error $ show x
                Right v ->
                  case v of
                    Failure x -> error $ show x
                    Success y -> y
        (dat ^. tcfgTitles . at "foo-bar") `shouldBe` Just "Foo Bar"
        (dat ^. tcfgColors ^? ix 1 . tcsTags) `shouldBe` Just ["spam-eggs"]
