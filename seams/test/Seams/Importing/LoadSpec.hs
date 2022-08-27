{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing.LoadSpec where

import Control.Lens
import Control.Monad.Except
import Data.List (intercalate, sort)
import Data.Validation
import Seams.Importing.FileSchema
import Seams.Importing.Load
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.TestUtils
import System.FilePath
import Test.Hspec

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
                  Failure x -> error $ intercalate "\n" $ map show x
                  Success y -> y
      length (dat ^. lcPosts) `shouldBe` 6
  describe "loadDocs" $ do
    it "loads example folder blog correctly" $ do
      result <-
        runExceptT $ runReadFileT (loadDocs (exampleDir </> "blog")) ioReadFile
      let dat :: [LoadedDoc PostMeta] = sort $
            case result of
              Left x -> error $ show x
              Right v ->
                case v of
                  Failure x -> error $ intercalate "\n" $ map show x
                  Success y -> y
      (dat ^? ix 0 . ldPath) `shouldBe`
        Just (exampleDir </> "blog" </> "2015-01-01.html.yml")
  describe "loadMergeableDir" $ do
    context "when loading tags" $ do
      it "loads example folder tags" $ do
        result <-
          runExceptT $
          runReadFileT (loadMergeableDir (exampleDir </> "tags")) ioReadFile
        let dat :: TagConfig =
              case result of
                Left x -> error $ show x
                Right v ->
                  case v of
                    Failure x -> error $ show x
                    Success y -> y
        (dat ^. tcfgTitles . at "foo-bar") `shouldBe` Just "Foo Bar"
        (dat ^. tcfgColors ^? ix 1 . tcsTags) `shouldBe` Just ["spam-eggs"]
