{-# LANGUAGE OverloadedStrings #-}

module Seams.Importing.LoadSpec where

import Test.Hspec
import Seams.Importing.Load
import System.FilePath
import qualified Data.Map as M
import Data.ByteString

rfFromFilemap :: [(FilePath, ByteString)] -> FilePath -> Maybe ByteString
rfFromFilemap pairs path = M.lookup path $ M.fromList pairs

exampleFS = rfFromFilemap
  [ ("test", "bs1")
  ]

spec = do
  describe "Loader" $ do
    describe "envReadFile" $ do
      it "reads existing files correctly" $ do
        let result = runReadFileT (envReadFile "test") exampleFS
        result `shouldBe` Just "bs1"

      it "reads nonexistent files correctly" $ do
        let result = runReadFileT (envReadFile "this-doesnt-exist") exampleFS
        result `shouldBe` Nothing

