{-# LANGUAGE OverloadedStrings #-}

module Seams.Importing.ReadFileSpec where

import qualified Data.Map as M
import Seams.Importing.ReadFile
import Test.Hspec
import Data.ByteString (ByteString)

rfFromFilemap :: [(FilePath, ReadResult a)] -> FilePath -> Maybe (ReadResult a)
rfFromFilemap pairs path = M.lookup path $ M.fromList pairs

exampleFS :: FilePath -> Maybe (ReadResult ByteString)
exampleFS = rfFromFilemap [("test", File "bs1")]

spec :: Spec
spec = do
  describe "ReadFileT" $ do
    describe "envReadFile" $ do
      it "reads existing files correctly" $ do
        let result = runReadFileT (envRead "test") exampleFS
        result `shouldBe` Just (File "bs1")
      it "reads nonexistent files correctly" $ do
        let result = runReadFileT (envRead "this-doesnt-exist") exampleFS
        result `shouldBe` Nothing
