{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Seams.Importing.ReadFileSpec where

import Data.ByteString
import qualified Data.Map as M
import Seams.Importing.Load
import Seams.Importing.ReadFile
import System.FilePath
import Test.Hspec

rfFromFilemap :: [(FilePath, ReadResult a)] -> FilePath -> Maybe (ReadResult a)
rfFromFilemap pairs path = M.lookup path $ M.fromList pairs

exampleFS = rfFromFilemap [("test", File "bs1")]

spec = do
  describe
    "ReadFileT"
    do describe
         "envReadFile"
         do it
              "reads existing files correctly"
              do let result = runReadFileT (envReadFile "test") exampleFS
                 result `shouldBe` Just "bs1"
            it
              "reads nonexistent files correctly"
              do let result =
                       runReadFileT (envReadFile "this-doesnt-exist") exampleFS
                 result `shouldBe` Nothing
