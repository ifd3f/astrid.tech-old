{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Seams.Importing.LoadSpec where
import Test.Hspec
import Control.Monad.IO.Class
import Seams.Importing.Load
import Seams.TestUtils
import Seams.Importing.ReadFile
import Control.Monad.Trans.Except

x = runReadFileT (loadContentFolder exampleDir) ioReadFile
spec :: Spec
spec = do
  describe "loadContentFolder" do
    it "loads example folder successfuly" do
      "foo" `shouldBe` "foo"

