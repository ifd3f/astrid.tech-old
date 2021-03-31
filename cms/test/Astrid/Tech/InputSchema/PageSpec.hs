{-# LANGUAGE QuasiQuotes #-}

module Astrid.Tech.InputSchema.PageSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Page (PageFormat (MarkdownFormat), parsePage)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ (r)

data ExampleFrontMatter = ExampleFrontMatter
  { a :: String,
    b :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromJSON ExampleFrontMatter

markdownWithFrontMatter :: ByteString
markdownWithFrontMatter =
  [r|---
a: "boat"
b: 31
---

# Title of my post
|]

spec :: Spec
spec = do
  describe "Page.parsePage" $ do
    it "parses markdown" $ do
      let (fm :: ExampleFrontMatter, page) = parsePage "directory" "someslug.md" markdownWithFrontMatter
       in fm `shouldBe` ExampleFrontMatter {a = "boat", b = 31}
