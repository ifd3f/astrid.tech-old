{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing.FileSchemaSpec where

import Test.Hspec
import Seams.Importing.FileSchema
import Seams.TestUtils
import Text.RawString.QQ
import Control.Lens
import Data.Yaml
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "FileSlug" do
    it "parses from array of only ints" do
      let input = "[3, 2, 1, 4]"
      let actual :: Maybe PostSlug = decode input
      actual `shouldBe` Just (PostSlug 3 2 1 4 Nothing)

    it "parses from array with name" do
      let input = "[3, 2, 1, 4, \"test\"]"
      let actual :: Maybe PostSlug = decode input
      actual `shouldBe` Just (PostSlug 3 2 1 4 $ Just "test")

  describe "Doc" do
    it "parses meta as flattened" do
      let input = [r|
uuid: "f1d12ae0-11a7-4658-9817-a54105fc3f90"
title: "foobar lol"
rsvp: false
slug: [2012, 12, 8, 0, "birthday"]
"content": "plaintext content"
"tags": ["a", "b"]
"time":
  published: "2012-12-08T03:32:01-08:00"
|]
      
      let doc :: Doc PostMeta = decodeYamlOrError input
      doc^.docColophon `shouldBe` Nothing

      let extra = doc^.docExtra
      extra^.postRSVP `shouldBe` Just RSVPNo
      extra^.postTitle `shouldBe` Just "foobar lol"
      extra^.postTagline `shouldBe` Nothing

  describe "TagConfig" do
    it "parses full file" do
      let input = [r|
titles:
  foo-bar: Foo Bar
colors:
  - text: "#ffffff"
    bg: "#000000"
    tags:
      - foo-bar
  - text: "#000000"
    bg: "#ffffff"
    tags:
      - spam-eggs
|]
      let cfg :: TagConfig = decodeYamlOrError input

      M.lookup "foo-bar" (cfg^.tcfgTitles) `shouldBe` Just "Foo Bar"
      cfg^.tcfgColors `shouldBe`
        [ TagColorSheet (Just "#ffffff") (Just "#000000") ["foo-bar"]
        , TagColorSheet (Just "#000000") (Just "#ffffff") ["spam-eggs"]
        ]

