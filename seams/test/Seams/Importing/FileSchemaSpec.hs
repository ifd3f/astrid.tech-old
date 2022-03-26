{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Seams.Importing.FileSchemaSpec where

import Test.Hspec
import Seams.Importing.FileSchema
import Text.RawString.QQ
import Control.Lens
import Data.Yaml

spec = do
  describe "FileSlug" $ do
    it "parses from array of only ints" $ do
      let input = "[3, 2, 1, 4]"
      let actual :: Maybe PostSlug = decode input
      actual `shouldBe` Just (PostSlug 3 2 1 4 Nothing)

    it "parses from array with name" $ do
      let input = "[3, 2, 1, 4, \"test\"]"
      let actual :: Maybe PostSlug = decode input
      actual `shouldBe` Just (PostSlug 3 2 1 4 $ Just "test")

  describe "Doc" $ do
    it "parses meta as flattened" $ do
      let input = [r|
uuid: "f1d12ae0-11a7-4658-9817-a54105fc3f90"
title: "foobar lol"
rsvp: false
slug: [2012, 12, 8, 0, "birthday"]
"content": "plaintext content"
"tags": ["a", "b"]
"time":
  published: "2012-12-08T03:32:01-08:00"|]
      
      let doc = case decodeEither input of
            Right x -> x :: Doc PostMeta
            Left err -> error err

      doc^.docColophon `shouldBe` Nothing

      let meta = doc^.docMeta

      meta^.postRSVP `shouldBe` Just RSVPNo
      meta^.postTitle `shouldBe` Just "foobar lol"
      meta^.postTagline `shouldBe` Nothing

