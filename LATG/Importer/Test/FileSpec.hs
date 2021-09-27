{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FileSpec where 

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Aeson
import LATG.Importer.FileSchema
import Data.UUID

spec :: Spec
spec = do 
  describe "GenericDocument" $ do 
    it "loads from JSON entries" $ do
      json <- BL.readFile "LATG/Example/2015-01-01.html.json"

      let result :: GenericDocument = case eitherDecode json of 
            Right x -> x
            Left msg -> error msg

      let expectedUUID = fromJust $ fromString "1bc6dde3-7512-4a4d-bf6f-cc21fe6c97f6"
      uuid result `shouldBe` expectedUUID

      let entry = case docType result of 
            HEntryObj e -> e
            x -> error $ "Not an entry: " ++ show x
      slug entry `shouldBe` mkSlug 2015 1 1 0 "json-metadata"

  describe "Slug" $ do 
    it "reads arrays with short name" $ do
      let json = "[2000, 12, 8, 0, \"birthday-post\"]"

      let result :: Slug = case eitherDecode json of 
            Right x -> x
            Left msg -> error msg

      result `shouldBe` mkSlug 2000 12 8 0 "birthday-post"

    it "reads arrays without short name" $ do
      let json = "[2000, 12, 8, 0]"

      let result :: Slug = case eitherDecode json of 
            Right x -> x
            Left msg -> error msg

      result `shouldBe` mkSlug' 2000 12 8 0

    it "gracefully fails short arrays" $ do
      let json = "[2000, 12, 8]"

      case eitherDecode json :: Either String Slug of 
        Right x -> expectationFailure $ "Incorrectly decoded: " ++ show x
        Left msg -> pure ()

    it "gracefully fails long arrays" $ do
      let json = "[2000, 12, 8, 0, \"this\", \"is\", \"too\", \"long\"]"

      case eitherDecode json :: Either String Slug of 
        Right x -> expectationFailure $ "Incorrectly decoded: " ++ show x
        Left msg -> pure ()
