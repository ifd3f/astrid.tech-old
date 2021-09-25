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
    it "loads JSON files" $ do
      json <- BL.readFile "latg/example/2015-01-01.html.json"

      let result :: GenericDocument = case eitherDecode json of 
            Right x -> x
            Left msg -> error msg

      let expectedUUID = fromJust $ fromString "1bc6dde3-7512-4a4d-bf6f-cc21fe6c97f6"
      uuid result `shouldBe` expectedUUID

      let entry = case docType result of 
            HEntryObj e -> e
            x -> error $ "Not an entry: " ++ show x
      slug entry `shouldBe` mkSlug 2015 1 1 0 "json-metadata"
      pure ()
