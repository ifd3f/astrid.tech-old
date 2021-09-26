{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LoadSpec where 

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Aeson
import LATG.Importer.FileSchema
import LATG.Importer.LoadContent
import Data.UUID

type ReadDocumentResult' = ReadDocumentResult (EncodedDocument GenericDocument)
spec :: Spec
spec = do 
  describe "readDocument" $ do 
    it "reads JSON" $ do
      content <- BL.readFile "latg/example/2015-01-01.html.json"

      case readDocument ".json" content :: ReadDocumentResult' of
        ValidDocument (DocumentOnly _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads YAML" $ do
      content <- BL.readFile "latg/example/2021/2021-09-25T01-31-40-328Z.html.yml"

      case readDocument ".yaml" content :: ReadDocumentResult' of
        ValidDocument (DocumentOnly _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads TOML" $ do
      content <- BL.readFile "latg/example/2012/2012-12-21-end-of-the-world.txt.toml"

      case readDocument ".toml" content :: ReadDocumentResult' of
        ValidDocument (DocumentOnly _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "returns NotADocument for non-frontmatter Markdown" $ do
      content <- BL.readFile "latg/example/2015-08-10.md"

      case readDocument ".md" content :: ReadDocumentResult' of
        NotADocument -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads frontmatter Markdown" $ do
      content <- BL.readFile "latg/example/2021/2021-09-25-test-post.md"

      case readDocument ".md" content :: ReadDocumentResult' of
        ValidDocument (DocumentWithContent _ _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x
