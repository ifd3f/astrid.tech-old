{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LoadSpec where 

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Aeson
import qualified LATG.Importer.FileSchema as FSch
import LATG.Importer.LoadContent 
import Data.UUID

type ReadDocumentResult' = ReadDocumentResult (EncodedDocument FSch.GenericDocument)
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
        ValidDocument (DocumentWithMarkdown _ _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

  describe "extractContentSource" $ do 
    it "handles same name-style documents" $ do
      let path = "mydir/foo/something.txt.toml"
      let content = Just $ FSch.FileRef Nothing False

      let result = extractContentSource path $ DocumentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/something.txt")

    it "handles content.src-style documents with relative paths" $ do
      let path = "mydir/foo/something.yaml"
      let content = Just $ FSch.FileRef (Just "../over/here.html") False

      let result = extractContentSource path $ DocumentOnly content

      result `shouldBe` (Right $ FileRef "mydir/over/here.html")

    it "handles markdown documents" $ do
      let path = "mydir/foo/something.md"
      let content = Nothing

      let result = extractContentSource path $ DocumentWithMarkdown content "my markdown"

      result `shouldBe` (Right $ EmbeddedMarkdown "my markdown")

    it "handles embedded plaintext documents" $ do
      let path = "mydir/foo/something.json"
      let content = Just $ FSch.EmbeddedPlaintext "my plaintext"

      let result = extractContentSource path $ DocumentWithMarkdown content "my markdown"

      result `shouldBe` (Right $ EmbeddedPlaintext "my plaintext")

    it "fails content.src-style documents that leave the content dir" $ do
      let path = "mydir/foo/something.toml"
      let content = Just $ FSch.FileRef (Just "../../../../../root/.ssh/id_rsa") False

      let result = extractContentSource path $ DocumentOnly content

      case result of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly extracted: " ++ show x

    it "fails markdown with content.src" $ do
      let path = "mydir/foo/something.md"
      let content = Just $ FSch.FileRef (Just "../over/here.html") False

      let result = extractContentSource path $ DocumentWithMarkdown content "this is text"

      case result of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly extracted: " ++ show x