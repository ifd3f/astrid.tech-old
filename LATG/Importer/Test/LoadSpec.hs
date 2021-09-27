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
type ReadDocumentResult'' = ReadDocumentResult (EncodedDocument FSch.Content)

spec :: Spec
spec = do 
  describe "readDocument" $ do 
    it "reads JSON" $ do
      content <- BL.readFile "LATG/Example/2015-01-01.html.json"

      case readDocument ".json" content :: ReadDocumentResult' of
        Right (EncodedDocument Nothing _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads YAML" $ do
      content <- BL.readFile "LATG/Example/2021/2021-09-25T01-31-40-328Z.html.yml"

      case readDocument ".yaml" content :: ReadDocumentResult' of
        Right (EncodedDocument Nothing _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads TOML" $ do
      content <- BL.readFile "LATG/Example/2012/2012-12-21-end-of-the-world.txt.toml"

      case readDocument ".toml" content :: ReadDocumentResult' of
        Right (EncodedDocument Nothing _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "returns NotADocument for non-frontmatter Markdown" $ do
      content <- BL.readFile "LATG/Example/2015-08-10.md"

      (readDocument ".md" content :: ReadDocumentResult'') `shouldBe` Left NonDocument

    it "returns NotADocument for unsupported extension" $ do
      let content = "myYaml: is cool"

      (readDocument ".notyaml" content :: ReadDocumentResult'') `shouldBe` Left NonDocument

    it "reads frontmatter Markdown" $ do
      content <- BL.readFile "LATG/Example/2021/2021-09-25-test-post.md"

      case readDocument ".md" content :: ReadDocumentResult' of
        Right (EncodedDocument (Just (MarkdownType, _)) _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "gracefully fails invalid JSON" $ do
      let content = "lol"

      case readDocument ".json" content :: ReadDocumentResult' of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "gracefully fails invalid TOML" $ do
      let content = "lol"

      case readDocument ".toml" content :: ReadDocumentResult' of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "gracefully fails frontmatter Markdown with invalid markdown" $ do
      let content = "---\nunclosedList:[\n---\nmy *markdown goes* here"

      case readDocument ".md" content :: ReadDocumentResult' of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

  describe "extractContentSource" $ do 
    it "handles same name-style documents" $ do
      let path = "mydir/foo/something.txt.toml"
      let content = Just $ FSch.FileRef Nothing False

      let result = extractContentSource path $ documentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/something.txt")

    it "handles content.src-style documents with relative paths" $ do
      let path = "mydir/foo/something.yaml"
      let content = Just $ FSch.FileRef (Just "../over/here.html") False

      let result = extractContentSource path $ documentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/../over/here.html")

    it "handles content.src-style documents in same directory" $ do
      let path = "mydir/foo/something.yaml"
      let content = Just $ FSch.FileRef (Just "samedir.html") False

      let result = extractContentSource path $ documentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/samedir.html")

    it "handles embedded markdown documents" $ do
      let path = "mydir/foo/something.md"
      let content = Nothing
      let embedded = Just (MarkdownType, "this is markdown")

      let result = extractContentSource path $ EncodedDocument embedded content

      result `shouldBe` (Right $ EmbeddedContent MarkdownType "this is markdown")

    it "handles embedded plaintext documents" $ do
      let path = "mydir/foo/something.json"
      let content = Just $ FSch.EmbeddedPlaintext "my plaintext"

      let result = extractContentSource path $ documentOnly content 

      result `shouldBe` (Right $ EmbeddedContent PlaintextType "my plaintext")

    it "gracefully fails embedded content with content.src" $ do
      let path = "mydir/foo/something.md"
      let content = Just $ FSch.FileRef (Just "../over/here.html") False
      let embedded = Just (MarkdownType, "this is embedded in the document")

      let result = extractContentSource path $ EncodedDocument embedded content

      case result of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly extracted: " ++ show x

  describe "loadContentSource" $ do
    it "passthroughs embedded markdown" $ do
      let input = EmbeddedContent MarkdownType "this is **markdown**"

      result <- loadContentSource input 

      result `shouldBe` Right (MarkdownType, "this is **markdown**")

    it "passthroughs embedded plaintext" $ do
      let input = EmbeddedContent PlaintextType "this is plaintext"

      result <- loadContentSource input 

      result `shouldBe` Right (PlaintextType, "this is plaintext")

    it "reads HTM from fileref" $ do
      let input = FileRef "LATG/Example/2021/something.HTM"
      let expected = "<p>Who the hell uses .htm for their HTML pages anymore?</p>"

      result <- loadContentSource input 

      result `shouldBe` Right (HTMLType, expected)

    it "reads HTML from fileref" $ do
      let input = FileRef "LATG/Example/2015-01-01.html"
      let expected = "<p>You can also specify your metadata in JSON.</p>"

      result <- loadContentSource input 

      result `shouldBe` Right (HTMLType, expected)

    it "reads Markdown from fileref" $ do
      let input = FileRef "LATG/Example/2015-01-01.html"
      let expected = "<p>You can also specify your metadata in JSON.</p>"

      result <- loadContentSource input 

      result `shouldBe` Right (HTMLType, expected)

    it "reads plaintext from fileref" $ do
      let input = FileRef "LATG/Example/2012/2012-12-21-end-of-the-world.txt"
      let expected = "Wait a sec, the world didn't end\n"

      result <- loadContentSource input 

      result `shouldBe` Right (PlaintextType, expected)

    it "gracefully fails unsupported file extension" $ do
      let input = FileRef "LATG/Example/non-sourced.json"

      result <- loadContentSource input 

      case result of
        Left _ -> pure ()
        x -> error $ "Incorrectly loaded: " ++ show x
