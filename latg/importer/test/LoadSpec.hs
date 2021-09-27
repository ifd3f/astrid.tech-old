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
      content <- BL.readFile "latg/example/2015-01-01.html.json"

      case readDocument ".json" content :: ReadDocumentResult' of
        Right (DocumentOnly _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads YAML" $ do
      content <- BL.readFile "latg/example/2021/2021-09-25T01-31-40-328Z.html.yml"

      case readDocument ".yaml" content :: ReadDocumentResult' of
        Right (DocumentOnly _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "reads TOML" $ do
      content <- BL.readFile "latg/example/2012/2012-12-21-end-of-the-world.txt.toml"

      case readDocument ".toml" content :: ReadDocumentResult' of
        Right (DocumentOnly _) -> pure ()
        x -> expectationFailure $ "Incorrectly decoded: " ++ show x

    it "returns NotADocument for non-frontmatter Markdown" $ do
      content <- BL.readFile "latg/example/2015-08-10.md"

      (readDocument ".md" content :: ReadDocumentResult'') `shouldBe` Left NonDocument

    it "returns NotADocument for unsupported extension" $ do
      let content = "myYaml: is cool"

      (readDocument ".notyaml" content :: ReadDocumentResult'') `shouldBe` Left NonDocument

    it "reads frontmatter Markdown" $ do
      content <- BL.readFile "latg/example/2021/2021-09-25-test-post.md"

      case readDocument ".md" content :: ReadDocumentResult' of
        Right (DocumentWithMarkdown _ _) -> pure ()
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

      let result = extractContentSource path $ DocumentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/something.txt")

    it "handles content.src-style documents with relative paths" $ do
      let path = "mydir/foo/something.yaml"
      let content = Just $ FSch.FileRef (Just "../over/here.html") False

      let result = extractContentSource path $ DocumentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/../over/here.html")

    it "handles content.src-style documents in same directory" $ do
      let path = "mydir/foo/something.yaml"
      let content = Just $ FSch.FileRef (Just "samedir.html") False

      let result = extractContentSource path $ DocumentOnly content

      result `shouldBe` (Right $ FileRef "mydir/foo/samedir.html")

    it "handles markdown documents" $ do
      let path = "mydir/foo/something.md"
      let content = Nothing

      let result = extractContentSource path $ DocumentWithMarkdown content "my markdown"

      result `shouldBe` (Right $ EmbeddedMarkdown "my markdown")

    it "handles embedded plaintext documents" $ do
      let path = "mydir/foo/something.json"
      let content = Just $ FSch.EmbeddedPlaintext "my plaintext"

      let result = extractContentSource path $ DocumentOnly content 

      result `shouldBe` (Right $ EmbeddedPlaintext "my plaintext")

    it "fails markdown with content.src" $ do
      let path = "mydir/foo/something.md"
      let content = Just $ FSch.FileRef (Just "../over/here.html") False

      let result = extractContentSource path $ DocumentWithMarkdown content "this is text"

      case result of
        Left _ -> pure ()
        x -> expectationFailure $ "Incorrectly extracted: " ++ show x

  describe "loadContentSource" $ do
    it "passthroughs embedded markdown" $ do
      let input = EmbeddedMarkdown "this is **markdown**"

      result <- loadContentSource input 

      result `shouldBe` Right (MarkdownType, "this is **markdown**")

    it "passthroughs embedded plaintext" $ do
      let input = EmbeddedPlaintext "this is plaintext"

      result <- loadContentSource input 

      result `shouldBe` Right (PlaintextType, "this is plaintext")

    it "reads HTM from fileref" $ do
      let input = FileRef "latg/example/2021/something.HTM"
      let expected = "<p>Who the hell uses .htm for their HTML pages anymore?</p>"

      result <- loadContentSource input 

      result `shouldBe` Right (HTMLType, expected)

    it "reads HTML from fileref" $ do
      let input = FileRef "latg/example/2015-01-01.html"
      let expected = "<p>You can also specify your metadata in JSON.</p>"

      result <- loadContentSource input 

      result `shouldBe` Right (HTMLType, expected)

    it "reads Markdown from fileref" $ do
      let input = FileRef "latg/example/2015-01-01.html"
      let expected = "<p>You can also specify your metadata in JSON.</p>"

      result <- loadContentSource input 

      result `shouldBe` Right (HTMLType, expected)

    it "reads plaintext from fileref" $ do
      let input = FileRef "latg/example/2012/2012-12-21-end-of-the-world.txt"
      let expected = "Wait a sec, the world didn't end\n"

      result <- loadContentSource input 

      result `shouldBe` Right (PlaintextType, expected)

    it "gracefully fails unsupported file extension" $ do
      let input = FileRef "latg/example/non-sourced.json"

      result <- loadContentSource input 

      case result of
        Left _ -> pure ()
        x -> error $ "Incorrectly loaded: " ++ show x
