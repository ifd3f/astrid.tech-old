module Seams.TypesSpec where

import Data.Foldable (for_)
import Seams.Types
import Test.Hspec
import Text.Printf

spec :: Spec
spec = do
  describe "extensionToDocumentType" $ do
    context "for correct extensions" $ do
      let cases =
            [ (".yml", YAML)
            , (".yaml", YAML)
            , ("yaml", YAML)
            , (".json", YAML)
            , ("md", FrontmatterMarkdown)
            , ("markdown", FrontmatterMarkdown)
            , (".md", FrontmatterMarkdown)
            ]
      for_ cases $ \(ext, expected) -> do
        it (printf "parses %s as %s" ext (show expected)) $ do
          extensionToDocumentType ext `shouldBe` Just expected
    context "for bad extensions" $ do
      let cases = [".asdfkl", "html", "jso", "mad"]
      for_ cases $ \ext -> do
        it (printf "rejects %s" ext) $ do
          extensionToDocumentType ext `shouldBe` Nothing
  describe "extensionToContentType" $ do
    context "for correct extensions" $ do
      let cases =
            [ (".htm", HTML)
            , ("HTM", HTML)
            , (".html", HTML)
            , ("md", Markdown)
            , (".IpYnB", Jupyter)
            , ("ipynb", Jupyter)
            , (".txt", Plaintext)
            , ("txt", Plaintext)
            ]
      for_ cases $ \(ext, expected) -> do
        it (printf "parses %s as %s" ext (show expected)) $ do
          extensionToContentType ext `shouldBe` Just expected
    context "for bad extensions" $ do
      let cases = [".asdfkl", "json", "yml", "text", "jso", "mad"]
      for_ cases $ \ext -> do
        it (printf "rejects %s" ext) $ do
          extensionToContentType ext `shouldBe` Nothing
