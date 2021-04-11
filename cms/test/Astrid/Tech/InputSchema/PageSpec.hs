{-# LANGUAGE QuasiQuotes #-}

module Astrid.Tech.InputSchema.PageSpec
  ( spec,
  )
where

import Astrid.Tech.InputSchema.Page
import Astrid.Tech.InputSchema.TestUtil (rootResourcesPath)
import Astrid.Tech.InputSchema.Util (readDirTreeBS)
import Control.Exception (throw)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import Test.Hspec
import Text.RawString.QQ (r)

data ExampleFrontMatter = ExampleFrontMatter
  { a :: String,
    b :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromJSON ExampleFrontMatter

type FMResult = Either PageException ExampleFrontMatter

markdownWithFrontMatter :: ByteString
markdownWithFrontMatter =
  [r|---
a: "boat"
b: 31
---

# Title of my post
|]

expectedMarkdownContent :: Text
expectedMarkdownContent = "\n# Title of my post\n"

resources :: FilePath
resources = rootResourcesPath </> "findIndex"

spec :: Spec
spec = do
  describe "findIndex" $ do
    it "throws NoIndex when no index" $ do
      tree <- readDirTreeBS (resources </> "no")
      findIndex tree `shouldBe` Left NoIndex

    it "throws MultipleIndex when multiple index" $ do
      tree <- readDirTreeBS (resources </> "many")
      findIndex tree `shouldBe` Left MultipleIndex

    it "returns path for only index" $ do
      tree <- readDirTreeBS (resources </> "one/index.md")
      result <- either throw pure $ findIndex tree
      assetRoot result `shouldBe` (resources </> "one/")
      name result `shouldBe` "one"

    it "returns path for bare file" $ do
      tree <- readDirTreeBS (resources </> "this-file.md")
      result <- either throw pure $ findIndex tree
      assetRoot result `shouldBe` (resources </> "one/")
      name result `shouldBe` "this-file"

  describe "detectFormatFromExtension" $ do
    it "returns Nothing for unsupported extensions" $
      detectFormatFromExtension ".exe" `shouldBe` Nothing

-- describe "parsePage" $ do
--   it "parses markdown" $
--     case parsePage "directory" "someslug.md" markdownWithFrontMatter :: FMResult of
--       Right (fm, page) -> do
--         fm `shouldBe` ExampleFrontMatter {a = "boat", b = 31}
--         content page `shouldBe` expectedMarkdownContent
--       Left err -> error $ show err
--
--   it "throws errors for unsupported extensions" $
--     (parsePage "directory" "virus.exe" markdownWithFrontMatter :: FMResult)
--       `shouldBe` Left (UnsupportedFormat ".exe")
