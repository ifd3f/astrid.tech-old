{-# LANGUAGE OverloadedStrings #-}

module Seams.ImportingSpec where

import Test.Hspec
import Seams.Importing
import System.FilePath


loadPathContent = Loader $ \p -> Right $ \t -> (p, t)

spec = do
  describe "Loader" $ do
    it "runs correctly for no redirect case" $ do
      let r = runLoader loadPathContent "a/b/c"

      fst r `shouldBe` "a/b/c"
      snd r (Right "foobar") `shouldBe` ("a/b/c", Right "foobar")
    
    it "runs correctly for redirect case" $ do
      let redir = Loader $ \p -> Left ("other" </> p, loadPathContent)

      let r = runLoader redir "a/b/c"

      fst r `shouldBe` "other" </> "a/b/c"
      snd r (Right "cont") `shouldBe` ("other" </> "a/b/c", Right "cont")

  describe "cd" $ do
    it "changes directory of operations" $ do
      let r = runLoader (cd "a/b" loadPathContent) "d/e"

      fst r `shouldBe` "d/e" </> "a/b"
      snd r (Right "cont") `shouldBe` ("d/e" </> "a/b", Right "cont")

