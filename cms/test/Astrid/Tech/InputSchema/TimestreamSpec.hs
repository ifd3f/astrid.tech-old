module Astrid.Tech.InputSchema.TimestreamSpec (spec) where

import Astrid.Tech.InputSchema.TestUtil (rootResourcesPath)
import Astrid.Tech.InputSchema.Timestream
import Astrid.Tech.InputSchema.Util (readDirectoryBS)
import System.FilePath ((</>))
import Test.Hspec

resources :: FilePath
resources = rootResourcesPath </> "timestreams"

spec :: Spec
spec = do
  describe "readTimestream" $ do
    it "loads files" $ do
      tree <- readDirectoryBS $ resources </> "single"
      let stream = readTimestream tree
      stream 
