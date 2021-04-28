module Astrid.Tech.InputSchema.TimestreamSpec (spec) where

import Astrid.Tech.InputSchema.TestUtil (rootResourcesPath)
import Astrid.Tech.InputSchema.Timestream
import Astrid.Tech.InputSchema.Util (readDirectoryBS)
import Control.Lens (at, (&), (^.), _Just)
import Data.Either (fromRight, isRight)
import qualified Data.Either.Unwrap as Unwrap
import System.Directory.Internal.Prelude (fromMaybe)
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck.Monadic (assert, stop)

resources :: FilePath
resources = rootResourcesPath </> "timestreams"

spec :: Spec
spec = do
  describe "readTimestream" $ do
    it "loads files" $ do
      tree <- readDirectoryBS $ resources </> "single"
      let stream = Unwrap.fromRight $ readTimestream tree
      let resultingEntries = stream ^. (years . (at 2000) . _Just . months . (at 3). _Just . days . (at 1). _Just . entries)

      length resultingEntries `shouldBe` 1
