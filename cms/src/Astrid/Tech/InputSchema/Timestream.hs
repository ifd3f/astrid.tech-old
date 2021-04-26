module Astrid.Tech.InputSchema.Timestream (Timestream (..)) where

import Data.Either (partitionEithers)
import Data.Either.Combinators (mapRight)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (Dir))
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import Text.Read (readMaybe)

data Timestream e d m y = Timestream
  { years :: Map.Map Integer (Year e d m y)
  }

data Year e d m y = Year
  { months :: Map.Map Integer (Month e d m),
    yearObjects :: [y]
  }

data Month e d m = Month
  { days :: Map.Map Int (Day e d),
    monthObjects :: [m]
  }

data Day e d = Day
  { entries :: Map.Map Int e,
    dayObjects :: [d]
  }

liftMultiError f anchor subDirs = case partitionEithers subDirs of
  ([], successes) -> Right $ f successes
  (errors, _) -> Left $ map (anchor </>) (concat errors)

-- | A dreadful horror show of a function that abstracts away several things.
-- |
-- | First argument constructs the parent object from its child directories and its list of index children if it has any.
-- | Second argument constructs the child objects, or errors with a list of failing files.
-- | Third argument is the root of the directory to read.
readNumericDir ::
  (Ord k, Read k) =>
  (Map.Map k child -> [AnchoredDirTree dt] -> r) ->
  (AnchoredDirTree dt -> Either [FilePath] child) ->
  AnchoredDirTree dt ->
  Either [FilePath] r
readNumericDir constructParent fChildren (anchor :/ dir) = case dir of
  Dir _ children ->
    let subDirs =
          mapMaybe
            ( \childDir -> do
                let dirName = DT.name childDir
                key <- readMaybe dirName
                let anchoredChildDir = (anchor </> dirName) :/ childDir
                pure $ mapRight (\childObj -> (key, childObj)) $ fChildren anchoredChildDir
            )
            children

        indexObjects = do
          indexDir <- maybeToList $ DT.dropTo "index" (anchor :/ dir)
          case DT.dirTree indexDir of
            Dir dirName indexContents -> map ((anchor </> dirName) :/) indexContents
            _ -> []
     in case partitionEithers subDirs of
          ([], successes) -> Right $ constructParent (Map.fromList successes) indexObjects
          (errors, _) -> Left $ map (anchor </>) (concat errors)
  invalid -> Left $ [anchor </> DT.name invalid]

readDay :: AnchoredDirTree a -> Either [FilePath] (Day (AnchoredDirTree a) (AnchoredDirTree a))
readDay = readNumericDir Day Right

readMonth :: AnchoredDirTree a -> Either [FilePath] (Month (AnchoredDirTree a) (AnchoredDirTree a) (AnchoredDirTree a))
readMonth = readNumericDir Month readDay

readYear :: AnchoredDirTree a -> Either [FilePath] (Year (AnchoredDirTree a) (AnchoredDirTree a) (AnchoredDirTree a) (AnchoredDirTree a))
readYear = readNumericDir Year readMonth
