{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Astrid.Tech.InputSchema.Timestream
  ( Timestream (..),
    Year (..),
    Month (..),
    Day (..),
    entries,
    dayObjects,
    days,
    monthObjects,
    months,
    yearObjects,
    years,
    readTimestream,
    readYear,
    readMonth,
    readDay,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Either (partitionEithers)
import Data.Either.Combinators (mapRight)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (Dir))
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import Text.Read (readMaybe)

data Day e d = Day
  { _entries :: Map.Map Int e,
    _dayObjects :: [d]
  }

makeLenses ''Day

data Month e d m = Month
  { _days :: Map.Map Int (Day e d),
    _monthObjects :: [m]
  }

makeLenses ''Month

data Year e d m y = Year
  { _months :: Map.Map Integer (Month e d m),
    _yearObjects :: [y]
  }

makeLenses ''Year

data Timestream e d m y = Timestream
  { _years :: Map.Map Integer (Year e d m y)
  }

makeLenses ''Timestream

liftMultiError :: ([b1] -> b2) -> FilePath -> [Either [FilePath] b1] -> Either [FilePath] b2
liftMultiError f anchor subDirs = case partitionEithers subDirs of
  ([], successes) -> Right $ f successes
  (errors, _) -> Left $ map (anchor </>) (concat errors)

-- | A dreadful horror show of a function that abstracts away several things.
-- |
-- | First argument constructs the parent object from a map of key to child directory, and its list of index children if it has any.
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

readTimestream :: AnchoredDirTree a -> Either [FilePath] (Timestream (AnchoredDirTree a) (AnchoredDirTree a) (AnchoredDirTree a) (AnchoredDirTree a))
readTimestream (anchor :/ dir) = case dir of
  Dir _ children ->
    let subDirs =
          mapMaybe
            ( \childDir -> do
                let dirName = DT.name childDir
                key :: Integer <- readMaybe dirName
                let anchoredChildDir = (anchor </> dirName) :/ childDir
                pure $ mapRight (\childObj -> (key, childObj)) $ readYear anchoredChildDir
            )
            children
     in case partitionEithers subDirs of
          ([], successes) ->
            Right $
              Timestream
                { _years = Map.fromList successes
                }
          (errors, _) -> Left $ map (anchor </>) (concat errors)
  invalid -> Left $ [anchor </> DT.name invalid]
