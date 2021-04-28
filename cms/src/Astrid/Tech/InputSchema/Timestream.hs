{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Astrid.Tech.InputSchema.Timestream
  ( TimeDirectory (..),
    objects,
    children,
    Timestream,
    Year,
    Month,
    Day,
  )
where

import Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (Dir))
import qualified System.Directory.Tree as DT
import System.Directory.Tree.From
import System.FilePath ((</>))
import Text.Read (readMaybe)

data TimeDirectory o k v = TimeDirectory
  { _objects :: [o],
    _children :: Map.Map k v
  } deriving (Show, Eq)

makeLenses ''TimeDirectory

type Day d e = TimeDirectory d Int e

type Month m d e = TimeDirectory m Int (Day d e)

type Year y m d e = TimeDirectory y Int (Month m d e)

type Timestream y m d e = TimeDirectory () Integer (Year y m d e)

instance Functor (TimeDirectory o k) where
  fmap f day = day {_children = fmap f (_children day)}

data TimeDirectoryConstructionError = NonDirectoryError FilePath

instance
  (Ord k, Read k, FromDirectory dt ce v, FromDirectory dt ce o) =>
  FromDirectory dt TimeDirectoryConstructionError (TimeDirectory (Either ce o) k (Either ce v))
  where
  constructFromDir (anchor :/ dir) = case dir of
    Dir _ childDirs ->
      let validChildDirs =
            mapMaybe
              ( \childDir -> do
                  let dirName = DT.name childDir
                  key :: k <- readMaybe dirName
                  pure (key, (anchor </> dirName) :/ childDir)
              )
              childDirs

          constructChildResults :: [(k, Either ce v)] = map (fmap constructFromDir) validChildDirs

          indexChildDirs = map constructFromDir $ case DT.dropTo "index" (anchor :/ dir) of
            Nothing -> []
            Just indexDir -> case DT.dirTree indexDir of
              Dir dirName indexContents -> map ((anchor </> dirName) :/) indexContents
              _ -> []
       in Right $
            TimeDirectory
              { _children = (Map.fromList constructChildResults),
                _objects = indexChildDirs
              }
    nonDir -> Left (NonDirectoryError $ anchor </> DT.name nonDir)
