{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Astrid.Tech.InputSchema.Timestream
  ( Timestream (..),
    Year (..),
    Month (..),
    Day (..),
  )
where

import Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (Dir))
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import Text.Read (readMaybe)

data TimeFolder o k v = TimeFolder
  { _objects :: [o],
    _children :: Map.Map k v
  }

makeLenses ''TimeFolder

type Day d e = TimeFolder d Int e

type Month m d e = TimeFolder m Int (Day d e)

type Year y m d e = TimeFolder y Int (Month m d e)

type Timestream y m d e = TimeFolder () Integer (Year y m d e)

instance Functor (TimeFolder o k) where
  fmap f day = day {_children = fmap f (_children day)}

class FromDirectory dt e a where
  constructFromDir :: DT.AnchoredDirTree dt -> Either e a

instance FromDirectory dt () (DT.AnchoredDirTree dt) where
  constructFromDir dt = Right dt

data TimeDirectoryConstructionError ce = ChildConstructionError [ce] | InvalidName FilePath | InvalidIndexFolder

instance
  (Ord k, Read k, FromDirectory dt ce v, FromDirectory dt ce o) =>
  FromDirectory dt (TimeDirectoryConstructionError ce) (TimeFolder (Either ce o) k (Either ce v))
  where
  constructFromDir (anchor :/ dir) = case dir of
    invalid -> Left (InvalidName $ anchor </> DT.name invalid)
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

          indexChildDirs = do
            indexDir <- maybeToList $ DT.dropTo "index" (anchor :/ dir)
            case DT.dirTree indexDir of
              Dir dirName indexContents -> map ((anchor </> dirName) :/) indexContents
              _ -> []

          constructIndexResults :: [Either ce o] = map constructFromDir indexChildDirs
       in Right $
            TimeFolder
              { _children = (Map.fromList constructChildResults),
                _objects = constructIndexResults
              }
