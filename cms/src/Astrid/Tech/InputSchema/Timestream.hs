module Astrid.Tech.InputSchema.Timestream (Timestream (..)) where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import System.Directory.Tree (DirTree (Dir), contents)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import Text.Read (readMaybe)

data Timestream e d m y = Timestream
  { years :: Map.Map Integer (Year e d m y)
  }

data Year e d m y = Year
  { yearObjects :: [y],
    months :: Map.Map Integer (Month e d m)
  }

data Month e d m = Month
  { monthObjects :: [m],
    days :: Map.Map Int (Day e d)
  }

data Day e d = Day
  { dayObjects :: [d],
    entries :: Map.Map Int e
  }

maybeNumericDirectory (Dir name ords) = do
  ord <- readMaybe name :: Maybe a
  pure (ord, Dir name ords)
maybeNumericDirectory invalid = Nothing

dirToDay ((DT.:/) anchor dir) = case dir of
  Dir _ children ->
    let ordinalDirs = mapMaybe maybeNumericDirectory children
        index = do
          indexDir <- maybeToList $ DT.dropTo "index" ((DT.:/) anchor dir)
          case DT.dirTree indexDir of
            Dir _ indexContents -> indexContents
            _ -> []
     in Right $
          Day
            { dayObjects = index,
              entries = Map.fromList ordinalDirs
            }
  invalid -> Left $ anchor </> DT.name invalid
