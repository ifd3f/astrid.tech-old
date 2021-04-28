module System.Directory.Tree.From (FromDirectory (..)) where

import qualified System.Directory.Tree as DT

class FromDirectory dt e a where
  constructFromDir :: DT.AnchoredDirTree dt -> Either e a

instance FromDirectory dt () (DT.AnchoredDirTree dt) where
  constructFromDir dt = Right dt
