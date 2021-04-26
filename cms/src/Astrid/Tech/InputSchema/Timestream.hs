module Astrid.Tech.InputSchema.Timestream where

import Control.Lens (Lens')
import qualified Data.Map as Map
import Astrid.Tech.Slug (DatedSlug)

type Timestream a = Map.Map DatedSlug a

