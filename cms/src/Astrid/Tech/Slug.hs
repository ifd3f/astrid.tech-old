module Astrid.Tech.Slug
  ( DatedSlug (..),
    ProjectSlug,
  )
where

import Data.List (intercalate)
import Text.Printf (printf)

data DatedSlug = DatedSlug
  { year :: Int,
    month :: Int,
    day :: Int,
    ord :: Int,
    name :: String
  }
  deriving (Eq)

instance Show DatedSlug where
  show (DatedSlug y m d o n) = printf "/%04d/%02d/%02d/%d/%s" y m d o n

type ProjectSlug = String
