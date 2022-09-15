module AT.Uploader.Image where

import Data.Char (toLower)

isImage :: String -> Bool
isImage ext = ext' `elem` ["jpg", "jpeg", "png"]
  where
    lower = map toLower ext
    ext' =
      case lower of
        '.':cs -> cs
        e -> e
