module Astrid.Tech.InputSchema.Blog
  ( BlogPost,
    BlogPostMeta,
  )
where

import Astrid.Tech.InputSchema.Page
import Data.Aeson
import Data.Time.Clock
import GHC.Generics

data BlogPostMeta = BlogPostMeta
  { title :: String,
    description :: String,
    date :: UTCTime,
    thumbnail :: Maybe String,
    tags :: [String]
  }
  deriving (Generic)

instance FromJSON BlogPostMeta

data BlogPost = BlogPost
  { slug :: String,
    assetRoot :: String,
    meta :: BlogPostMeta,
    page :: Page
  }

-- parseBlogPost :: FilePath -> IO BlogPost
