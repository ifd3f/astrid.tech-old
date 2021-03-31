module Astrid.Tech.InputSchema.Blog
  ( BlogPost,
    BlogPostMeta,
  )
where

import Astrid.Tech.InputSchema.Page (Page)
import Astrid.Tech.Slug (DatedSlug (DatedSlug))
import Data.Aeson (FromJSON)
import Data.Time (Day)
import GHC.Generics (Generic)

data BlogPostMeta = BlogPostMeta
  { title :: String,
    description :: String,
    date :: Day,
    thumbnail :: Maybe String,
    tags :: [String]
  }
  deriving (Generic)

instance FromJSON BlogPostMeta

data BlogPost = BlogPost
  { slug :: DatedSlug,
    assetRoot :: FilePath,
    meta :: BlogPostMeta,
    page :: Page
  }

-- parseBlogPost :: FilePath -> IO BlogPost
