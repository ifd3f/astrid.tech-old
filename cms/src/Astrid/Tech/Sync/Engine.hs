module Astrid.Tech.Sync.Engine where

import Data.Time (UTCTime)

data ExistingExternalPost = ExternalNote
  { externalLink :: String,
    posted :: UTCTime,
    content :: String
  }

data NewExternalPost = NewExternalPost
  { backlink :: String,
    --content :: String,
    images :: [String]
  }

data NoteSyncDestination = NoteSyncDestination
  { syncTo :: NewExternalPost -> IO ExistingExternalPost,
    pull :: IO [ExistingExternalPost]
  }
