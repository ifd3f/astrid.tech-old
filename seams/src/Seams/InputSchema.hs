{-# LANGUAGE DuplicateRecordFields #-}

module Seams.ContentFolder where

data Post = Post {
  meta :: PostMeta,
  content :: Content
}

data Content = EmbeddedMarkdown { markdown :: Text } | EmbeddedPlaintext { plaintext :: Text } | FileRef {
  ctype :: ContentType,
  file :: FilePath 
} 

data ContentType = Markdown | HTML 

data PostMeta = PostMeta {
  title :: Text,
  tagline :: Text,
  tags :: [Text],
  time :: Timestamps,
  ordinal :: Integer
}

data ProjectMeta = ProjectMeta {
  title :: Text,
  tagline :: Text,
  tags :: [Text],
  time :: Timestamps,
  ordinal :: Integer
}

data Timestamps = Timestamps {
  created :: DateTime,
  modified :: DateTime,  
  published :: DateTime
}