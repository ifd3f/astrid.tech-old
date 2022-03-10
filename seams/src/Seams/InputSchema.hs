{-# LANGUAGE DuplicateRecordFields #-}

module Seams.ContentFolder where

data Post = Post {
  location :: FilePath,
  meta :: PostMeta,
  content :: Content
}

data Project = Project {
  location :: FilePath,
  meta :: ProjectMeta,
  content :: Content
}

data Content = Embedded { ctype :: ContentType, body :: Text } | FileRef {
  ctype :: ContentType,
  file :: FilePath 
} 

data ContentType = Markdown | HTML 

data PostMeta = PostMeta {
  title :: Maybe Text,
  slug :: Maybe Text,
  tagline :: Maybe Text,
  thumbnail :: Maybe Text,
  tags :: [Text],
  time :: Timestamps,
  ordinal :: Integer
}

data ProjectMeta = ProjectMeta {
  title :: Text,
  tagline :: Text,
  thumbnail :: Maybe Text,
  tags :: [Text],
  time :: Timestamps,
  started :: DateTime,
  ended :: Maybe DateTime,
  ordinal :: Integer
}

data Timestamps = Timestamps {
  created :: DateTime,
  modified :: DateTime,  
  published :: DateTime
}

