{-# LANGUAGE TemplateHaskell #-}

module Seams.Importing.Types where
import Seams.Types
import Control.Lens.TH
import Data.ByteString
import Seams.Importing.FileSchema

data LoadedContent = LoadedContent {
  _lcPosts :: [LoadedDoc (Doc PostMeta)],
  _lcProjects :: [LoadedDoc (Doc ProjectMeta)],
  _lcTagConfigs :: TagConfig
}

data LoadedDoc m = LoadedDoc {
  _ldPath :: FilePath,
  _ldMeta :: m,
  _ldContent :: Content
} deriving (Show)

instance Functor LoadedDoc where
  fmap f (LoadedDoc l m c) = LoadedDoc l (f m) c

data Content = Content {
  _contentPath :: FilePath,
  _contentType :: ContentType,
  _contentBody :: ByteString
} deriving (Show)

makeLenses ''LoadedContent
makeLenses ''LoadedDoc
makeLenses ''Content

