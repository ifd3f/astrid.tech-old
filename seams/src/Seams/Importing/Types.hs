{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seams.Importing.Types where
import Seams.Types
import Control.Lens.TH
import Data.ByteString
import Seams.Importing.FileSchema
import Data.Yaml

data LoadedContent = LoadedContent {
  _lcPosts :: [LoadedDoc (Doc PostMeta)],
  _lcProjects :: [LoadedDoc (Doc ProjectMeta)],
  _lcTagConfigs :: TagConfig
}

data LoadedDoc m = LoadedDoc {
  _ldPath :: FilePath,
  _ldMeta :: m,
  _ldContent :: Content
} deriving (Show, Functor)

data Content = Content {
  _contentPath :: FilePath,
  _contentType :: ContentType,
  _contentBody :: ByteString
} deriving (Show)

data WithPath a = WithPath {
  _rPath :: FilePath,
  _rResult :: a
} deriving (Functor)

data LoadError
  = BadYaml String
  | BadYaml' ParseException
  | BadConfig FilePath
  | IncompleteFrontmatter
  | NoContent FilePath
  | NoDocument FilePath
  | UnsupportedContentExtension String
  deriving (Show)

makeLenses ''WithPath
makeLenses ''LoadedContent
makeLenses ''LoadedDoc
makeLenses ''Content

