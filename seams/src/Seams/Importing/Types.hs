{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seams.Importing.Types where

import Control.Lens.TH
import Data.ByteString
import Data.Yaml
import Seams.Importing.FileSchema
import Seams.Types

data LoadedContent =
  LoadedContent
    { _lcPosts :: [LoadedDoc (Doc PostMeta)]
    , _lcProjects :: [LoadedDoc (Doc ProjectMeta)]
    , _lcTagConfigs :: TagConfig
    }

data LoadedDoc m =
  LoadedDoc
    { _ldPath :: FilePath
    , _ldMeta :: m
    , _ldContent :: Content
    }
  deriving (Show, Functor)

data Content =
  Content
    { _contentPath :: FilePath
    , _contentType :: ContentType
    , _contentBody :: ByteString
    }
  deriving (Show, Eq)

data WithPath a =
  WithPath
    { _rPath :: FilePath
    , _rResult :: a
    }
  deriving (Show, Eq, Functor)

data LoadError
  = BadYaml String
  | BadYaml' ParseException
  | BadConfig
  | IncompleteFrontmatter
  | NoContent FilePath
  | NoDocument FilePath
  | UnsupportedContentExtension String
  deriving (Show)

makeLenses ''WithPath

makeLenses ''LoadedContent

makeLenses ''LoadedDoc

makeLenses ''Content
