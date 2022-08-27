{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seams.Importing.Types where

import Control.Lens
import Control.Lens.TH
import Data.ByteString
import Data.Yaml
import Seams.Importing.FileSchema
import Seams.Types

data LoadedContent =
  LoadedContent
    { _lcPosts :: [LoadedDoc PostMeta]
    , _lcProjects :: [LoadedDoc ProjectMeta]
    , _lcTagConfigs :: TagConfig
    }

data LoadedDoc m =
  LoadedDoc
    { _ldPath :: FilePath
    , _ldMeta :: Doc m
    , _ldContent :: Content
    }
  deriving (Show, Functor)

data Content =
  Content
    { _contentPath :: FilePath
    , _contentType :: ContentType
    , _contentBody :: ByteString
    }
  deriving (Show, Eq, Ord)

data WithPath a =
  WithPath
    { _rPath :: FilePath
    , _rResult :: a
    }
  deriving (Show, Eq, Ord, Functor)

data LoadError
  = BadYaml String
  | BadYaml' ParseException
  | BadConfig
  | IncompleteFrontmatter
  | NoContent FilePath
  | NoDocument DocumentType FilePath
  | UnsupportedDocumentExtension String
  | UnsupportedContentExtension String
  deriving (Show)

makeLenses ''WithPath

makeLenses ''LoadedContent

makeLenses ''LoadedDoc

makeLenses ''Content

instance Eq (LoadedDoc m) where
  a == b = (a ^. ldPath) == (b ^. ldPath)

instance Ord (LoadedDoc m) where
  a `compare` b = (a ^. ldPath) `compare` (b ^. ldPath)
