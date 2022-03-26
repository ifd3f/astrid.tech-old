{-# LANGUAGE TemplateHaskell #-}

module Seams.Importing.Types where
import Seams.InputSchema
import Data.ByteString

data Document m = Document {
  _docPath :: FilePath,
  _docMeta :: m,
  _docContent :: Content
}

instance Functor Document where
  fmap f (Document l m c) = Document l (f m) c

data Content = Content {
  _contentPath :: FilePath,
  _contentType :: ContentType,
  _contentBody :: ByteString
}

makeLenses ''Document
makeLenses ''Content

