{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Seams.DB.Models where

import Data.Text (Text)
import Database.Persist.Postgresql
import Database.Persist.TH 
import Data.Time
import Data.ByteString
import Seams.Types

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
TagObj
  deriving Show

Tag
  slug Text 
  title Text Maybe
  textColor Text Maybe
  bgColor Text Maybe

  UniqueTagSlug slug
  deriving Show

TagAssoc
  tagObj TagObjId
  ordinal Int
  tag TagId

  UniqueAssocs tagObj tag ordinal
  deriving Show

Content
  path FilePath Unique
  cType ContentType
  body ByteString
  deriving Show

Doc
  contentObj ContentId 
  path FilePath 
  published UTCTime
  created UTCTime Maybe
  updated UTCTime Maybe
  deriving Show

Post
  title Text Maybe
  tagline Text Maybe

  slugYear Int
  slugMonth Int
  slugDay Int
  slugOrdinal Int
  slugName Text Maybe

  doc DocId 
  tagObj TagObjId 

  UniquePostSlug slugYear slugMonth slugDay slugOrdinal
  deriving Show

Project
  title Text 
  tagline Text 
  slug Text 

  started UTCTime

  doc DocId 
  tagObj TagObjId 

  deriving Show
|]

