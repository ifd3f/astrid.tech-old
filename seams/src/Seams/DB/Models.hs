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

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
TagObj
  deriving Show

Tag
  slug Text
  title Text Maybe

  UniqueSlug slug
  deriving Show

TagAssoc
  tagObj TagObjId
  ordinal Int
  tag TagId

  UniqueAssocs tagObj tag ordinal
  deriving Show

Content
  contentType Text
  body Text
  deriving Show

Doc
  published UTCTime
  created UTCTime Maybe
  updated UTCTime Maybe
  content ContentId
  deriving Show

BlogPost
  title Text Maybe
  doc DocId
  tagObj TagObjId
  deriving Show
|]

