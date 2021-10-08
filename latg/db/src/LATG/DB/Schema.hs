{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module LATG.DB.Schema where

import Data.Aeson
import Data.Time.LocalTime ( ZonedTime )
import GHC.Generics

data PublishingDates = 
  PublishingDates {
    created :: ZonedTime,
    published :: ZonedTime,
    updated :: Maybe ZonedTime
  } deriving (Show, Generic)

instance ToJSON PublishingDates
instance FromJSON PublishingDates

data Document = 
  Document {
    content :: String,
    pageSrcUrl :: Maybe String
  } deriving (Show, Generic)

instance ToJSON Document
instance FromJSON Document

data Tagging = 
  Tagging {
    tags :: [String]
  } deriving (Show, Generic)

instance ToJSON Tagging
instance FromJSON Tagging

data Entry = 
  Entry {
    uuid :: String,
    document :: Document,
    dates :: PublishingDates,
    tagging :: Tagging
  } deriving (Show, Generic)

instance ToJSON Entry
instance FromJSON Entry
