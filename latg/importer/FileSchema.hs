{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module LATG.Importer.FileSchema where

import GHC.Generics

import Data.Time.LocalTime
import Data.UUID
import qualified Data.Text as T

data Entry = Entry
  { name :: Maybe T.Text
  , uuid :: UUID
  , slug :: Slug
  , summary :: Maybe T.Text
  , createdDate :: ZonedTime
  , publishedDate :: ZonedTime
  , updatedDate :: Maybe ZonedTime
  , tags :: Maybe [T.Text]
  , location :: Maybe T.Text 
  , photos :: Maybe [T.Text]
  , replyTo :: Maybe [T.Text]
  , repostOf :: Maybe T.Text
  , rsvp :: RSVP
  , downloadable :: Bool
  , colophon :: Maybe T.Text
  }
  deriving (Generic, Show)

data Project = Project
  { uuid :: T.Text
  , slug :: T.Text
  , status :: ProjectStatus
  , startedDate :: ZonedTime
  , finishedDate :: Maybe ZonedTime
  , sortDate :: Maybe ZonedTime
  , publishedDate :: Maybe ZonedTime
  , updatedDate :: Maybe ZonedTime
  , name :: T.Text
  , summary :: Maybe T.Text 
  , url :: Maybe T.Text 
  , source :: Maybe T.Text 
  , location :: Maybe T.Text 
  , tags :: [T.Text]
  }
  deriving (Generic, Show)

data RSVP = RSVPYes | RSVPNo | RSVPMaybe | RSVPInterested
  deriving (Generic, Show)

data ProjectStatus = Early | WIP | Scrapped | Complete
  deriving (Generic, Show)

data Slug = Slug
  { year :: Int
  , month :: Int
  , day :: Int
  , ordinal :: Int
  , slug :: Maybe T.Text
  }
  deriving (Generic, Show)

data Content = EmbeddedPlaintext T.Text | FileRef { src :: T.Text }
  deriving (Generic, Show)
