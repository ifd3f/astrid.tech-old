{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module LATG.Importer.FileSchema where

import GHC.Generics

import Data.Time.LocalTime
import Data.UUID
import qualified Data.Text as T

data Document = Document
  { uuid :: UUID
  , createdDate :: ZonedTime
  , publishedDate :: ZonedTime
  , updatedDate :: Maybe ZonedTime
  , document :: DocType
  , content :: Content
  , tags :: Maybe [T.Text]
  , colophon :: Maybe T.Text
  }
  deriving (Show)

data DocType = HEntry Entry | XProject Project deriving (Generic, Show)

data Entry = Entry
  { name :: Maybe T.Text
  , summary :: Maybe T.Text
  , location :: Maybe T.Text 
  , photos :: Maybe [T.Text]
  , replyTo :: Maybe [T.Text]
  , repostOf :: Maybe T.Text
  , rsvp :: Maybe RSVP
  }
  deriving (Generic, Show)

data Project = Project
  { status :: ProjectStatus
  , startedDate :: ZonedTime
  , finishedDate :: Maybe ZonedTime
  , sortDate :: Maybe ZonedTime
  , name :: T.Text
  , summary :: T.Text 
  , url :: Maybe T.Text 
  , source :: Maybe T.Text 
  , location :: Maybe T.Text 
  }
  deriving (Generic, Show)

data RSVP = RSVP { to: Text, value: RSVPValue } deriving (Generic, Show)

data RSVPValue = RSVPYes | RSVPNo | RSVPMaybe | RSVPInterested
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

data Content = 
  | EmbeddedPlaintext T.Text 
  | FileRef { src :: Maybe T.Text, downloadable :: Maybe Bool }
  deriving (Generic, Show)
