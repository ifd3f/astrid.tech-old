{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LATG.Importer.FileSchema where

import GHC.Generics

import Control.Monad
import Data.Aeson
import Data.Aeson.Types

import Data.Maybe
import Data.Time.LocalTime
import Data.UUID
import Debug.Trace
import qualified Data.Text as T

data Document a = Document
  { uuid :: UUID
  , createdDate :: ZonedTime
  , publishedDate :: ZonedTime
  , updatedDate :: Maybe ZonedTime
  , content :: Maybe Content
  , tags :: [String]
  , colophon :: Maybe String
  , object :: a
  }
  deriving (Show)

type GenericDocument = Document DocTypeObj

instance FromJSON GenericDocument where 
  parseJSON = withObject "GenericDocument" $ \o ->
    let 
      docTypeObj = do
        attrs <- o .: "attrs"
        docType <- (o .: "docType") >>= parseJSON
        result <- case docType of
          HEntry -> HEntryObj <$> parseJSON attrs
          XProject -> XProjectObj <$> parseJSON attrs
        pure (trace (show result) result)
    in
      Document <$>
        o .: "uuid" <*>
        o .: "createdDate" <*>
        o .: "publishedDate" <*>
        o .:? "updatedDate" <*>
        o .:? "content" <*>
        (fromMaybe [] <$> o .:? "tags") <*>
        o .:? "colophon" <*>
        docTypeObj

data DocTypeObj = HEntryObj Entry | XProjectObj Project deriving (Generic, Show)

data DocType = HEntry | XProject deriving (Generic, Show, Eq)

instance FromJSON DocType where 
  parseJSON = withText "DocType" $ \t -> case strToDocType t of 
    Just x -> return x
    Nothing -> fail $ "Unsupported type " ++ show t

strToDocType :: T.Text -> Maybe DocType
strToDocType "h-entry" = Just HEntry
strToDocType "x-project" = Just XProject
strToDocType _ = Nothing

data Entry = Entry
  { name :: Maybe T.Text
  , summary :: Maybe T.Text
  , location :: Maybe T.Text 
  , photos :: Maybe [T.Text]
  , replyTo :: Maybe [T.Text]
  , repostOf :: Maybe T.Text
  , rsvp :: Maybe RSVP
  }
  deriving (Generic, Show, Eq)

instance FromJSON Entry

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

instance FromJSON Project

data RSVP = RSVP { to :: T.Text, value :: RSVPValue } deriving (Generic, Show, Eq)
instance FromJSON RSVP

data RSVPValue = RSVPYes | RSVPNo | RSVPMaybe | RSVPInterested
  deriving (Generic, Show, Eq)
instance FromJSON RSVPValue

data ProjectStatus = Early | WIP | Scrapped | Complete
  deriving (Generic, Show, Eq)
instance FromJSON ProjectStatus

data Slug = Slug
  { year :: Int
  , month :: Int
  , day :: Int
  , ordinal :: Int
  , slug :: Maybe T.Text
  }
  deriving (Generic, Show, Eq)

data Content 
  = EmbeddedPlaintext T.Text 
  | FileRef { src :: Maybe T.Text, downloadable :: Maybe Bool }
  deriving (Generic, Show, Eq)

instance FromJSON Content