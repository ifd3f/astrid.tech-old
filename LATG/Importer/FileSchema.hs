{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LATG.Importer.FileSchema where

import GHC.Generics ( Generic )
import Data.Vector((!), (!?))
import Data.Aeson
    ( FromJSON(parseJSON),
      (.:),
      (.:?),
      withArray,
      withObject,
      withText,
      Value(String, Object, Bool) )
import Data.Maybe ( fromMaybe )
import Data.Time.LocalTime ( ZonedTime )
import Data.UUID ( UUID )
import qualified Data.Text as T

data Document a = Document
  { uuid :: UUID
  , createdDate :: ZonedTime
  , publishedDate :: ZonedTime
  , updatedDate :: Maybe ZonedTime
  , content :: Maybe Content
  , tags :: [String]
  , colophon :: Maybe String
  , docType :: a
  }
  deriving (Show)

instance Functor Document where
  fmap f x = x { docType = f (docType x) }

type GenericDocument = Document DocTypeObj

instance FromJSON GenericDocument where 
  parseJSON = withObject "GenericDocument" $ \o ->
    let 
      docTypeObj = do
        attrs <- o .: "attrs"
        docType <- (o .: "docType") >>= parseJSON
        case docType of
          HEntry -> HEntryObj <$> parseJSON attrs
          XProject -> XProjectObj <$> parseJSON attrs
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
  , slug :: Slug
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

instance FromJSON RSVPValue where 
  parseJSON (Bool True) = pure RSVPYes
  parseJSON (Bool False) = pure RSVPNo
  parseJSON (String "yes") = pure RSVPYes
  parseJSON (String "no") = pure RSVPNo
  parseJSON (String "maybe") = pure RSVPMaybe
  parseJSON (String "interested") = pure RSVPInterested
  parseJSON unknown = fail $ "Unknown value for RSVP: " ++ show unknown

data ProjectStatus = Early | WIP | Scrapped | Complete
  deriving (Generic, Show, Eq)
instance FromJSON ProjectStatus

data Slug = Slug
  { year :: Int
  , month :: Int
  , day :: Int
  , ordinal :: Int
  , shortName :: Maybe T.Text
  }
  deriving (Generic, Show, Eq)

mkSlug :: Int -> Int -> Int -> Int -> T.Text -> Slug
mkSlug y m d o n = Slug y m d o (Just n)
mkSlug' :: Int -> Int -> Int -> Int -> Slug
mkSlug' y m d o = Slug y m d o Nothing

instance FromJSON Slug where 
  parseJSON = withArray "Slug" $ \a -> do 
    if length a > 5 
      then fail "Slug array is too long"
      else pure ()
    if length a < 4
      then fail "Slug array is too short"
      else pure ()
    Slug <$>
        parseJSON (a ! 0) <*>
        parseJSON (a ! 1) <*>
        parseJSON (a ! 2) <*>
        parseJSON (a ! 3) <*>
        case a !? 4 of 
          Just x -> parseJSON x
          Nothing -> pure Nothing

data Content 
  = EmbeddedPlaintext T.Text 
  | FileRef { src :: Maybe String, downloadable :: Bool }
  deriving (Generic, Show, Eq)

instance FromJSON Content where 
  parseJSON (Object o) = FileRef <$>
    o .:? "src" <*>
    (fromMaybe False <$> o .:? "downloadable")
  parseJSON (String t) = pure $ EmbeddedPlaintext t
  parseJSON unknown = fail $ "Unknown value for Content: " ++ show unknown