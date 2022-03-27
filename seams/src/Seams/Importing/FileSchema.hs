{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module defines the input schema and some parsing stuff.
module Seams.Importing.FileSchema where

import Control.Lens.TH
import Data.Text(Text)
import Data.Time
import Data.Char
import Data.Aeson
import Data.Aeson.TH
import Data.UUID
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Data.Maybe (maybeToList)
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as M
import Seams.Types

-- | JSON of a document's metadata.
data Doc extra = Doc {
  _docUUID :: Maybe UUID,
  _docExtra :: extra,
  _docTime :: Timestamps,
  _docColophon :: Maybe Text,
  _docContent :: Maybe ContentField,
  -- | HTML thumbnail.
  _docThumbnail :: Maybe FilePath,
  -- | HTML preview description.
  _docPreview :: Maybe Text
} deriving (Show, Functor)

-- | Document fields. Document Meta fields are flattened.
instance FromJSON meta => FromJSON (Doc meta) where
  parseJSON = withObject "Doc" $ \o ->
    Doc <$>
      o .: "uuid" <*>
      parseJSON (Object o) <*> -- meta's fields are flattened
      o .: "time" <*>
      o .:? "colophon" <*>
      o .:? "content" <*>
      o .:? "thumbnail" <*>
      o .:? "preview"

-- | A field that refers to another content object.
data ContentField
  = FileRef FilePath
  | EmbeddedPlaintext Text
  deriving (Show)

instance ToJSON ContentField where
  toJSON (FileRef path) = object [ "path" .= path ]
  toJSON (EmbeddedPlaintext text) = A.String text

instance FromJSON ContentField where
  parseJSON x = case x of
    A.String t -> pure $ EmbeddedPlaintext t
    Object o -> EmbeddedPlaintext <$> o .: "path"
    other -> fail $ "ContentField: unexpected object" ++ show other

data Timestamps = Timestamps {
  -- | When this content was published on the website.
  _tsPublished :: ZonedTime,
  -- | When this content was created. Semantically, the content itself, not
  -- | the document holding the content.
  -- | If Nothing, then it is the same as published.
  _tsCreated :: Maybe ZonedTime,
  -- | When this content was last edited, or Nothing if it was not edited
  -- | after publishing.
  _tsModified :: Maybe ZonedTime
} deriving (Show)

$(deriveJSON defaultOptions{
  fieldLabelModifier = map toLower . drop 3
} ''Timestamps)

-- | The slug that a post will be placed under.
-- | The dates don't have to correspond to the actual publishing dates.
data PostSlug = PostSlug {
  _slugYear :: Int,
  _slugMonth :: Int,
  _slugDay :: Int,
  _slugOrdinal :: Int,
  _slugName :: Maybe Text
} deriving (Show, Eq)

-- | A PostSlug is an array like so:
-- | [2020, 3, 20, 2] 
-- | [2020, 3, 20, 2, "some-slug"] 
instance FromJSON PostSlug where
  parseJSON = withArray "PostSlug" $ \a -> do
    if length a > 5
      then fail "array is too long"
      else pure ()
    if length a < 4
      then fail "array is too short"
      else pure ()

    PostSlug <$>
      parseJSON (a ! 0) <*>
      parseJSON (a ! 1) <*>
      parseJSON (a ! 2) <*>
      parseJSON (a ! 3) <*>
      traverse parseJSON (a !? 4)

instance ToJSON PostSlug where
  toJSON (PostSlug y m d o n) = Array $ V.fromList $
    (Number . fromIntegral <$> [y, m, d, o]) ++ (String <$> maybeToList n)

data RSVP = RSVPYes | RSVPNo | RSVPMaybe | RSVPInterested
  deriving (Show, Eq)

instance FromJSON RSVP where
  parseJSON (Bool True) = pure RSVPYes
  parseJSON (Bool False) = pure RSVPNo
  parseJSON (String "yes") = pure RSVPYes
  parseJSON (String "no") = pure RSVPNo
  parseJSON (String "maybe") = pure RSVPMaybe
  parseJSON (String "interested") = pure RSVPInterested
  parseJSON unknown = fail $ "Unknown value for RSVP: " ++ show unknown

instance ToJSON RSVP where
  toJSON RSVPYes = String "yes"
  toJSON RSVPNo = String "no"
  toJSON RSVPMaybe = String "maybe"
  toJSON RSVPInterested = String "interested"

-- | Metadata associated with a blog post.
data PostMeta = PostMeta {
  _postTitle :: Maybe Text,
  _postSlug :: PostSlug,
  _postTagline :: Maybe Text,
  _postRSVP :: Maybe RSVP,
  _postTags :: [Text]
} deriving (Show, Eq)

$(deriveJSON defaultOptions{
  fieldLabelModifier = map toLower . drop 5
} ''PostMeta)

-- | Metadata associated with a project.
data ProjectMeta = ProjectMeta {
  _projectTitle :: Text,
  _projectTagline :: Text,
  _projectSlug :: Text,
  _projectTags :: [Text],
  _projectStart :: ZonedTime,
  _projectSource :: Maybe String,
  _projectURI :: Maybe String,
  _projectTile :: Maybe FilePath,
  _projectEnd :: Maybe ZonedTime
} deriving (Show)

$(deriveJSON defaultOptions{
  fieldLabelModifier = map toLower . drop 7
} ''ProjectMeta)

data TagColorSheet = TagColorSheet {
  _tcsText :: Maybe Color,
  _tcsBG :: Maybe Color,
  _tcsSlugs :: [Text]
} deriving (Show, Eq)

makeColorMap :: TagColorSheet -> TagColorMap
makeColorMap (TagColorSheet appText appBg slugs) =
  M.fromList $ map (, TagColors appText appBg) slugs

$(deriveJSON defaultOptions{
  fieldLabelModifier = map toLower . drop 4
} ''TagColorSheet)

data TagConfig = TagConfig {
  _tcfgTitles :: Map Text Text,
  _tcfgColors :: [TagColorSheet]
} deriving (Show, Eq)

$(deriveJSON defaultOptions{
  fieldLabelModifier = map toLower . drop 4
} ''TagConfig)

instance Semigroup TagConfig where
  TagConfig lt lc <> TagConfig rt rc = TagConfig (lt <> rt) (lc <> rc)

instance Monoid TagConfig where
  mempty = TagConfig mempty mempty

makeLenses ''ProjectMeta
makeLenses ''PostMeta
makeLenses ''PostSlug
makeLenses ''TagColorSheet
makeLenses ''TagConfig
makeLenses ''Doc
makeLenses ''Timestamps

