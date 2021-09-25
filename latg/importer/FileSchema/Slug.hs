module LATG.Importer.FileSchema.Slug where 

data Slug = Slug
  { year :: Int
  , month :: Int
  , day :: Int
  , ordinal :: Int
  , slug :: Maybe Text
  }
  deriving (Generic, Show)
