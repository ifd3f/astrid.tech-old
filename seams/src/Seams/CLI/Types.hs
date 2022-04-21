{-# LANGUAGE TemplateHaskell #-}

module Seams.CLI.Types where

import Control.Lens.TH
import qualified Data.ByteString.UTF8 as BSU
import Database.Persist.Postgresql
import System.Directory
import System.Environment

class Executable a where
  execute :: a -> SeamsEnv -> IO ()

data SeamsEnv =
  SeamsEnv
    { _envDBUrl :: Maybe ConnectionString
    , _envContentDir :: FilePath
    }
  deriving (Show, Eq)

makeLenses ''SeamsEnv

getContentDir :: IO FilePath
getContentDir = lookupEnv "SEAMS_CONTENT_DIR" >>= maybe getCurrentDirectory pure

getDBUrl :: IO (Maybe ConnectionString)
getDBUrl = fmap BSU.fromString <$> lookupEnv "SEAMS_DB_URL"

getEnv :: IO SeamsEnv
getEnv = SeamsEnv <$> getDBUrl <*> getContentDir
