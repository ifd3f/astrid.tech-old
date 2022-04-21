{-# LANGUAGE TemplateHaskell #-}

module Seams.CLI.Upload where

import Control.Lens
import Control.Monad.Except
import Database.Persist.Postgresql
import Options.Applicative
import Seams.CLI.Types
import Seams.Importing.Load
import Seams.Importing.ReadFile

newtype Upload =
  Upload
    { _uploadDryRun :: Bool
    }
  deriving (Show, Eq)

makeLenses ''Upload

data UploadBehavior =
  UploadBehavior
    { _ubContentDir :: FilePath
    , _ubDatabase :: Maybe ConnectionString
    }
  deriving (Show, Eq)

makeLenses ''UploadBehavior

parseInputs :: Upload -> SeamsEnv -> UploadBehavior
parseInputs (Upload dry) env
  | dry = UploadBehavior contentDir Nothing
  | otherwise =
    case env ^. envDBUrl of
      Nothing -> error "ERROR: No database URL was provided."
      Just x -> UploadBehavior contentDir (Just x)
  where
    contentDir = env ^. envContentDir

uploadParser :: Parser Upload
uploadParser =
  Upload <$>
  switch
    (long "dry-run" <>
     short 'd' <>
     help
       "Perform a dry run, where we only verify the content. If this is not provided, then SEAMS_DB_URL must be provided.")

instance Executable Upload where
  execute args env = do
    _ <-
      runExceptT $
      runReadFileT (loadContentFolder (ub ^. ubContentDir)) ioReadFile
    case ub ^. ubDatabase of
      Nothing -> putStrLn "Skipping upload due to dry run"
      Just _ -> error "Not yet implemented"
    return ()
    where
      ub = parseInputs args env
