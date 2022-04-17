module Seams.CLI where

import Options.Applicative

class Executable a where
  execute :: a -> IO ()

data SeamsArgs
  = CmdUpload Upload
  | CmdServe Serve
  deriving (Show, Eq)

instance Executable SeamsArgs where
  execute (CmdUpload a) = execute a
  execute (CmdServe a) = execute a

data Upload =
  Upload
    { contentDir :: FilePath
    , targetDatabase :: Maybe String
    }
  deriving (Show, Eq)

instance Executable Upload where
  execute cfg = do
    putStrLn $ "Using dir " ++ contentDir cfg
    return ()

data Serve =
  Serve
    { serverAddr :: String
    , sourceDatabase :: String
    }
  deriving (Show, Eq)

instance Executable Serve where
  execute cfg = do
    putStrLn $ "Using address " ++ show (serverAddr cfg)
    return ()

parserInfo :: ParserInfo SeamsArgs
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "seams - a Content Management System")

parser :: Parser SeamsArgs
parser =
  subparser
    (command
       "upload"
       (info
          (CmdUpload <$> uploadParser)
          (progDesc "Upload a content directory to a database.")) <>
     command
       "serve"
       (info
          (CmdUpload <$> uploadParser)
          (progDesc "Start a server for some data.")))

uploadParser :: Parser Upload
uploadParser =
  Upload <$> argument str (metavar "CONTENT_DIR") <*>
  optional
    (strOption $
     long "db" <>
     short 'd' <>
     help
       "URL to the database to upload data to. If not provided, assuming dry run.")

serveParser :: Parser Serve
serveParser =
  Serve <$> argument str (metavar "PORT") <*>
  strOption
    (long "db" <> short 'd' <> help "URL to the database to serve data from.")

main :: IO ()
main = execParser parserInfo >>= execute
