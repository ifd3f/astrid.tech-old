module Seams.CLI where

import Options.Applicative
import Seams.CLI.ServeContent
import Seams.CLI.Types
import Seams.CLI.Upload

data SeamsArgs
  = CmdUpload Upload
  | CmdServeContent ServeContent
  deriving (Show, Eq)

instance Executable SeamsArgs where
  execute (CmdUpload a) = execute a
  execute (CmdServeContent a) = execute a

parserInfo :: ParserInfo SeamsArgs
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "seams - a Content Management System")

parser :: Parser SeamsArgs
parser =
  hsubparser
    (command
       "upload"
       (info
          (CmdUpload <$> uploadParser)
          (progDesc "Upload a content directory to the database.")) <>
     command
       "serve-content"
       (info
          (CmdServeContent <$> serveContentParser)
          (progDesc "Serve an API to access the content.")))

main :: IO ()
main = do
  args <- execParser parserInfo
  env <- getEnv
  execute args env
