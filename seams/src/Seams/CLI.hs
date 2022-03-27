module Seams.CLI where
import Options.Applicative

data Args = Import {
  contentDir :: FilePath,
  dryRun :: Bool
}

main :: IO ()
main = execParser parserInfo >>= execute

parserInfo :: ParserInfo Args
parserInfo = info (parser <**> helper) (fullDesc <> header "seams - a Content Management System")

parser :: Parser Args
parser = Import <$>
  argument str (metavar "CONTENT_DIR") <*>
  switch
    ( long "verify" <>
      help "Only verify if the content is correct." )

execute :: Args -> IO ()
execute cfg = do
  putStrLn $ "Using dir " ++ contentDir cfg
  return ()

