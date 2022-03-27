module Seams.CLI where
import Options.Applicative

data Args = Import {
  contentDir :: FilePath,
  dryRun :: Bool
}

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

