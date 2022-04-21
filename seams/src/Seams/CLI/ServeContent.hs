module Seams.CLI.ServeContent where

import Options.Applicative
import Seams.CLI.Types

newtype ServeContent =
  ServeContent
    { serverAddr :: String
    }
  deriving (Show, Eq)

serveContentParser :: Parser ServeContent
serveContentParser = ServeContent <$> argument str (metavar "address")

instance Executable ServeContent where
  execute cfg _ = do
    putStrLn $ "Using address " ++ show (serverAddr cfg)
    return ()
