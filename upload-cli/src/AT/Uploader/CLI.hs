module AT.Uploader.CLI where

import Data.Maybe (fromMaybe)
import Data.Text
import Network.Minio
import Options.Applicative
import System.Environment (getEnv)

data CLI =
  CLI
    { files :: [FilePath]
    }

cli :: Parser CLI
cli = CLI <$> (many (argument (str) (metavar "FILE")))

data Env =
  Env
    { bucket :: Bucket
    , credentials :: Credentials
    }

getEnvs :: IO Env
getEnvs =
  Env <$> (pack <$> getEnv "UPLOAD_BUCKET_NAME") <*>
  (fromMaybe <$> (ioError $ userError "Couldn't find credentials") <*>
   (findFirst [fromAWSEnv, fromAWSConfigFile]))

