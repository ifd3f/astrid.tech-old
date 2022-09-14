{-# LANGUAGE OverloadedStrings #-}

module AT.Uploader where

import AT.Uploader.CLI (CLI(CLI), Env(Env), cli, getEnvs)
import AT.Uploader.Load
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import Data.Traversable (for)
import Network.Minio
  ( Bucket
  , ConnectInfo
  , Minio
  , defaultPutObjectOptions
  , fPutObject
  , runMinio
  , setCreds
  )
import Options.Applicative (execParser, idm, info)
import System.FilePath

main :: IO ()
main = do
  (CLI filePaths) <- execParser (info cli idm)
  (Env bucket creds) <- getEnvs
  let ci = setCreds creds b2cfg

  putStrLn "Uploading"
  _ <- runMinio ci $ for filePaths (uploadFile bucket)
  pure ()

getBucketDest :: FilePath -> IO String
getBucketDest path = do
  hash <- getHash path
  pure $ hash </> takeFileName path

uploadFile :: Bucket -> FilePath -> Minio ()
uploadFile bucket p = do
  bucketDest <- liftIO $ getBucketDest p
  fPutObject bucket (T.pack bucketDest) p defaultPutObjectOptions
  let url = "https://" ++ T.unpack bucket ++ ".s3.us-west-000.backblazeb2.com/" </> bucketDest
  liftIO $ putStrLn ("![](" ++ url ++ ")")
  pure ()

b2cfg :: ConnectInfo
b2cfg = "https://s3.us-west-000.backblazeb2.com"
