{-# LANGUAGE OverloadedStrings #-}

module AT.Uploader where

import AT.Uploader.CLI (CLI(CLI), Env(Env), cli, getEnvs)
import AT.Uploader.Load
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.MIME.Types (defaultmtd, guessType, readMIMETypes)
import qualified Data.Text as T
import Data.Traversable (for)
import Network.Minio
  ( Bucket
  , ConnectInfo
  , Minio
  , PutObjectOptions(pooContentEncoding, pooContentType)
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
  _ <- for filePaths (uploadFile ci bucket)
  pure ()

getBucketDest :: FilePath -> IO String
getBucketDest path = do
  hash <- getHash path
  pure $ hash </> takeFileName path

uploadFile :: ConnectInfo -> Bucket -> FilePath -> IO ()
uploadFile ci bucket p = do
  let (contentType, encoding) = guessType defaultmtd False p
  let opts =
        defaultPutObjectOptions
          { pooContentType = T.pack <$> contentType
          , pooContentEncoding = T.pack <$> encoding
          }
  bucketDest <- getBucketDest p
  putStrLn $ show (contentType, encoding) ++ bucketDest
  result <- runMinio ci $ fPutObject bucket (T.pack bucketDest) p opts
  case result of
    Right _ -> pure ()
    Left err -> error (show err)
  let url =
        "https://" ++
        T.unpack bucket ++ ".s3.us-west-000.backblazeb2.com/" </> bucketDest
  putStrLn ("![](" ++ url ++ ")")
  pure ()

b2cfg :: ConnectInfo
b2cfg = "https://s3.us-west-000.backblazeb2.com"
