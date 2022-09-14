{-# LANGUAGE ScopedTypeVariables #-}

module AT.Uploader.Load where

import Crypto.Hash.SHA256 (hashlazy)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getHash :: FilePath -> IO String
getHash path = do
  fileData <- BL.readFile path
  let hash = hashlazy fileData
  let ascii = B16.encode hash
  pure $ T.unpack $ T.decodeUtf8 $ ascii
