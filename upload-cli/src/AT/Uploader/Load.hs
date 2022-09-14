{-# LANGUAGE ScopedTypeVariables #-}

module AT.Uploader.Load where

import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T

getHash :: FilePath -> IO String
getHash path = do
  fileData <- BL.readFile path
  let hash = hashlazy fileData
  let b64 :: Text = encodeBase64 hash
  pure $ T.unpack b64
