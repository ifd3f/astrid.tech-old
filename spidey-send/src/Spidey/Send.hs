module Spidey.Send where

import Data.ByteString
import Data.Text

data SendStatus = Success | Fail {sendAttempts :: Int}

data SendResult = SendResult
  { withMF2Hash :: ByteString,
    result :: SendStatus,
    time :: Integer
  }

sendMentions :: Text -> ByteString -> Text -> IO SendResult
sendMentions = undefined
