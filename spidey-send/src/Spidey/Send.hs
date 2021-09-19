module Spidey.Send where

import Data.ByteString
import Data.Text

data SendResult = Success | Fail {sendAttempts :: Int}

data SendState = SendState
  { withMF2Hash :: ByteString,
    result :: SendResult,
    time :: Integer
  }

sendMentions :: Text -> ByteString -> Text -> IO SendState
sendMentions = undefined
