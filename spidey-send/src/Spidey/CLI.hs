module Spidey.CLI where

import Data.Text
import Spidey.Crawl

data Params = Params
  { pages :: [Text],
    depth :: Int,
    autoconfirm :: Bool
  }

run :: Params -> IO ()
run Params {pages} = do
  -- pages <- crawl pages
  -- sendData <- readSendData
  -- let toSend = calculate pages sendData
  -- shouldSend <- confirm toSend
  -- if shouldSend then
  --   result <- mapConcurrently sendMentions pages
  --   _ <- save result
  --   return ()
  -- else
  --   return ()
  return ()
