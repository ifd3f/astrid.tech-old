module Seams.TestUtils where

import Data.Aeson
import Data.ByteString
import Data.Yaml
import Seams.DB.Models

decodeYamlOrError :: FromJSON a => ByteString -> a
decodeYamlOrError input =
  case decodeEither input of
    Right x -> x
    Left err -> error err

testDir :: FilePath
testDir = "."

exampleDir :: FilePath
exampleDir = "./example"
