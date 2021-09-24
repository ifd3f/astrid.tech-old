module Main where

import Codec.Compression.Zlib (compress, decompress)
import Prelude ((.), putStrLn)

main = putStrLn "Hello from rules_haskell!"

slowId = decompress . compress
