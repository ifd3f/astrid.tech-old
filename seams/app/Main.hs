module Main where

import Seams.CLI

main :: IO ()
main = execParser parser >>= execute

