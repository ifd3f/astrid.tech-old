module Astrid.Tech.InputSchema.Util
  ( FileAccessor,
    readDirectoryFA,
    readDirectoryBS,
    readDirTreeBS,
  )
where

import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import qualified System.Directory.Tree as DT

type FileAccessor = IO BS.ByteString

fileAccessor :: FilePath -> IO (IO BS.ByteString)
fileAccessor path = pure $ BS.readFile path

readDirectoryFA :: FilePath -> IO (DT.AnchoredDirTree FileAccessor)
readDirectoryFA = DT.readDirectoryWith fileAccessor

readDirectoryBS :: FilePath -> IO (DT.AnchoredDirTree BS.ByteString)
readDirectoryBS = DT.readDirectoryWith BS.readFile

readDirTreeBS :: FilePath -> IO (DT.DirTree BS.ByteString)
readDirTreeBS path = readDirectoryBS path <&> DT.dirTree
