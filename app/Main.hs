module Main where

import FuseBindings
import System.Fuse
import Foreign.C.Error as C
import Control.Exception
import Logging
import Posix.Directory
import System.FilePath

main :: IO ()
main = do
  clearLogFile
  fuseMain (fo (toBacking)) (const $ return C.eOK :: SomeException -> IO C.Errno)

backingPath :: FilePath
backingPath = "/home/david/backing"

toBacking :: FilePath -> FP
toBacking path = FP $ backingPath ++ path
