module Main where

import FuseBindings
import System.Fuse
import Foreign.C.Error as C
import Control.Exception

newtype FileKey = FileKey ()

main :: IO ()
main = fuseMain (fo :: FuseOperations FileKey) (const $ return C.eOK :: SomeException -> IO C.Errno)
