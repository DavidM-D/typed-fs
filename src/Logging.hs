module Logging (
    clearLogFile
  , traceLog
  , traceItLog
  , logItM
  , traceItLogIO
  ) where

import System.IO.Unsafe
import Foreign.C.Error

logFile :: String
logFile = "/home/david/debug.txt"

traceLog :: (Show b) => b -> a -> a
traceLog s out = unsafePerformIO $ do
  appendFile logFile (show s++"\n")
  return out

traceItLog :: (Show a) => a -> a
traceItLog a = traceLog a a

clearLogFile :: IO ()
clearLogFile = writeFile logFile ""

logItM :: Show a => IO a -> IO a
logItM inp = inp >>= traceItLogIO

instance Show Errno where
  show (Errno a) = show $ fromIntegral a

traceLogIO :: (Show b) => b -> a -> IO a
traceLogIO s out = do
  appendFile logFile (show s++"\n")
  return out

traceItLogIO :: (Show a) => a -> IO a
traceItLogIO a = traceLogIO a a
