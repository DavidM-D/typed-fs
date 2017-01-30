module Logging (
    clearLogFile
  , traceLog
  ) where

import System.IO.Unsafe

logFile :: String
logFile = "/home/david/debug.txt"

traceLog :: (Show b) => b -> a -> a
traceLog s out = unsafePerformIO $ do
  appendFile logFile (show s++"\n")
  return out

clearLogFile :: IO ()
clearLogFile = writeFile logFile ""
