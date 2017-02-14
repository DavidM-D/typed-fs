{-# LANGUAGE ScopedTypeVariables #-}
-- | a safer and simpler wrapper around the posix directory functions
--   less efficient than just using System.Posix.Directory directly as it
--   often makes multiple posix calls to make error handling simpler
--   posix is needlessly complex so some errors may still float through
module Posix.Directory (
    module D
  , dirExist
  , fileExist
  , getFileStat
  , FP(..)
  , Permission(..)
  , hasPermission

  , getFileStatus
  , rename
  , removeLink
  , readSymbolicLink
  , removeDirectory
  , createDirectory
  , createDevice
  , createLink
  , setFileMode
  , setOwnerAndGroup
  , setFileSize
  , setFileTimes
  , open

  , fdRead
  , readDirectory
  , fdWrite

  , closeFd
  ) where

import qualified System.Posix.Directory as D hiding (openDirStream)
import qualified System.Posix.Files     as F
import qualified System.FilePath        as Path
import qualified System.Directory       as SD
import qualified System.Posix.Types     as T
import qualified System.Posix.IO        as IO
import qualified System.Posix.IO.ByteString as BIO
import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as BSUTF
import qualified Data.Word              as Word
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import CInterop
import Logging
import Utility

mapFP :: (FilePath -> FilePath) -> FP -> FP
mapFP f = FP . f . unFP

fileExist :: FP -> PosixAssert
fileExist fp =
    liftBool (NoSuchFile fp)
  $ F.fileExist $ unFP fp

dirExist :: FP -> PosixAssert
dirExist fp = do
  fileExist fp
  -- if I get round to batching F'S operations this should be done via fileStat calls
  liftBool (NotADirectory fp) $ SD.doesDirectoryExist $ unFP fp

getFileStatus :: FP -> PosixOp F.FileStatus
getFileStatus fp = do
  fileExist fp
  lift $ F.getFileStatus $ unFP fp

getFileStat :: FP -> PosixOp FileStat
getFileStat = fileStat <.> getFileStatus . traceItLog

rename :: FP -> FP -> PosixAssert
rename fa fb = do
  fileExist fa
  canModifyParent fa
  canModifyParent fb
  lift $ F.rename (unFP fa) (unFP fb)

-- | TODO find out if I need to remove the orphans if there are 0 links remaining
removeLink :: FP -> PosixAssert
removeLink fp = do
  fileExist fp
  canModifyParent fp
  lift $ F.removeLink $ unFP fp

removeDirectory :: FP -> PosixAssert
removeDirectory fp = do
  canModifyParent fp
  lift $ D.removeDirectory $ unFP fp

createDevice :: FP -> T.FileMode -> T.DeviceID -> PosixAssert
createDevice fp mode deviceID = do
  canModifyParent fp
  lift $ F.createDevice (unFP fp) mode deviceID

createDirectory :: FP -> T.FileMode -> PosixAssert
createDirectory fp fileM = do
  canModifyParent fp
  lift $ D.createDirectory (unFP fp) fileM

-- | TODO: work out what permissions this is reliant on
readSymbolicLink :: FP -> PosixOp FilePath
readSymbolicLink fp = do
  lift $ F.readSymbolicLink $ unFP fp

-- | Also asserts whether the file exists
hasPermission :: [Permission] -> FP -> PosixAssert
hasPermission pms fp = do
  fileExist fp
  liftBool (MissingPermission fp pms) $ uncurry3 (F.fileAccess $ unFP fp) args
  where toBool :: Permission -> (Bool, Bool, Bool) -> (Bool, Bool, Bool)
        toBool pm (r,w,e) = case pm of
          Read    -> (True, w, e)
          Write   -> (r, True, e)
          Execute -> (r, w, True)
        args :: (Bool, Bool, Bool)
        args = foldr toBool (False, False, False) pms

parent :: FP -> FP
parent = mapFP $ Path.takeDirectory

canModifyParent :: FP -> PosixAssert
canModifyParent = hasPermission [Write, Execute] . parent

createLink :: FP -> FP -> PosixAssert
createLink fa fb = do
  fileExist fa
  canModifyParent fb
  lift $ F.createLink (unFP fa) (unFP fb)

setFileMode :: FP -> T.FileMode -> PosixAssert
setFileMode fp mode = do
  hasPermission [Write] fp
  lift $ F.setFileMode (unFP fp) mode

setOwnerAndGroup :: FP -> T.UserID -> T.GroupID -> PosixAssert
setOwnerAndGroup fp user group = do
  hasPermission [Write] fp
  lift $ F.setOwnerAndGroup (unFP fp) user group

setFileSize :: FP -> T.FileOffset -> PosixAssert
setFileSize fp offset = do
  hasPermission [Write] fp
  lift $ F.setFileSize (unFP fp) offset

setFileTimes :: FP -> T.EpochTime -> T.EpochTime -> PosixAssert
setFileTimes fp aTime mTime = do
  fileExist fp
  lift $ F.setFileTimes (unFP fp) aTime mTime

-- | unlike openFD, this will not open a file that doesn't exist
open :: FP -> IO.OpenMode -> IO.OpenFileFlags -> PosixOp T.Fd
open fp mode flags = do
  flip hasPermission fp $
    case mode of
      IO.ReadOnly  -> [Read]
      IO.WriteOnly -> [Write]
      IO.ReadWrite -> [Read, Write]
  liftCatch $ IO.openFd (unFP fp) mode Nothing flags

-- | TODO: catch error when end of file is reached within the bytecount
--   because of changes in the haskell unix API there is no longer offset
fdRead :: FP -> T.Fd -> T.ByteCount -> T.FileOffset -> PosixOp BS.ByteString
fdRead fp fh count offset = do
  hasPermission [Read] fp
  (str , readBytes) <- liftCatch $ BIO.fdRead fh $ fromIntegral toBeRead
  let bs = BSUTF.fromString str
      (hRead :: Int) = fromIntegral readBytes
      (trailingZeros :: BS.ByteString) = BS.replicate (toBeRead - hRead) 0
  return $ BS.append bs trailingZeros
  where (toBeRead :: Int) = hCount + hOffset
        hCount  = fromIntegral toBeRead
        hOffset = fromIntegral offset

-- | this doesn't cope with offset, I know reading then writing is wrong
--   tests will clarify this
fdWrite :: FP -> T.Fd -> BS.ByteString -> T.FileOffset -> PosixOp T.ByteCount
fdWrite fp fh content _offset = do
  hasPermission [Write] fp
  liftCatch $ BIO.fdWrite fh $ BSUTF.toString content

closeFd :: T.Fd -> IO ()
closeFd = IO.closeFd

readDirectory :: FilePath -> (FilePath -> FP) -> PosixOp [(FilePath, FileStat)]
readDirectory path toFP = do
  dirExist fp
  hasPermission [Read] fp
  files <- lift $ SD.listDirectory (unFP fp)
  -- we can justify ignoring the path mapping function here because all listed
  -- files exist in this directory
  mapM (\p -> fmap ((,) p) $ getFileStat $ toFP p)
    $ map (\f -> Path.joinPath [path, f]) files
  where fp = toFP path
