{-# LANGUAGE StandaloneDeriving #-}
module FuseBindings (
  fo
  ) where
import System.Fuse
--import Storage
import Foreign.C.Error as C
import Logging
import qualified System.Posix.Files as F
import qualified System.Posix.Directory as D
import qualified System.FilePath.Posix as P
import qualified Posix.Directory as FS
import CInterop
import qualified System.Posix.Types     as T

fo :: (FilePath -> FS.FP) -> FuseOperations T.Fd
fo toFP = FuseOperations {
      -- FilePath -> IO (Either Errno FileStat)
      fuseGetFileStat =
          traceLog "fuseGetFileStat"
        . unLift . FS.getFileStat . toFP

      -- FilePath -> IO (Either Errno FilePath),
    , fuseReadSymbolicLink =
          traceLog "fuseReadSymbolicLink"
        . unLift . FS.readSymbolicLink . toFP

    , fuseAccess =
          traceLog "fuseAccess"
        $ const $ const $ return C.eOK

      -- FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno,
    , fuseCreateDevice = \path _ mode device ->
          traceLog "fuseCreateDevice"
        $ getErr $ FS.createDevice (toFP path) mode device

      -- FilePath -> FileMode -> IO
    , fuseCreateDirectory = \path mode -> 
          traceLog "fuseCreateDirectory"
        $ getErr $ FS.createDirectory (toFP path) mode

      -- FilePath -> IOglobal Errno
    , fuseRemoveLink =
          traceLog "fuseRemoveLink"
        . getErr . FS.removeLink . toFP

    -- FilePath -> IO Errno
    , fuseRemoveDirectory =
          traceLog "fuseRemoveDirectory"
        . getErr . FS.removeDirectory . toFP

    -- FilePath -> FilePath -> IO Errno,
    , fuseCreateSymbolicLink =
      traceLog "fuseCreateSymbolicLink" undefined

      -- FilePath -> FilePath -> IO Errno,
    , fuseRename = \fa fb ->
         traceLog "fuseRename"
       $ getErr $ FS.rename (toFP fa) (toFP fb)

    -- FilePath -> FilePath -> IO Errno,
    , fuseCreateLink = \pathA pathB -> 
      traceLog "fuseCreateLink"
      $ getErr $ FS.createLink (toFP pathA) (toFP pathB)

    -- FilePath -> FileMode -> IO Errno,
    , fuseSetFileMode = \path mode ->
      traceLog "fuseSetFileMode"
      $ getErr $ FS.setFileMode (toFP path) mode

    -- FilePath -> UserID -> GroupID -> IO Errno,
    , fuseSetOwnerAndGroup = \path userID groupID -> 
      traceLog "fuseSetOwnerAndGroup"
      $ getErr $ FS.setOwnerAndGroup (toFP path) userID groupID

    -- FilePath -> FileOffset -> IO Errno,
    , fuseSetFileSize = \path offset ->
      traceLog "fuseSetFileSize"
      $ getErr $ FS.setFileSize (toFP path) offset

    -- FilePath -> EpochTime -> EpochTime -> IO Errno,
    , fuseSetFileTimes = \path aTime mTime ->
      traceLog "fuseSetFileTimes"
      $ getErr $ FS.setFileTimes (toFP path) aTime mTime

    -- FilePath -> OpenMode  -> OpenFileFlags -> IO (Either Errno fh),
    , fuseOpen = \path mode flags -> 
      traceLog "fuseOpen"
      $ unLift $ FS.open (toFP path) mode flags

    -- FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString),
    , fuseRead = \path fh count offset -> 
      traceLog "fuseRead"
      $ unLift $ FS.fdRead (toFP path) fh count offset

    -- FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount),
    , fuseWrite = \path fh content offset -> 
      traceLog "fuseWrite"
      $ unLift $ FS.fdWrite (toFP path) fh content offset

    , fuseGetFileSystemStats =
      -- FilePath -> IO (Either Errno FileSystemStats),
        traceLog "fuseGetFileSystemStats"
          $ undefined

    -- FilePath -> fh -> IO Errno,
    , fuseFlush = \_path _fh ->
         traceLog "fuseFlush"
         $ return eOK

    -- FilePath -> fh -> IO (),
    , fuseRelease = traceLog "fuseRelease"
                  $ const $ FS.closeFd

    -- FilePath -> SyncType -> IO Errno,
    , fuseSynchronizeFile =
         traceLog "fuseSynchronizeFile"
         undefined

    -- FilePath -> IO Errno,
    , fuseOpenDirectory =
         traceLog ("fuseOpenDirectory")
       . getErr . FS.dirExist . toFP

    -- FilePath -> IO (Either Errno [(FilePath, FileStat)]),
    , fuseReadDirectory = \path ->
        traceLog "fuseReadDirectory"
        unLift $ FS.readDirectory path toFP

    , fuseReleaseDirectory = \_path ->
          traceLog "fuseReleaseDirectory"
        $ return eOK -- FilePath -> IO Errno,

    , fuseSynchronizeDirectory =
          traceLog "fuseSynchronizeDirectory"
          undefined -- FilePath -> SyncType -> IO Errno,

    , fuseInit =
          traceLog "fuseInit"
        $ return () -- IO (),

    , fuseDestroy =
          traceLog "fuseDestroy"
        $ return () -- IO ()
    }
