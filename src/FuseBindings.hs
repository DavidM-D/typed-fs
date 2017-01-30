{-# LANGUAGE StandaloneDeriving #-}
module FuseBindings (
  fo
          ) where
import System.Fuse
--import Storage
import Foreign.C.Error as C
import Logging (traceLog)
import System.Posix.Files
import System.Posix.Directory
import qualified System.FilePath.Posix as Path
import CInterop

fo :: FuseOperations fs
fo = FuseOperations {
      fuseGetFileStat = traceLog "fuseGetFileStat"
        $ getFileStat -- FilePath -> IO (Either Errno FileStat),
    , fuseReadSymbolicLink = traceLog "fuseReadSymbolicLink" $ fmap Right . readSymbolicLink -- FilePath -> IO (Either Errno FilePath),
    , fuseAccess = traceLog "fuseAccess" $ const $ const $ return C.eOK
    , fuseCreateDevice = traceLog "fuseCreateDevice" undefined -- FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno,
    , fuseCreateDirectory = traceLog "fuseCreateDirectory" undefined -- FilePath -> FileMode -> IO
    , fuseRemoveLink = traceLog "fuseRemoveLink" undefined -- FilePath -> IO Errno,
    , fuseRemoveDirectory = traceLog "fuseRemoveDirectory" undefined -- FilePath -> IO Errno,
    , fuseCreateSymbolicLink = traceLog "fuseCreateSymbolicLink" undefined -- FilePath -> FilePath -> IO Errno,
    , fuseRename = traceLog "fuseRename" undefined -- FilePath -> FilePath -> IO Errno,
    , fuseCreateLink = traceLog "fuseCreateLink" undefined -- FilePath -> FilePath -> IO Errno,
    , fuseSetFileMode = traceLog "fuseSetFileMode" undefined -- FilePath -> FileMode -> IO Errno,
    , fuseSetOwnerAndGroup = traceLog "fuseSetOwnerAndGroup" undefined -- FilePath -> UserID -> GroupID -> IO Errno,
    , fuseSetFileSize = traceLog "fuseSetFileSize" undefined -- FilePath -> FileOffset -> IO Errno,
    , fuseSetFileTimes = traceLog "fuseSetFileTimes" undefined -- FilePath -> EpochTime -> EpochTime -> IO Errno,
    , fuseOpen = traceLog "fuseOpen" undefined -- FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh),
    , fuseRead = traceLog "fuseRead" undefined -- FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString),
    , fuseWrite = traceLog "fuseWrite" undefined -- FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount),
    , fuseGetFileSystemStats = traceLog "fuseGetFileSystemStats" undefined -- String -> IO (Either Errno FileSystemStats),
    , fuseFlush = traceLog "fuseFlush" undefined -- FilePath -> fh -> IO Errno,
    , fuseRelease = traceLog "fuseRelease" $ const $ const $ return () -- FilePath -> fh -> IO (),
    , fuseSynchronizeFile = traceLog "fuseSynchronizeFile" undefined -- FilePath -> SyncType -> IO Errno,
    , fuseOpenDirectory = const $ return (traceLog ("fuseOpenDirectory") $ C.eOK) -- FilePath -> IO Errno,
    , fuseReadDirectory = traceLog "fuseReadDirectory" undefined -- FilePath -> IO (Either Errno [(FilePath, FileStat)]),
    , fuseReleaseDirectory = traceLog "fuseReleaseDirectory" undefined -- FilePath -> IO Errno,
    , fuseSynchronizeDirectory = traceLog "fuseSynchronizeDirectory" undefined -- FilePath -> SyncType -> IO Errno,
    , fuseInit = traceLog "fuseInit" $ return () -- IO (),
    , fuseDestroy = traceLog "fuseDestroy" $ return () -- IO ()
    }

backingPath :: FilePath
backingPath = "/home/david/backing"


getFileStat :: FilePath -> IO (Either C.Errno FileStat)
getFileStat fp = do
  status <- getFileStatus $ traceLog path path
  stat <- return $ Right $ fileStat status
  return $ traceLog "fileStat" $ stat
  where path = backingPath ++ fp
