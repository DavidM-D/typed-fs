
module FuseBindings (
  fo
          ) where
import System.Fuse
--import Storage
import Foreign.C.Error as C
import Debug.Trace
import System.IO.Unsafe
import Data.Monoid
import Data.Default

fo :: FuseOperations fs
fo = clearFile $ FuseOperations {
      fuseGetFileStat = const (return de) -- FilePath -> IO (Either Errno FileStat),
    , fuseReadSymbolicLink = printFile "M34ade it!" undefined -- FilePath -> IO (Either Errno FilePath),
    , fuseAccess = printFile "Made 54it!" undefined
    , fuseCreateDevice = printFile "Ma65de it!" undefined -- FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno,
    , fuseCreateDirectory = printFile "Ma6fde it!" undefined -- FilePath -> FileMode -> IO
    , fuseRemoveLink = printFile "Made dsit!" undefined -- FilePath -> IO Errno,
    , fuseRemoveDirectory = printFile "Madsdde it!" undefined -- FilePath -> IO Errno,
    , fuseCreateSymbolicLink = printFile "Madsdde it!" undefined -- FilePath -> FilePath -> IO Errno,
    , fuseRename = printFile "Madedsasv it!" undefined -- FilePath -> FilePath -> IO Errno,
    , fuseCreateLink = printFile "Made vdcit!" undefined -- FilePath -> FilePath -> IO Errno,
    , fuseSetFileMode = printFile "Made icat!" undefined -- FilePath -> FileMode -> IO Errno,
    , fuseSetOwnerAndGroup = printFile "Madyee it!" undefined -- FilePath -> UserID -> GroupID -> IO Errno,
    , fuseSetFileSize = printFile "Made evit!" undefined -- FilePath -> FileOffset -> IO Errno,
    , fuseSetFileTimes = printFile "Made icdt!" undefined -- FilePath -> EpochTime -> EpochTime -> IO Errno,
    , fuseOpen = printFile "Made hf vit!" undefined -- FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh),
    , fuseRead = printFile "Made idvdct!" undefined -- FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString),
    , fuseWrite = printFile "Made icd t!" undefined -- FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount),
    , fuseGetFileSystemStats = printFile "Mgdade it!" undefined -- String -> IO (Either Errno FileSystemStats),
    , fuseFlush = printFile "Madgasdfe it!" undefined -- FilePath -> fh -> IO Errno,
    , fuseRelease = printFile "Made ifaeft!" undefined -- FilePath -> fh -> IO (),
    , fuseSynchronizeFile = printFile "Madsfade it!" undefined -- FilePath -> SyncType -> IO Errno,
    , fuseOpenDirectory = const $ return (printFile ("SUP!") $ C.eOK) -- FilePath -> IO Errno,
    , fuseReadDirectory = printFile "Made fsdit!" undefined -- FilePath -> IO (Either Errno [(FilePath, FileStat)]),
    , fuseReleaseDirectory = printFile "Made ewsit!" undefined -- FilePath -> IO Errno,
    , fuseSynchronizeDirectory = printFile "Madebf it!" undefined -- FilePath -> SyncType -> IO Errno,
    , fuseInit = printFile "Madecds it!" undefined -- IO (),
    , fuseDestroy = printFile "Madeagds it!" undefined -- IO ()
    }

printFile :: String -> a -> a
printFile s out = unsafePerformIO $ do
  appendFile "/home/david/debug.txt" (s++"\n")
  return out

clearFile :: a -> a
clearFile out = unsafePerformIO $ do
  writeFile "/home/david/debug.txt" ""
  return out
