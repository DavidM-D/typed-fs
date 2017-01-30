module CInterop (fileStat) where
import System.Posix.Files
import System.Fuse

fileStat :: FileStatus -> FileStat
fileStat fs = FileStat
  {
      statEntryType = Directory
    , statFileMode = fileMode fs
    , statLinkCount = linkCount fs
    , statFileOwner = fileOwner fs
    , statFileGroup = fileGroup fs
    , statSpecialDeviceID = specialDeviceID fs
    , statFileSize = fileSize fs
    , statBlocks = 512
    , statAccessTime = accessTime fs
    , statModificationTime = modificationTime fs
    , statStatusChangeTime = statusChangeTime fs
  }

