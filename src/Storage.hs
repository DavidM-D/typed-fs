module Storage () where

import qualified Data.ByteString as BS

newtype FileHandle = FileHandle {handleToString :: String}

newtype Tag = Tag {unTag :: String}



-- | SafeIO is IO where it is unable to observe the effect of any of it's operations
--   the benefit of this is that many SafeIO operations can be run in parallel

class Storage a where
  toTags       :: a -> FileHandle -> IO [Tag]
  addTag       :: a -> FileHandle -> Tag -> IO ()
  fromTags     :: a -> [Tag] -> IO [FileHandle]
  getContents  :: a -> FileHandle -> IO BS.ByteString
