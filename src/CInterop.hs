{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module CInterop (
    fileStat
  , FileStat

  , FileError(..)
  , toCErr

  , PosixOp
  , PosixAssert

  , liftBool
  , unLift
  , liftCatch
  , getErr

  , FP(..)
  , Permission(..)
  ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Posix.Files
import System.Fuse
import Foreign.C.Error
import Control.Exception
import Foreign.C.Types
import Data.Bifunctor
import Logging

fileStat :: FileStatus -> FileStat
fileStat fs = FileStat
  {
      statEntryType = getEntryType fs
    , statFileMode = fileMode fs
    , statLinkCount = linkCount fs
    , statFileOwner = fileOwner fs
    , statFileGroup = fileGroup fs
    , statSpecialDeviceID = specialDeviceID fs
    , statFileSize = fileSize fs
    -- is this always true
    , statBlocks = 512
    , statAccessTime = accessTime fs
    , statModificationTime = modificationTime fs
    , statStatusChangeTime = statusChangeTime fs
  }

getEntryType :: FileStatus -> EntryType
getEntryType fs
  | isNamedPipe fs       = NamedPipe
  | isCharacterDevice fs = CharacterSpecial
  | isBlockDevice fs     = BlockSpecial
  | isRegularFile fs     = RegularFile
  | isDirectory fs       = Directory
  | isSymbolicLink fs    = SymbolicLink
  | isSocket fs          = Socket
  | otherwise            = Unknown


-- | a many to one mapping between descriptive errors and C error numbers
data FileError =
    NoSuchFile FP
  | NotAFile FP
  | NotADirectory FP
  -- should probably include the error message for logs
  | UnknownError String
  -- I'd like to add a bit more detail about the missing permission and the source
  | MissingPermission FP [Permission]
  | AllreadyExists FP
  deriving Show

-- | POSIX be damned, most programs just output 1 if something goes wrong
generalError :: Errno
generalError = Errno $ CInt 1

-- | tranforms into C error and logs the error
toCErr :: FileError -> Errno
toCErr er = traceLog er $ case er of
  NoSuchFile    _ -> eNFILE
  NotADirectory _ -> eNOTDIR
  _               -> generalError

-- | probably worth putting this somewhere else

type PosixOp a = ExceptT FileError IO a

type PosixAssert = PosixOp ()

getErr :: PosixAssert -> IO Errno
getErr fo = f <$> runExceptT fo
  where f (Left err) = toCErr err
        f (Right ()) = eOK

unLift :: PosixOp a -> IO (Either Errno a)
unLift = fmap (first toCErr) . runExceptT


-- | if true the assertation passes, if false it fails with the inputted error
liftBool :: FileError -> IO Bool -> PosixAssert
liftBool fe p = lift p >>= f
  where f True  = lift $ return ()
        f False = throwE fe

-- | there's about a million ways for POSIX to throw exceptions
--   this does the very lazy thing of catching all of them
--   TODO: make the errors more specific and don't catch everything
liftCatch :: forall a . IO a -> PosixOp a
liftCatch a =
  ExceptT $ first (UnknownError . displayException) <$> (try a :: IO (Either IOError a))

newtype FP = FP {unFP :: FilePath}
  deriving (Eq, Show)

data Permission = Read | Write | Execute
  deriving (Show, Eq)
