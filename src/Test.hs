module Spec (main) where

import Test.Hspec
import Test.QuickCheck
import System.Process
import GHC.IO.Exception
import System.Directory

main :: IO ()
main = hspec $ do
  describe "Setup env" $ do

    it "successfully mounts" $ do
      envReturn <- setupEnv
      mapM_ (`shouldBe` ExitSuccess) envReturn

    it "can cd into the directory" $ do
      mnt <- toMount
      mnt `shouldBe` ExitSuccess

  describe "The file access" $ do

    it "correctly shows an empty directory" $
      getDirectoryContents mountPath >>= (`shouldBe` [])

    it "correctly shows a directory with one file" $ do
      createFile "test"
      getDirectoryContents mountPath >>= (`shouldBe` ["test"])

  describe "prints out the log file" $ do
    it "a" $ do
      file <- readFile "/home/david/debug.txt"
      v <- putStrLn file
      v `shouldBe` ()


mountPath :: String
mountPath = "/home/david/mounted"

backingPath :: String
backingPath = "~/backing"

setupEnv :: IO [ExitCode]
setupEnv =
  mapM system [ "sudo umount ~/mounted"
              , "stack build"
              , "stack exec FuseSys-exe ~/mounted -- -o default_permissions"
              ]

  -- mount fuse partition



toMount :: IO ExitCode
toMount = system ("cd " ++ mountPath)

toBacking :: IO ExitCode
toBacking = system $ "cd " ++ backingPath

createFile :: FilePath -> IO ExitCode
createFile path = do
  toBacking
  system $ "touch " ++ path
  toMount
