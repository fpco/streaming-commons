{-# LANGUAGE CPP #-}
module Data.Streaming.FilesystemSpec (spec) where

import Test.Hspec
import Data.Streaming.Filesystem
import Control.Exception (bracket)
import Data.List (sort)
#if !WINDOWS
import System.Posix.Files (removeLink, createSymbolicLink, createNamedPipe)
import Control.Exception (try, IOException)
#endif

spec :: Spec
spec = describe "Data.Streaming.Filesystem" $ do
    it "dirstream" $ do
        res <- bracket (openDirStream "test/filesystem") closeDirStream
            $ \ds -> do
                Just w <- readDirStream ds
                Just x <- readDirStream ds
                Just y <- readDirStream ds
                Just z <- readDirStream ds
                return $ sort [w, x, y, z]
        res `shouldBe` ["bar.txt", "baz.txt", "bin", "foo.txt"]
    describe "getFileType" $ do
        it "file" $ getFileType "streaming-commons.cabal" >>= (`shouldBe` FTFile)
        it "dir" $ getFileType "Data" >>= (`shouldBe` FTDirectory)
#if !WINDOWS
        it "file sym" $ do
            _ <- tryIO $ removeLink "tmp"
            createSymbolicLink "streaming-commons.cabal" "tmp"
            ft <- getFileType "tmp"
            _ <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTFileSym
        it "file sym" $ do
            _ <- tryIO $ removeLink "tmp"
            createSymbolicLink "Data" "tmp"
            ft <- getFileType "tmp"
            _ <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTDirectorySym
        it "other" $ do
            _ <- tryIO $ removeLink "tmp"
            e <- tryIO $ createNamedPipe "tmp" 0
            case e of
                -- Creating named pipe might fail on some filesystems
                Left _ -> return ()
                Right _ -> do
                    ft <- getFileType "tmp"
                    _ <- tryIO $ removeLink "tmp"
                    ft `shouldBe` FTOther
        it "recursive symlink is other" $ do
            _ <- tryIO $ removeLink "tmp"
            createSymbolicLink "tmp" "tmp"
            ft <- getFileType "tmp"
            _ <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTOther
        it "dangling symlink is other" $ do
            _ <- tryIO $ removeLink "tmp"
            createSymbolicLink "doesnotexist" "tmp"
            ft <- getFileType "tmp"
            _ <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTOther

tryIO :: IO a -> IO (Either IOException a)
tryIO = try
#endif
