{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Streaming.Filesystem.OsPathSpec (spec) where

import Control.Exception (bracket)
import Data.List (sort)
import Data.Streaming.Filesystem.OsPath
import System.OsPath (osp)
import Test.Hspec

#if !WINDOWS
import System.Posix.Files (removeLink, createSymbolicLink, createNamedPipe)
import Control.Exception (try, IOException)
#endif

spec :: Spec
spec = describe "Data.Streaming.Filesystem" $ do
    it "dirstream" $ do
        res <- bracket (openDirStream [osp|test/filesystem|]) closeDirStream
            $ \ds -> do
                Just w <- readDirStream ds
                Just x <- readDirStream ds
                Just y <- readDirStream ds
                Just z <- readDirStream ds
                return $ sort [w, x, y, z]
        res `shouldBe` [[osp|bar.txt|], [osp|baz.txt|], [osp|bin|], [osp|foo.txt|]]
    describe "getFileType" $ do
        it "file" $ getFileType [osp|streaming-commons.cabal|] >>= (`shouldBe` FTFile)
        it "dir" $ getFileType [osp|Data|] >>= (`shouldBe` FTDirectory)
#if !WINDOWS
        it "file sym" $ do
            _  <- tryIO $ removeLink "tmp"
            createSymbolicLink "streaming-commons.cabal" "tmp"
            ft <- getFileType [osp|tmp|]
            _  <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTFileSym
        it "file sym" $ do
            _  <- tryIO $ removeLink "tmp"
            createSymbolicLink "Data" "tmp"
            ft <- getFileType [osp|tmp|]
            _  <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTDirectorySym
        it "other" $ do
            _ <- tryIO $ removeLink "tmp"
            e <- tryIO $ createNamedPipe "tmp" 0
            case e of
                -- Creating named pipe might fail on some filesystems
                Left _ -> return ()
                Right _ -> do
                    ft <- getFileType [osp|tmp|]
                    _  <- tryIO $ removeLink "tmp"
                    ft `shouldBe` FTOther
        it "recursive symlink is other" $ do
            _ <- tryIO $ removeLink "tmp"
            createSymbolicLink "tmp" "tmp"
            ft <- getFileType [osp|tmp|]
            _  <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTOther
        it "dangling symlink is other" $ do
            _  <- tryIO $ removeLink "tmp"
            createSymbolicLink "doesnotexist" "tmp"
            ft <- getFileType [osp|tmp|]
            _  <- tryIO $ removeLink "tmp"
            ft `shouldBe` FTOther

tryIO :: IO a -> IO (Either IOException a)
tryIO = try
#endif
