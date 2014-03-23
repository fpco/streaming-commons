module Data.Streaming.FilesystemSpec (spec) where

import Test.Hspec
import Data.Streaming.Filesystem
import Control.Exception (bracket)
import Data.List (sort)

spec :: Spec
spec = describe "Data.Streaming.Filesystem" $ do
    it "works" $ do
        res <- bracket (openDirStream "test/filesystem") closeDirStream
            $ \ds -> do
                Just w <- readDirStream ds
                Just x <- readDirStream ds
                Just y <- readDirStream ds
                Just z <- readDirStream ds
                return $ sort [w, x, y, z]
        res `shouldBe` ["bar.txt", "baz.txt", "bin", "foo.txt"]
