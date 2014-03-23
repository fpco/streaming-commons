{-# LANGUAGE CPP #-}
-- | Streaming functions for interacting with the filesystem.
module Data.Streaming.Filesystem
    ( DirStream
    , openDirStream
    , readDirStream
    , closeDirStream
    ) where

#if WINDOWS

import qualified System.Win32 as Win32
import System.FilePath ((</>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data DirStream = DirStream !Win32.HANDLE !Win32.FindData !(IORef Bool)

openDirStream :: FilePath -> IO DirStream
openDirStream fp = do
    (h, fdat) <- Win32.findFirstFile $ fp </> "*"
    imore <- newIORef True -- always at least two records, "." and ".."
    return $! DirStream h fdat imore

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream h _ _) = Win32.findClose h

readDirStream :: DirStream -> IO (Maybe FilePath)
readDirStream ds@(DirStream h fdat imore) = do
    more <- readIORef imore
    if more
        then do
            filename <- Win32.getFindDataFileName fdat
            Win32.findNextFile h fdat >>= writeIORef imore
            if filename == "." || filename == ".."
                then readDirStream ds
                else return $ Just filename
        else return Nothing

#else

import System.Posix.Directory (DirStream, openDirStream, closeDirStream)
import qualified System.Posix.Directory as Posix

readDirStream :: DirStream -> IO (Maybe FilePath)
readDirStream ds = do
    fp <- Posix.readDirStream ds
    case fp of
        "" -> return Nothing
        "." -> readDirStream ds
        ".." -> readDirStream ds
        _ -> return $ Just fp

#endif
