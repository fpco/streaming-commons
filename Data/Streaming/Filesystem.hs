{-# LANGUAGE CPP #-}
-- | Streaming functions for interacting with the filesystem.
module Data.Streaming.Filesystem
    ( DirStream
    , openDirStream
    , readDirStream
    , closeDirStream
    ) where

#if WINDOWS
#error "Need to write Windows code"

openDirStream :: FilePath -> IO DirStream
openDirStream = undefined

closeDirStream :: DirStream -> IO ()
closeDirStream = undefined

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
