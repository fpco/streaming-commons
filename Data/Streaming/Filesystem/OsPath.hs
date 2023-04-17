{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming functions for interacting with the filesystem.

module Data.Streaming.Filesystem.OsPath
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream
  , FileType (..)
  , getFileType
  ) where

import Data.Typeable (Typeable)

import System.Directory.Internal (os)

#if WINDOWS

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory.OsPath (doesFileExist, doesDirectoryExist)
import System.OsPath ((</>))
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import qualified System.Win32.Types as Win32
import qualified System.Win32.WindowsString.File as Win32

data DirStream = DirStream !Win32.HANDLE !Win32.FindData !(IORef Bool)
    deriving Typeable

openDirStream :: OsPath -> IO DirStream
openDirStream fp = do
    (h, fdat) <- Win32.findFirstFile $ getOsString $ fp </> os "*"
    imore <- newIORef True -- always at least two records, "." and ".."
    return $! DirStream h fdat imore

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream h _ _) = Win32.findClose h

readDirStream :: DirStream -> IO (Maybe OsPath)
readDirStream ds@(DirStream h fdat imore) = do
    more <- readIORef imore
    if more
        then do
            filename <- Win32.getFindDataFileName fdat
            Win32.findNextFile h fdat >>= writeIORef imore
            if filename == getOsString (os ".") || filename == getOsString (os "..")
                then readDirStream ds
                else return $ Just $ OsString filename
        else return Nothing

isSymlink :: OsPath -> IO Bool
isSymlink _ = return False

getFileType :: OsPath -> IO FileType
getFileType fp = do
    isFile <- doesFileExist fp
    if isFile
        then return FTFile
        else do
            isDir <- doesDirectoryExist fp
            return $ if isDir then FTDirectory else FTOther

#else

import Control.Exception (try, IOException)
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import System.Posix.Directory.PosixPath (DirStream, openDirStream, closeDirStream)
import qualified System.Posix.Directory.PosixPath as Posix
import qualified System.Posix.Files.PosixString as PosixF

readDirStream :: DirStream -> IO (Maybe OsPath)
readDirStream ds = loop
  where
    loop = do
        fp <- Posix.readDirStream ds
        if fp == mempty
        then return Nothing
        else
            if fp == getOsString (os ".") || fp == getOsString (os "..")
            then loop
            else return $ Just $ OsString fp

getFileType :: OsPath -> IO FileType
getFileType fp = do
    s <- PosixF.getSymbolicLinkStatus $ getOsString fp
    case () of
        ()
            | PosixF.isRegularFile s -> return FTFile
            | PosixF.isDirectory s -> return FTDirectory
            | PosixF.isSymbolicLink s -> do
                es' <- try $ PosixF.getFileStatus $ getOsString fp
                case es' of
                    Left (_ :: IOException) -> return FTOther
                    Right s'
                        | PosixF.isRegularFile s' -> return FTFileSym
                        | PosixF.isDirectory s'   -> return FTDirectorySym
                        | otherwise               -> return FTOther
            | otherwise -> return FTOther

#endif

data FileType
    = FTFile
    | FTFileSym -- ^ symlink to file
    | FTDirectory
    | FTDirectorySym -- ^ symlink to a directory
    | FTOther
    deriving (Show, Read, Eq, Ord, Typeable)

