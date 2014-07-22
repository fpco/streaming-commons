{-# LANGUAGE CPP #-}
-- | A full tutorial for this module is available on FP School of Haskell:
-- <https://www.fpcomplete.com/user/snoyberg/library-documentation/data-conduit-process>.
--
-- Note that, while the tutorial covers @Data.Streaming.Process@, this module is
-- the basis of the streaming version, and almost all concepts there apply here.
module Data.Streaming.Process
    ( -- * Functions
      streamingProcess
      -- * Specialized streaming types
    , Inherited (..)
    , ClosedStream (..)
    , UseProvidedHandle (..)
      -- * Process handle
    , StreamingProcessHandle
    , waitForStreamingProcess
    , waitForStreamingProcessSTM
    , getStreamingProcessExitCode
    , getStreamingProcessExitCodeSTM
    , streamingProcessHandleRaw
    , streamingProcessHandleTMVar
      -- * Type classes
    , InputSource
    , OutputSink
      -- * Reexport
    , module System.Process
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Concurrent              (forkIO)
import           Control.Concurrent.STM          (STM, TMVar, atomically,
                                                  newEmptyTMVar, putTMVar,
                                                  readTMVar)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Maybe                      (fromMaybe)
import           Data.Streaming.Process.Internal
import           System.Exit                     (ExitCode)
import           System.IO                       (hClose)
import           System.Process

#if MIN_VERSION_stm(2,3,0)
import           Control.Concurrent.STM          (tryReadTMVar)
#else
import           Control.Concurrent.STM          (tryTakeTMVar, putTMVar)

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar var = do
    mx <- tryTakeTMVar var
    case mx of
        Nothing -> return ()
        Just x -> putTMVar var x
    return mx
#endif

-- | Use the @Handle@ provided by the @CreateProcess@ value. This would allow
-- you, for example, to open up a @Handle@ to a file, set it as @std_out@, and
-- avoid any additional overhead of dealing with providing that data to your
-- process.
--
-- Since 0.1.4
data UseProvidedHandle = UseProvidedHandle

-- | Inherit the stream from the current process.
--
-- Since 0.1.4
data Inherited = Inherited

-- | Close the stream with the child process.
--
-- Since 0.1.4
data ClosedStream = ClosedStream

instance InputSource ClosedStream where
    isStdStream = (\(Just h) -> hClose h >> return ClosedStream, Just CreatePipe)
instance InputSource Inherited where
    isStdStream = (\Nothing -> return Inherited, Just Inherit)
instance InputSource UseProvidedHandle where
    isStdStream = (\Nothing -> return UseProvidedHandle, Nothing)

instance OutputSink ClosedStream where
    osStdStream = (\(Just h) -> hClose h >> return ClosedStream, Just CreatePipe)
instance OutputSink Inherited where
    osStdStream = (\Nothing -> return Inherited, Just Inherit)
instance OutputSink UseProvidedHandle where
    osStdStream = (\Nothing -> return UseProvidedHandle, Nothing)

-- | Blocking call to wait for a process to exit.
--
-- Since 0.1.4
waitForStreamingProcess :: MonadIO m => StreamingProcessHandle -> m ExitCode
waitForStreamingProcess = liftIO . atomically . waitForStreamingProcessSTM

-- | STM version of @waitForStreamingProcess@.
--
-- Since 0.1.4
waitForStreamingProcessSTM :: StreamingProcessHandle -> STM ExitCode
waitForStreamingProcessSTM = readTMVar . streamingProcessHandleTMVar

-- | Non-blocking call to check for a process exit code.
--
-- Since 0.1.4
getStreamingProcessExitCode :: MonadIO m => StreamingProcessHandle -> m (Maybe ExitCode)
getStreamingProcessExitCode = liftIO . atomically .  getStreamingProcessExitCodeSTM

-- | STM version of @getStreamingProcessExitCode@.
--
-- Since 0.1.4
getStreamingProcessExitCodeSTM :: StreamingProcessHandle -> STM (Maybe ExitCode)
getStreamingProcessExitCodeSTM = tryReadTMVar . streamingProcessHandleTMVar

-- | Get the raw @ProcessHandle@ from a @StreamingProcessHandle@. Note that
-- you should avoid using this to get the process exit code, and instead
-- use the provided functions.
--
-- Since 0.1.4
streamingProcessHandleRaw :: StreamingProcessHandle -> ProcessHandle
streamingProcessHandleRaw (StreamingProcessHandle ph _) = ph

-- | Get the @TMVar@ storing the process exit code. In general, one of the
-- above functions should be used instead to avoid accidentally corrupting the variable\'s state..
--
-- Since 0.1.4
streamingProcessHandleTMVar :: StreamingProcessHandle -> TMVar ExitCode
streamingProcessHandleTMVar (StreamingProcessHandle _ var) = var

-- | The primary function for running a process. Note that, with the
-- exception of 'UseProvidedHandle', the values for @std_in@, @std_out@
-- and @std_err@ will be ignored by this function.
--
-- Since 0.1.4
streamingProcess :: (MonadIO m, InputSource stdin, OutputSink stdout, OutputSink stderr)
               => CreateProcess
               -> m (stdin, stdout, stderr, StreamingProcessHandle)
streamingProcess cp = liftIO $ do
    let (getStdin, stdinStream) = isStdStream
        (getStdout, stdoutStream) = osStdStream
        (getStderr, stderrStream) = osStdStream

    (stdinH, stdoutH, stderrH, ph) <- createProcess cp
        { std_in = fromMaybe (std_in cp) stdinStream
        , std_out = fromMaybe (std_out cp) stdoutStream
        , std_err = fromMaybe (std_err cp) stderrStream
        }

    ec <- atomically newEmptyTMVar
    _ <- forkIO $ waitForProcess ph >>= atomically . putTMVar ec

    (,,,)
        <$> getStdin stdinH
        <*> getStdout stdoutH
        <*> getStderr stderrH
        <*> return (StreamingProcessHandle ph ec)
