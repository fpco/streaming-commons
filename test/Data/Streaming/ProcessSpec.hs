{-# LANGUAGE CPP #-}
module Data.Streaming.ProcessSpec (spec, main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import System.Exit
import Control.Concurrent (threadDelay)
import Data.Streaming.Process
import System.IO (hClose)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
#ifndef WINDOWS
    prop "cat" $ \wss -> do
        let lbs = L.fromChunks $ map S.pack wss
        (sink, source, Inherited, cph) <- streamingProcess (shell "cat")
        ((), bs) <- concurrently
            (do
                L.hPut sink lbs
                hClose sink)
            (S.hGetContents source)
        L.fromChunks [bs] `shouldBe` lbs
        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess

    it "closed stream" $ do
        (ClosedStream, source, Inherited, cph) <- streamingProcess (shell "cat")
        bss <- S.hGetContents source
        bss `shouldBe` S.empty

        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess

    it "checked process" $ do
        let isRightException ProcessExitedUnsuccessfully {} = True
        withCheckedProcess (proc "false" [])
            (\Inherited Inherited Inherited -> return ())
            `shouldThrow` isRightException

#endif
    it "blocking vs non-blocking" $ do
        (ClosedStream, ClosedStream, ClosedStream, cph) <- streamingProcess (shell "sleep 1")

        mec1 <- getStreamingProcessExitCode cph
        mec1 `shouldBe` Nothing

        threadDelay 1500000

        mec2 <- getStreamingProcessExitCode cph
        mec2 `shouldBe` Just ExitSuccess

        ec <- waitForStreamingProcess cph
        ec `shouldBe` ExitSuccess
