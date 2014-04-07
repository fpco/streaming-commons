{-# LANGUAGE OverloadedStrings #-}
module Data.Streaming.NetworkSpec where

import           Control.Concurrent.Async (withAsync)
import           Control.Exception        (bracket)
import           Control.Monad            (forever, replicateM_)
import           Data.Array.Unboxed       (elems)
import qualified Data.ByteString.Char8    as S8
import           Data.Char                (toUpper)
import           Data.Streaming.Network
import           Network.Socket           (sClose)
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "getUnassignedPort" $ do
        it "sanity" $ replicateM_ 100000 $ do
            port <- getUnassignedPort
            (port `elem` elems unassignedPorts) `shouldBe` True
    describe "bindRandomPortTCP" $ do
        modifyMaxSuccess (const 5) $ prop "sanity" $ \content -> bracket
            (bindRandomPortTCP "*4")
            (sClose . snd)
            $ \(port, socket) -> do
                let server ad = forever $ appRead ad >>= appWrite ad . S8.map toUpper
                    client ad = do
                        appWrite ad bs
                        appRead ad >>= (`shouldBe` S8.map toUpper bs)
                    bs
                        | null content = "hello"
                        | otherwise = S8.pack $ take 1000 content
                withAsync (runTCPServer (serverSettingsTCPSocket socket) server) $ \_ -> do
                    runTCPClient (clientSettingsTCP port "127.0.0.1") client
