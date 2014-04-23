{-# LANGUAGE OverloadedStrings #-}
module Data.Streaming.BlazeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.ByteString.Char8 ()
import Data.Streaming.Blaze
import Data.Monoid
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, insertLazyByteString, insertByteString, flush, Builder, copyByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Maybe (fromMaybe, catMaybes)

tester :: BufferAllocStrategy -> [Builder] -> IO [S.ByteString]
tester strat builders0 = do
    (recv, finish) <- newBlazeRecv strat
    let loop front [] = do
            mbs <- finish
            return $ front $ maybe [] return mbs
        loop front0 (bu:bus) = do
            popper <- recv bu
            let go front = do
                    bs <- popper
                    if S.null bs
                        then loop front bus
                        else go (front . (bs:))
            go front0
    loop id builders0

testerFlush :: BufferAllocStrategy -> [Maybe Builder] -> IO [Maybe S.ByteString]
testerFlush strat builders0 = do
    (recv, finish) <- newBlazeRecv strat
    let loop front [] = do
            mbs <- finish
            return $ front $ maybe [] (return . Just) mbs
        loop front0 (mbu:bus) = do
            popper <- recv $ fromMaybe flush mbu
            let go front = do
                    bs <- popper
                    if S.null bs
                        then
                            case mbu of
                                Nothing -> loop (front . (Nothing:)) bus
                                Just _ -> loop front bus
                        else go (front . (Just bs:))
            go front0
    loop id builders0

spec :: Spec
spec =
    describe "Data.Streaming.Blaze" $ do
        prop "idempotent to toLazyByteString" $ \bss' -> do
            let bss = map S.pack bss'
            let builders = map fromByteString bss
            let lbs = toLazyByteString $ mconcat builders
            outBss <- tester defaultStrategy builders
            L.fromChunks outBss `shouldBe` lbs

        it "works for large input" $ do
            let builders = replicate 10000 (fromByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            outBss <- tester defaultStrategy builders
            L.fromChunks outBss `shouldBe` lbs

        it "works for lazy bytestring insertion" $ do
            let builders = replicate 10000 (insertLazyByteString "hello world!")
            let lbs = toLazyByteString $ mconcat builders
            outBss <- tester defaultStrategy builders
            L.fromChunks outBss `shouldBe` lbs

        prop "works for strict bytestring insertion" $ \bs' -> do
            let bs = S.pack bs'
            let builders = replicate 10000 (copyByteString bs `mappend` insertByteString bs)
            let lbs = toLazyByteString $ mconcat builders
            outBss <- tester defaultStrategy builders
            L.fromChunks outBss `shouldBe` lbs

        it "flush shouldn't bring in empty strings." $ do
            let dat = ["hello", "world"]
                builders = map ((`mappend` flush) . fromByteString) dat
            out <- tester defaultStrategy builders
            dat `shouldBe` out

        prop "flushing" $ \bss' -> do
            let bss = concatMap (\bs -> [Just $ S.pack bs, Nothing]) $ filter (not . null) bss'
            let builders = map (fmap fromByteString) bss
            outBss <- testerFlush defaultStrategy builders
            outBss `shouldBe` bss
        it "large flush input" $ do
            let lbs = L.pack $ concat $ replicate 100000 [0..255]
                chunks = map (Just . fromByteString) (L.toChunks lbs)
            bss <- testerFlush defaultStrategy chunks
            L.fromChunks (catMaybes bss) `shouldBe` lbs
