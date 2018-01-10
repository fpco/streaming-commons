{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Streaming.ByteString.BuilderSpec
    ( spec
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.IORef
import Data.Maybe
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.Streaming.ByteString.Builder

tester :: BufferAllocStrategy -> [Builder] -> IO [S.ByteString]
tester strat builders0 = do
    (recv, finish) <- newBuilderRecv strat
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
    (recv, finish) <- newBuilderRecv strat
    let loop front [] = do
            mbs <- finish
            return $ front $ maybe [] (return . Just) mbs
        loop front0 (mbu:bus) = do
            popper <- recv $ fromMaybe B.flush mbu
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

builderSpec :: Spec
builderSpec = do
    prop "idempotent to toLazyByteString" $ \bss' -> do
        let bss = map S.pack bss'
        let builders = map B.byteString bss
        let lbs = B.toLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    it "works for large input" $ do
        let builders = replicate 10000 (B.byteString "hello world!")
        let lbs = B.toLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    it "works for lazy bytestring insertion" $ do
        let builders = replicate 10000 (B.lazyByteStringInsert "hello world!")
        let lbs = B.toLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    prop "works for strict bytestring insertion" $ \bs' -> do
        let bs = S.pack bs'
        let builders = replicate 10000 (B.byteStringCopy bs `Data.Monoid.mappend` B.byteStringInsert bs)
        let lbs = B.toLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    it "flush shouldn't bring in empty strings." $ do
        let dat = ["hello", "world"]
            builders = map ((`mappend` B.flush) . B.byteString) dat
        out <- tester defaultStrategy builders
        dat `shouldBe` out

    prop "flushing" $ \bss' -> do
        let bss = concatMap (\bs -> [Just $ S.pack bs, Nothing]) $ filter (not . null) bss'
        let builders = map (fmap B.byteString) bss
        outBss <- testerFlush defaultStrategy builders
        outBss `shouldBe` bss
    it "large flush input" $ do
        let lbs = L.pack $ concat $ replicate 100000 [0..255]
            chunks = map (Just . B.byteString) (L.toChunks lbs)
        bss <- testerFlush defaultStrategy chunks
        L.fromChunks (catMaybes bss) `shouldBe` lbs

spec :: Spec
spec =
    describe "Data.Streaming.ByteString.Builder" $ do

        builderSpec

        prop "toByteStringIO idempotent to toLazyByteString" $ \bss' -> do
            let bss = mconcat (map (B.byteString . S.pack) bss')
            ior <- newIORef []
            toByteStringIOWith 16
                               (\s -> do s' <- S.useAsCStringLen s S.unsafePackCStringLen
                                         modifyIORef ior (s' :))
                               bss
            chunks <- readIORef ior
            L.fromChunks (reverse chunks) `shouldBe` B.toLazyByteString bss
