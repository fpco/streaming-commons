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
import Data.Word (Word8)

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

    it "#49 nix-reported failure" $ do
        let bss = map S.pack nixInput
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

nixInput :: [[Word8]]
nixInput = [[221,42,220,12,85,129,93,106,35,208,150,255,67,235,26,13,37,122],[0,139,100,196,26,255,133],[13,129,215,101,210,74,190,0,126,241,22,129,148,156,140,200,172],[],[],[29,246,121,24,207],[144,220,14,27,70,9,216,215,85,136,191,168,54,146,145,158,104,50,85,100,9,111,154,5,181,179,95,240,175,170,156,255,151,246,241,54,98,17,223,165,106,102,33,106,91,216,239,221,133],[],[104,111,31,220,20,31,120,0],[30,105,87,28,31,169,202,31,52,57,38,106,173,231,128,58,253,188,180,106,47,235,78,74,128,90,206,27,116],[1,199,209,219,3,6,167,161,53,31,153,231,77,18,153,187,140,40,228,6,252,187,78,204,22,146,147,255,216,117],[101,10,63,30,100,16,30,4,207,73,208,249,166,15],[23,70,72,65,2,18,253,94,84,124,44,252,174,1,196,94,125,218,112,33,237,33,0,244,227,130,25,125,151,1,185,219,159,10,119,1,26,154,143,90,199,236,68,135,14,84,201,64,96,81,12,55,255],[38,24,112,208,24,119,3,136,221,175,63,34,230,111,124,44,84,125,99,213,220,47,206,48,76,84,85,211,89,205,172,10,206,161,69,74,252,233,153,32,227,41,129,164,172,106,148,199],[178,56,130,109,184,77,26,126,90,197,236,49,221,188,43,93,169,159,200,110,148,23,49,232,38,255],[209,176,22,85,5,151,88,108,221,76,138,31,61,7,250,26,3,159,234,176,149,135,79,47,145,225,132,125,157,98,74,66,237,208],[242,252,35,71,176,215,66],[52,185,227,50,223,98,97,39,210,54,152,19,115,193,6,245,151,5,178,131,1,204,92],[173,25,184,240,246,205,57,204,48,106,251,30,196,117,69],[151,165,224,190,202,165,214,147,36,122,248,160,155,70,69,209,228,72,28,134,192,0,105,246,2,199,19,115,201,194,197,237,105,158,103,4,205],[132,0,255,193,159,97,106,83,252,185,170,34,60,217,76,225,247,101,116,18,35,79,57,145,254,13,28,81,59,56,6,48,125,201,212,111],[201,193,119,203,207,190,233,36,1,154,70,175,12,178,17,103,13,146,9,209,159,110,187,40,136,154,26,87,193,140,234,209,22,84,102,80],[85,77,1,97,151,169,99,163,23,195,169],[84,5,188,42,188,125,233,155,39,38,14,206,193,26,8,92,106,180,55,240,102,165,91,181,144,128,128,166,230,123,21,193,245,124,140,89,113,170,72,144,193,85,219,58,15,45,166],[12,141,167,201,182,28,178,252,232,195,154,86,5,64,239,8,127,233,28,202,11,124,191,101,120,180,221,77,69,159,78,111,171,82,225,213,233,77,219],[152,223,227,132,98,118,12,69,113,64,50,29,118,183,146,185,147,149,174,228,6,27,63,215,226,198,184,61,214,250,89,86,183,51,13,242,202,89,185,86,140,114],[83,202,145,247,159,233,14,255,181,19,134,197,150,241,134,61,202,29,115,120,227,127,190,245,20,84,145,226,65,221,223,240,79],[],[246,165,167,27,61,16,131,174,155,78,135,171,131,223,109,85],[190,73,41,176,154,11,215,181,173,254,188,18,65,69,69,46,1,225,27,111,236,117,247,156,240,164,20,237,171,178,103,13,180,72,64,71,98,97,71,3,65,128,219,26,50,175,128,245],[226,251,164,15,247,143,173,236,34,170,239,68,138,54,67,200,86,64,226,25,160,104,227,210,55,99,225,223,229,190,180,141,228,58,53,233,136,69,191,201,229,86,192,123,160,224,189,195,24,88,220,241],[207,122,195,155,212,176,223,231,206,234,143,124,132,160,171,14,144,237,146,92,36,121,190,67,249,138,56,181,111,122,93,70,136,40,185,12,237,156,66,246,230,122,78,105,154,120,8,71,15,100,43,20],[109,35,35,47,243,169,95,245,236,155,171,100,221,207,121,220,107,123,206,196,182,163,33,7,69,175,59,173,37,240,6,237,49,200,5,198,111],[185,132,153,43,130,81,209,156,50,241,110,172,83,203,46,201,204,140,181,59,128,10,223,8,247,203],[170,156,33,159,61,180,126,56,221,90,0,26,224,52,244,186,184,22,29,3,110,203,112,245,34,55,13,58,200,102,182],[136,176,244,205,188,74,136,217,138],[137,47,21,8,135,228,198,88,243,241,58,190,38,163,205,137,157,113,143,48,19,243,49,191,212,86,169,4,50,131,145,203,98,188,113,16,132,63,236,69],[15,37,249,26,40,52,171,164,240,183,4,125,98,143,97,22,140,5,15,233,207,80,180,189,230,79,106,127,38,49,189,107,106,164,109,51,233,225,122,253,84,5,61,100,238,89,203,218,42,199]]
