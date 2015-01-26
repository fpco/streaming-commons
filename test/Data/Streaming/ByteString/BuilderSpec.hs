{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Streaming.ByteString.BuilderSpec
    ( spec
    , builderSpec
    , BuilderFunctions(..)
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.IORef
import Data.Maybe
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.Streaming.ByteString.Builder
import Data.Streaming.ByteString.Builder.Class

data BuilderFunctions b = BuilderFunctions
    { bfFromByteString       :: S.ByteString -> b
    , bfInsertLazyByteString :: L.ByteString -> b
    , bfToLazyByteString     :: b -> L.ByteString
    , bfInsertByteString     :: S.ByteString -> b
    , bfCopyByteString       :: S.ByteString -> b
    }

tester :: StreamingBuilder b => BufferAllocStrategy -> [b] -> IO [S.ByteString]
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

testerFlush :: StreamingBuilder b
            => BufferAllocStrategy -> [Maybe b] -> IO [Maybe S.ByteString]
testerFlush strat builders0 = do
    (recv, finish) <- newBuilderRecv strat
    let loop front [] = do
            mbs <- finish
            return $ front $ maybe [] (return . Just) mbs
        loop front0 (mbu:bus) = do
            popper <- recv $ fromMaybe builderFlush mbu
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

builderSpec :: forall b. StreamingBuilder b => BuilderFunctions b -> Spec
builderSpec BuilderFunctions{..} = do
    prop "idempotent to toLazyByteString" $ \bss' -> do
        let bss = map S.pack bss'
        let builders :: [b]
            builders = map bfFromByteString bss
        let lbs = bfToLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    it "works for large input" $ do
        let builders :: [b]
            builders = replicate 10000 (bfFromByteString "hello world!" :: b)
        let lbs = bfToLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    it "works for lazy bytestring insertion" $ do
        let builders :: [b]
            builders = replicate 10000 (bfInsertLazyByteString "hello world!")
        let lbs = bfToLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    prop "works for strict bytestring insertion" $ \bs' -> do
        let bs = S.pack bs'
        let builders :: [b]
            builders = replicate 10000 (bfCopyByteString bs `mappend` bfInsertByteString bs)
        let lbs = bfToLazyByteString $ mconcat builders
        outBss <- tester defaultStrategy builders
        L.fromChunks outBss `shouldBe` lbs

    it "flush shouldn't bring in empty strings." $ do
        let dat = ["hello", "world"]
            builders :: [b]
            builders = map ((`mappend` builderFlush) . bfFromByteString) dat
        out <- tester defaultStrategy builders
        dat `shouldBe` out

    prop "flushing" $ \bss' -> do
        let bss = concatMap (\bs -> [Just $ S.pack bs, Nothing]) $ filter (not . null) bss'
        let builders :: [Maybe b]
            builders = map (fmap bfFromByteString) bss
        outBss <- testerFlush defaultStrategy builders
        outBss `shouldBe` bss
    it "large flush input" $ do
        let lbs = L.pack $ concat $ replicate 100000 [0..255]
            chunks :: [Maybe b]
            chunks = map (Just . bfFromByteString) (L.toChunks lbs)
        bss <- testerFlush defaultStrategy chunks
        L.fromChunks (catMaybes bss) `shouldBe` lbs

spec :: Spec
spec =
    describe "Data.Streaming.ByteString.Builder" $ do

        builderSpec BuilderFunctions
            { bfFromByteString       = B.byteString
            , bfInsertLazyByteString = B.lazyByteStringInsert
            , bfToLazyByteString     = B.toLazyByteString
            , bfInsertByteString     = B.byteStringInsert
            , bfCopyByteString       = B.byteStringCopy
            }

        prop "toByteStringIO idempotent to toLazyByteString" $ \bss' -> do
            let bss = mconcat (map (B.byteString . S.pack) bss')
            ior <- newIORef []
            toByteStringIOWith 16
                               (\s -> do s' <- S.useAsCStringLen s S.unsafePackCStringLen
                                         modifyIORef ior (s' :))
                               bss
            chunks <- readIORef ior
            L.fromChunks (reverse chunks) `shouldBe` B.toLazyByteString bss
