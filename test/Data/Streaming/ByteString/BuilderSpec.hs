{-# LANGUAGE OverloadedStrings #-}
module Data.Streaming.ByteString.BuilderSpec (spec) where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.Streaming.ByteString.Builder

spec :: Spec
spec =
    describe "Data.Streaming.ByteString.Builder" $ do
        prop "toByteStringIO idempotent to toLazyByteString" $ \bss' -> do
            let bss = mconcat (map (B.byteString . S.pack) bss')
            ior <- newIORef []
            toByteStringIOWith 16
                               (\s -> do s' <- S.useAsCStringLen s S.unsafePackCStringLen
                                         modifyIORef ior (s' :))
                               bss
            chunks <- readIORef ior
            L.fromChunks (reverse chunks) `shouldBe` B.toLazyByteString bss
