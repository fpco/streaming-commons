{-# LANGUAGE OverloadedStrings #-}
module Data.Streaming.BlazeSpec (spec) where

import Test.Hspec
import qualified Blaze.ByteString.Builder as B
import Data.Streaming.ByteString.BuilderSpec hiding (spec)

spec :: Spec
spec = do
    describe "Data.Streaming.Blaze" $ builderSpec BuilderFunctions
        { bfFromByteString       = B.fromByteString
        , bfInsertLazyByteString = B.insertLazyByteString
        , bfToLazyByteString     = B.toLazyByteString
        , bfInsertByteString     = B.insertByteString
        , bfCopyByteString       = B.copyByteString
        }
