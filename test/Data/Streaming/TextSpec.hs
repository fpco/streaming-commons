{-# LANGUAGE OverloadedStrings #-}
module Data.Streaming.TextSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Streaming.Text as SD
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Exception (evaluate, try, SomeException)
import Control.DeepSeq (deepseq, NFData)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Control.Monad (forM_)
import Data.ByteString.Char8 ()

try' :: NFData a => a -> IO (Either SomeException a)
try' a = try $ evaluate (a `deepseq` a)

spec :: Spec
spec = describe "Data.Streaming.TextSpec" $ {-modifyMaxSuccess (const 10000) $ -}do
    let test name lazy stream encodeLazy encodeStrict = describe name $ do
            prop "bytes" $ check lazy stream
            prop "chars" $ \css -> do
                let ts = map T.pack css
                    lt = TL.fromChunks ts
                    lbs = encodeLazy lt
                    bss = L.toChunks lbs
                    wss = map S.unpack bss
                 in check lazy stream wss
            it "high code points" $ forM_ [100, 200..50000] $ \cnt -> do
                let t = T.replicate cnt "\x10000"
                    bs = encodeStrict t
                case stream bs of
                    SD.DecodeResultSuccess t' dec -> do
                        t' `shouldBe` t
                        case dec S.empty of
                            SD.DecodeResultSuccess _ _ -> return ()
                            SD.DecodeResultFailure _ _ -> error "unexpected failure 1"
                    SD.DecodeResultFailure _ _ -> error "unexpected failure 2"

        check lazy stream wss = do
            let bss = map S.pack wss
                lbs = L.fromChunks bss
            x <- try' $ feedLazy stream lbs
            y <- try' $ lazy lbs
            case (x, y) of
                (Right x', Right y') -> x' `shouldBe` y'
                (Left _, Left _) -> return ()
                _ -> error $ show (x, y)
    test "UTF8" TLE.decodeUtf8 SD.decodeUtf8 TLE.encodeUtf8 TE.encodeUtf8
    test "UTF8 pure" TLE.decodeUtf8 SD.decodeUtf8Pure TLE.encodeUtf8 TE.encodeUtf8
    test "UTF16LE" TLE.decodeUtf16LE SD.decodeUtf16LE TLE.encodeUtf16LE TE.encodeUtf16LE
    test "UTF16BE" TLE.decodeUtf16BE SD.decodeUtf16BE TLE.encodeUtf16BE TE.encodeUtf16BE
    test "UTF32LE" TLE.decodeUtf32LE SD.decodeUtf32LE TLE.encodeUtf32LE TE.encodeUtf32LE
    test "UTF32BE" TLE.decodeUtf32BE SD.decodeUtf32BE TLE.encodeUtf32BE TE.encodeUtf32BE

    describe "UTF8 leftovers" $ do
        describe "C" $ do
            it "single chunk" $ do
                let bs = "good\128\128bad"
                case SD.decodeUtf8 bs of
                    SD.DecodeResultSuccess _ _ -> error "Shouldn't have succeeded"
                    SD.DecodeResultFailure t bs' -> do
                        t `shouldBe` "good"
                        bs' `shouldBe` "\128\128bad"

            it "multi chunk, no good" $ do
                let bs1 = "\226"
                    bs2 = "\130"
                    bs3 = "ABC"
                case SD.decodeUtf8 bs1 of
                    SD.DecodeResultSuccess "" dec2 ->
                        case dec2 bs2 of
                            SD.DecodeResultSuccess "" dec3 ->
                                case dec3 bs3 of
                                    SD.DecodeResultFailure "" bs -> bs `shouldBe` "\226\130ABC"
                                    _ -> error "fail on dec3"
                            _ -> error "fail on dec2"
                    _ -> error "fail on dec1"

            it "multi chunk, good in the middle" $ do
                let bs1 = "\226"
                    bs2 = "\130\172\226"
                    bs3 = "\130ABC"
                case SD.decodeUtf8 bs1 of
                    SD.DecodeResultSuccess "" dec2 ->
                        case dec2 bs2 of
                            SD.DecodeResultSuccess "\x20AC" dec3 ->
                                case dec3 bs3 of
                                    SD.DecodeResultFailure "" bs -> bs `shouldBe` "\226\130ABC"
                                    _ -> error "fail on dec3"
                            _ -> error "fail on dec2"
                    _ -> error "fail on dec1"
        describe "pure" $ do
            it "multi chunk, no good" $ do
                let bs1 = "\226"
                    bs2 = "\130"
                    bs3 = "ABC"
                case SD.decodeUtf8Pure bs1 of
                    SD.DecodeResultSuccess "" dec2 ->
                        case dec2 bs2 of
                            SD.DecodeResultSuccess "" dec3 ->
                                case dec3 bs3 of
                                    SD.DecodeResultFailure "" bs -> bs `shouldBe` "\226\130ABC"
                                    _ -> error "fail on dec3"
                            _ -> error "fail on dec2"
                    _ -> error "fail on dec1"

    describe "UTF16LE spot checks" $ do
        it "[[0,216,0],[220,0,0,0,0,0,0]]" $ do
            let bss = map S.pack [[0,216,0],[220,0,0,0,0,0,0]]
                lbs = L.fromChunks bss
            x <- try' $ feedLazy SD.decodeUtf16LE lbs
            y <- try' $ TLE.decodeUtf16LE lbs
            case (x, y) of
                (Right x', Right y') -> x' `shouldBe` y'
                (Left _, Left _) -> return ()
                _ -> error $ show (x, y)

feedLazy :: (S.ByteString -> SD.DecodeResult)
         -> L.ByteString
         -> TL.Text
feedLazy start =
    TL.fromChunks . loop start . L.toChunks
  where
    loop dec [] =
        case dec S.empty of
            SD.DecodeResultSuccess t _ -> [t]
            SD.DecodeResultFailure _ _ -> [error "invalid sequence 1"]
    loop dec (bs:bss) =
        case dec bs of
            SD.DecodeResultSuccess t dec' -> t : loop dec' bss
            SD.DecodeResultFailure _ _ -> [error "invalid sequence 2"]
