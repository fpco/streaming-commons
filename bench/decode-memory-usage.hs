{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString (ByteString)
import Data.Streaming.Text
import System.Environment (getArgs)

input :: [ByteString]
input = replicate 1000000 "Hello World!\n"

main :: IO ()
main = do
    args <- getArgs
    let dec =
            case args of
                ["16le"] -> decodeUtf16LE
                ["16be"] -> decodeUtf16BE
                ["32le"] -> decodeUtf32LE
                ["32be"] -> decodeUtf32BE
                ["8pure"] -> decodeUtf8Pure
                _ -> decodeUtf8

    loop dec input

loop :: (ByteString -> DecodeResult) -> [ByteString] -> IO ()
loop dec [] =
    case dec "" of
        DecodeResultSuccess _ _ -> return ()
        DecodeResultFailure _ _ -> error "failure1"
loop dec (bs:bss) =
    case dec bs of
        DecodeResultSuccess _ dec' -> loop dec' bss
        DecodeResultFailure _ _ -> error "failure2"
