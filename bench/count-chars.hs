import Gauge.Main
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString (..))
import Data.Streaming.Text

calcLen :: (S.ByteString -> DecodeResult)
        -> L.ByteString
        -> Int
calcLen =
    loop 0
  where
    loop total _ Empty = total
    loop total dec (Chunk bs bss) =
        total' `seq` loop total' dec' bss
      where
        DecodeResultSuccess t dec' = dec bs
        total' = total + T.length t

handleEncoding :: ( String
                  , TL.Text -> L.ByteString
                  , L.ByteString -> TL.Text
                  , S.ByteString -> DecodeResult
                  )
               -> Benchmark
handleEncoding (name, encodeLazy, decodeLazy, decodeStream) = bgroup name
    [ bench "lazy" $ whnf (TL.length . decodeLazy) lbs
    , bench "stream" $ whnf (calcLen decodeStream) lbs
    ]
  where
    text = TL.pack $ concat $ replicate 10 ['\27'..'\2003']
    lbs = encodeLazy text

main :: IO ()
main = defaultMain $ map handleEncoding
    [ ("UTF-8", TLE.encodeUtf8, TLE.decodeUtf8, decodeUtf8)
    , ("UTF-8 pure", TLE.encodeUtf8, TLE.decodeUtf8, decodeUtf8Pure)
    , ("UTF-16LE", TLE.encodeUtf16LE, TLE.decodeUtf16LE, decodeUtf16LE)
    , ("UTF-16BE", TLE.encodeUtf16BE, TLE.decodeUtf16BE, decodeUtf16BE)
    , ("UTF-32LE", TLE.encodeUtf32LE, TLE.decodeUtf32LE, decodeUtf32LE)
    , ("UTF-32BE", TLE.encodeUtf32BE, TLE.decodeUtf32BE, decodeUtf32BE)
    ]
