{-# LANGUAGE ScopedTypeVariables #-}

import Gauge.Main
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Builder as BB
import Data.Monoid (mconcat, Monoid)
import qualified Data.Streaming.ByteString.Builder as BB

main :: IO ()
main = defaultMain [ bgroup "Data.Streaming.ByteString.Builder.toByteStringIO"
                            (benchmarks bIO b100_10000 b10000_100 b10000_10000)
                   , bgroup "Data.ByteString.Builder.toLazyByteString"
                            (benchmarks bLazy b100_10000 b10000_100 b10000_10000)
                   ]
  where
    bIO = whnfIO . BB.toByteStringIO (const (return ()))
    bLazy = nf BB.toLazyByteString
    benchmarks run bld100_10000 bld10000_100 bld10000_10000 =
        [ bench' run bld100_10000 100 10000
        , bench' run bld10000_100 10000 100
        , bench' run bld10000_10000 10000 10000
        ]
    bench' :: (b -> Benchmarkable) -> b -> Int -> Int -> Benchmark
    bench' run bld' len reps = bench (show len ++ "/" ++ show reps) (run bld')
    b100_10000 = bld BB.byteString 100 10000
    b10000_100 = bld BB.byteString 10000 100
    b10000_10000 = bld BB.byteString 10000 10000
    bld :: Monoid a => (S.ByteString -> a) -> Int -> Int -> a
    bld f len reps = mconcat (replicate reps (f (S.replicate len 'x')))
