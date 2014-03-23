module Data.Streaming.FileReadSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as S
import qualified Data.Streaming.FileRead as F
import Control.Exception (bracket)

spec :: Spec
spec = describe "Data.Streaming.FileRead" $ do
    it "works" $ do
        let fp = "LICENSE"
        expected <- S.readFile fp
        actual <- bracket (F.openFile fp) F.closeFile $ \fh -> do
            let loop front = do
                    bs <- F.readChunk fh
                    if S.null bs
                        then return $ S.concat $ front []
                        else loop (front . (bs:))
            loop id
        actual `shouldBe` expected
