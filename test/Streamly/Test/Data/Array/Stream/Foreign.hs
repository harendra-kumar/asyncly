module Main (main) where

import Streamly.Test.Common (listEquals, chooseInt)
import Test.Hspec (hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property, vectorOf, Gen)
import Test.QuickCheck.Monadic (monadicIO, run)

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Test.Hspec as Hspec

import Prelude hiding (sequence)

#if MIN_VERSION_QuickCheck(2,14,0)

import Test.QuickCheck (chooseAny)

#else

import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

#endif

maxTestCount :: Int
maxTestCount = 100

parse :: Property
parse = do
    let len = 200
    -- ls = input list (stream)
    -- clen = chunk size
    -- tlen = parser take size
    forAll
        ((,,)
            <$> vectorOf len (chooseAny :: Gen Int)
            <*> chooseInt (1, len)
            <*> chooseInt (0, len))
        $ \(ls, clen, tlen) ->
            monadicIO $ do
                (ls1, str) <-
                    let input =
                            Stream.chunksOf
                                clen (Array.writeN clen) (Stream.fromList ls)
                        parser = ParserD.fromFold (Fold.take tlen Fold.toList)
                     in run $ ArrayStream.parse parser input
                ls2 <- run $ Stream.toList $ ArrayStream.concat str
                listEquals (==) (ls1 ++ ls2) ls

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Stream.Foreign"

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream parsing" $ do
                prop "parse" parse
