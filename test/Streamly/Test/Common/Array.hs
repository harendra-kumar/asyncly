-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

-- This is a common array test module that gets included in different
-- Array test modules with the corresponding macro defined.

import Foreign.Storable (Storable(..))

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.Hspec as H

import Streamly.Prelude (SerialT)
import Streamly.Test.Common (listEquals)

import qualified Streamly.Prelude as S
#ifdef TEST_SMALL_ARRAY
import qualified Streamly.Internal.Data.SmallArray as A
type Array = A.SmallArray
#elif defined(TEST_ARRAY)
-- import Data.Word(Word8)

import qualified Streamly.Internal.Data.Array.Foreign as A
-- import qualified Streamly.Internal.Data.Array.Foreign.Types as A
-- import qualified Streamly.Internal.Data.Stream.IsStream as IP
-- import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
type Array = A.Array
#elif defined(DATA_ARRAY_PRIM_PINNED)
import qualified Streamly.Internal.Data.Array.Prim.Pinned as A
-- import qualified Streamly.Internal.Data.Array.Prim.Pinned.Types as A
-- import qualified Streamly.Internal.Data.Stream.IsStream as IP
type Array = A.Array
#elif defined(DATA_ARRAY_PRIM)
import qualified Streamly.Internal.Data.Array.Prim as A
-- import qualified Streamly.Internal.Data.Array.Prim.Types as A
-- import qualified Streamly.Internal.Data.Stream.IsStream as IP
type Array = A.Array
#else
import qualified Streamly.Internal.Data.Array as A
type Array = A.Array
#endif

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- XXX this should be in sync with the defaultChunkSize in Array code, or we
-- should expose that and use that. For fast testing we could reduce the
-- defaultChunkSize under CPP conditionals.
--
defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

genericTestFrom ::
       (Int -> SerialT IO Int -> IO (Array Int))
    -> Property
genericTestFrom arrFold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- run $ arrFold len $ S.fromList list
                assert (A.length arr == len)

testLength :: Property
testLength = genericTestFrom (\n -> S.fold (A.writeN n))

testLengthFromStreamN :: Property
testLengthFromStreamN = genericTestFrom A.fromStreamN

#ifndef TEST_SMALL_ARRAY
testLengthFromStream :: Property
testLengthFromStream = genericTestFrom (const A.fromStream)
#endif

genericTestFromTo ::
       (Int -> SerialT IO Int -> IO (Array Int))
    -> (Array Int -> SerialT IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Property
genericTestFromTo arrFold arrUnfold listEq =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- run $ arrFold len $ S.fromList list
                xs <- run $ S.toList $ arrUnfold arr
                assert (listEq xs list)

testFoldNUnfold :: Property
testFoldNUnfold =
    genericTestFromTo (\n -> S.fold (A.writeN n)) (S.unfold A.read) (==)

testFoldNToStream :: Property
testFoldNToStream =
    genericTestFromTo (\n -> S.fold (A.writeN n)) A.toStream (==)

testFoldNToStreamRev :: Property
testFoldNToStreamRev =
    genericTestFromTo
        (\n -> S.fold (A.writeN n))
        A.toStreamRev
        (\xs list -> xs == reverse list)

testFromStreamNUnfold :: Property
testFromStreamNUnfold = genericTestFromTo A.fromStreamN (S.unfold A.read) (==)

testFromStreamNToStream :: Property
testFromStreamNToStream = genericTestFromTo A.fromStreamN A.toStream (==)

testFromListN :: Property
testFromListN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromListN n list
                    xs <- run $ S.toList $ (S.unfold A.read) arr
                    listEquals (==) xs (take n list)

#ifndef TEST_SMALL_ARRAY
testFromStreamToStream :: Property
testFromStreamToStream = genericTestFromTo (const A.fromStream) A.toStream (==)

testFoldUnfold :: Property
testFoldUnfold = genericTestFromTo (const (S.fold A.write)) (S.unfold A.read) (==)

testFromList :: Property
testFromList =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    xs <- run $ S.toList $ (S.unfold A.read) arr
                    assert (xs == list)
#endif

#if defined(TEST_ARRAY) ||\
    defined(DATA_ARRAY_PRIM) ||\
    defined(DATA_ARRAY_PRIM_PINNED)
{-
testArraysOf :: Property
testArraysOf =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                xs <- run
                    $ S.toList
                    $ S.concatUnfold A.read
                    $ arraysOf 240
                    $ S.fromList list
                assert (xs == list)
  where
    arraysOf n = IP.chunksOf n (A.writeNUnsafe n)
-}
#endif

#ifdef TEST_ARRAY

lastN :: Int -> [a] -> [a]
lastN n l = drop (length l - n) l

testLastN :: Property
testLastN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    xs <- run
                        $ fmap A.toList
                        $ S.fold (A.lastN n)
                        $ S.fromList list
                    assert (xs == lastN n list)

testLastN_LN :: Int -> Int -> IO Bool
testLastN_LN len n = do
    let list = [1..len]
    l1 <- fmap A.toList $ S.fold (A.lastN n) $ S.fromList list
    let l2 = lastN n list
    return $ l1 == l2

-- Instead of hard coding 10000 here we can have maxStreamLength for operations
-- that use stream of arrays.
{-
concatArrayW8 :: Property
concatArrayW8 =
    forAll (vectorOf 10000 (arbitrary :: Gen Word8))
        $ \w8List -> do
              let w8ArrList = A.fromList . (: []) <$> w8List
              f2 <- S.toList $ AS.concat $ S.fromList w8ArrList
              w8List `shouldBe` f2
-}
#endif

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe "Construction" $ do
            prop "length . writeN n === n" testLength
            prop "length . fromStreamN n === n" testLengthFromStreamN
            prop "read . writeN === id " testFoldNUnfold
            prop "toStream . writeN === id" testFoldNToStream
            prop "toStreamRev . writeN === reverse" testFoldNToStreamRev
            prop "read . fromStreamN === id" testFromStreamNUnfold
            prop "toStream . fromStreamN === id" testFromStreamNToStream
            prop "fromListN" testFromListN

#ifndef TEST_SMALL_ARRAY
            prop "length . fromStream === n" testLengthFromStream
            prop "toStream . fromStream === id" testFromStreamToStream
            prop "read . write === id" testFoldUnfold
            prop "fromList" testFromList
#endif

#if defined(TEST_ARRAY) ||\
    defined(DATA_ARRAY_PRIM) ||\
    defined(DATA_ARRAY_PRIM_PINNED)
            -- prop "arraysOf concats to original" testArraysOf
#endif

#ifdef TEST_ARRAY
            -- prop "AS.concat . (A.fromList . (:[]) <$>) === id" $ concatArrayW8
        describe "Fold" $ do
            prop "lastN : 0 <= n <= len" $ testLastN
            describe "lastN boundary conditions" $ do
                it "lastN -1" (testLastN_LN 10 (-1) `shouldReturn` True)
                it "lastN 0" (testLastN_LN 10 0 `shouldReturn` True)
                it "lastN length" (testLastN_LN 10 10 `shouldReturn` True)
                it "lastN (length + 1)" (testLastN_LN 10 11 `shouldReturn` True)
#endif
