-- |
-- Module      : Serial.Transformation2
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Serial.Transformation2 (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Prelude  as S

import Gauge
import Streamly.Prelude (SerialT, serially)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude
import Prelude hiding (reverse, tail)

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: S.MonadAsync m
    => (Int -> SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> SerialT m Int
iterateSingleton g count n = iterateN g (return n) count

-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       S.MonadAsync m
    => (Int -> SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> SerialT m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n

-- Apply transformation g count times on a stream of length len
{-# INLINE iterateSource #-}
iterateSource ::
       S.MonadAsync m
    => (SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> Int
    -> SerialT m Int
iterateSource g count len n = f count (sourceUnfoldrM len n)

    where

    f (0 :: Int) stream = stream
    f i stream = f (i - 1) (g stream)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

o_n_space_functor :: Int -> [Benchmark]
o_n_space_functor value =
    [ bgroup "Functor"
        [ benchIO "(+) (n times) (baseline)" $ \i0 ->
            iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value
        , benchIOSrc serially "(<$) (n times)" $
            iterateSingleton (<$) value
        , benchIOSrc serially "fmap (n times)" $
            iterateSingleton (fmap . (+)) value
        {-
        , benchIOSrc serially "_(<$) (n times)" $
            _iterateSingleton (<$) value
        , benchIOSrc serially "_fmap (n times)" $
            _iterateSingleton (fmap . (+)) value
        -}
        ]
    ]

-------------------------------------------------------------------------------
-- Grouping transformations
-------------------------------------------------------------------------------

{-# INLINE groups #-}
groups :: MonadIO m => SerialT m Int -> m ()
groups = S.drain . S.groups FL.drain

-- XXX Change this test when the order of comparison is later changed
{-# INLINE groupsByGT #-}
groupsByGT :: MonadIO m => SerialT m Int -> m ()
groupsByGT = S.drain . S.groupsBy (>) FL.drain

{-# INLINE groupsByEq #-}
groupsByEq :: MonadIO m => SerialT m Int -> m ()
groupsByEq = S.drain . S.groupsBy (==) FL.drain

-- XXX Change this test when the order of comparison is later changed
{-# INLINE groupsByRollingLT #-}
groupsByRollingLT :: MonadIO m => SerialT m Int -> m ()
groupsByRollingLT =
    S.drain . S.groupsByRolling (<) FL.drain

{-# INLINE groupsByRollingEq #-}
groupsByRollingEq :: MonadIO m => SerialT m Int -> m ()
groupsByRollingEq =
    S.drain . S.groupsByRolling (==) FL.drain

{-# INLINE foldMany #-}
foldMany :: Monad m => SerialT m Int -> m ()
foldMany =
      S.drain
    . S.map getSum
    . Internal.foldMany (FL.take 2 FL.mconcat)
    . S.map Sum

{-# INLINE foldIterateM #-}
foldIterateM :: Monad m => SerialT m Int -> m ()
foldIterateM =
    S.drain
        . S.map getSum
        . Internal.foldIterateM (return . FL.take 2 . FL.sconcat) (Sum 0)
        . S.map Sum

o_1_space_grouping :: Int -> [Benchmark]
o_1_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [ benchIOSink value "groups" groups
        , benchIOSink value "groupsByGT" groupsByGT
        , benchIOSink value "groupsByEq" groupsByEq
        , benchIOSink value "groupsByRollingLT" groupsByRollingLT
        , benchIOSink value "groupsByRollingEq" groupsByRollingEq
        , benchIOSink value "foldMany" foldMany
        , benchIOSink value "foldIterateM" foldIterateM
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> SerialT m Int -> m ()
reverse n = composeN n S.reverse

{-# INLINE reverse' #-}
reverse' :: MonadIO m => Int -> SerialT m Int -> m ()
reverse' n = composeN n Internal.reverse'

{-# INLINE sortBy #-}
sortBy :: MonadIO m => Int -> SerialT m Int -> m ()
sortBy n = composeN n (Internal.sortBy compare)

{-# INLINE sortByD #-}
sortByD :: MonadIO m => Int -> SerialT m Int -> m ()
sortByD n = composeN n (Internal.sortByD compare)

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
        -- Reversing/sorting a stream
          benchIOSink value "reverse" (reverse 1)
        , benchIOSink value "reverse'" (reverse' 1)
        , benchIOSink value "sortBy" (sortBy 1)
        , benchIOSink value "sortByD" (sortByD 1)
        , bench "sort Lists" $ nf (\x -> List.sort [1..x]) value

        , benchIOSink value "mkAsync" (mkAsync serially)
        ]
    ]

-------------------------------------------------------------------------------
-- Grouping/Splitting
-------------------------------------------------------------------------------

{-# INLINE classifySessionsOf #-}
classifySessionsOf :: (S.MonadAsync m) => SerialT m Int -> m ()
classifySessionsOf =
      S.drain
    . Internal.classifySessionsOf (const (return False)) 3 FL.drain
    . Internal.timestamped
    . S.concatMap (\x -> S.map (x,) (S.enumerateFromTo 1 (10 :: Int)))

o_n_space_grouping :: Int -> [Benchmark]
o_n_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [ benchIOSink (value `div` 10) "classifySessionsOf" classifySessionsOf
        ]
    ]

-------------------------------------------------------------------------------
-- Mixed Transformation
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: MonadIO m => Int -> SerialT m Int -> m ()
scanMap n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: MonadIO m => Int -> SerialT m Int -> m ()
dropMap n = composeN n $ S.map (subtract 1) . S.drop 1

{-# INLINE dropScan #-}
dropScan :: MonadIO m => Int -> SerialT m Int -> m ()
dropScan n = composeN n $ S.scanl' (+) 0 . S.drop 1

{-# INLINE takeDrop #-}
takeDrop :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeDrop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# INLINE takeScan #-}
takeScan :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeScan value n = composeN n $ S.scanl' (+) 0 . S.take (value + 1)

{-# INLINE takeMap #-}
takeMap :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeMap value n = composeN n $ S.map (subtract 1) . S.take (value + 1)

{-# INLINE filterDrop #-}
filterDrop :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# INLINE filterTake #-}
filterTake :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# INLINE filterScan #-}
filterScan :: MonadIO m => Int -> SerialT m Int -> m ()
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> SerialT m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterMap value n = composeN n $ S.map (subtract 1) . S.filter (<= (value + 1))

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => SerialT m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s, p) x -> (s + x, p * x)) (0, 1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => SerialT m Int -> m (Pair Int Int)
sumProductScan =
    S.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p * x)) (Pair 0 1) .
    S.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

{-# INLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Monad m => SerialT m Int -> m Int
foldl'ReduceMap = fmap (+ 1) . S.foldl' (+) 0

o_1_space_transformations_mixed :: Int -> [Benchmark]
o_1_space_transformations_mixed value =
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    [ bgroup "mixed"
        [ benchIOSink value "scanl-map" (scanMap 1)
        , benchIOSink value "foldl-map" foldl'ReduceMap
        , benchIOSink value "sum-product-fold" sumProductFold
        , benchIOSink value "sum-product-scan" sumProductScan
        ]
    ]

o_1_space_transformations_mixedX4 :: Int -> [Benchmark]
o_1_space_transformations_mixedX4 value =
    [ bgroup "mixedX4"
        [ benchIOSink value "scan-map" (scanMap 4)
        , benchIOSink value "drop-map" (dropMap 4)
        , benchIOSink value "drop-scan" (dropScan 4)
        , benchIOSink value "take-drop" (takeDrop value 4)
        , benchIOSink value "take-scan" (takeScan value 4)
        , benchIOSink value "take-map" (takeMap value 4)
        , benchIOSink value "filter-drop" (filterDrop value 4)
        , benchIOSink value "filter-take" (filterTake value 4)
        , benchIOSink value "filter-scan" (filterScan 4)
        , benchIOSink value "filter-scanl1" (filterScanl1 4)
        , benchIOSink value "filter-map" (filterMap value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Iterating a transformation over and over again
-------------------------------------------------------------------------------

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateScan = iterateSource (S.scanl' (+) 0)

-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateScanl1 = iterateSource (S.scanl1' (+))

{-# INLINE iterateMapM #-}
iterateMapM :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateMapM = iterateSource (S.mapM return)

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateFilterEven = iterateSource (S.filter even)

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: S.MonadAsync m => Int -> Int -> Int -> Int -> SerialT m Int
iterateTakeAll value = iterateSource (S.take (value + 1))

{-# INLINE iterateDropOne #-}
iterateDropOne :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateDropOne = iterateSource (S.drop 1)

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: S.MonadAsync m
    => Int -> Int -> Int -> Int -> SerialT m Int
iterateDropWhileFalse value = iterateSource (S.dropWhile (> (value + 1)))

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: S.MonadAsync m
    => Int -> Int -> Int -> Int -> SerialT m Int
iterateDropWhileTrue value = iterateSource (S.dropWhile (<= (value + 1)))

{-# INLINE tail #-}
tail :: Monad m => SerialT m a -> m ()
tail s = S.tail s >>= mapM_ tail

{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => SerialT m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= mapM_ nullHeadTail

-- Head recursive operations.
o_n_stack_iterated :: Int -> [Benchmark]
o_n_stack_iterated value = by10 `seq` by100 `seq`
    [ bgroup "iterated"
        [ benchIOSrc serially "mapM (n/10 x 10)" $ iterateMapM by10 10
        , benchIOSrc serially "scanl' (quadratic) (n/100 x 100)" $
            iterateScan by100 100
        , benchIOSrc serially "scanl1' (n/10 x 10)" $ iterateScanl1 by10 10
        , benchIOSrc serially "filterEven (n/10 x 10)" $
            iterateFilterEven by10 10
        , benchIOSrc serially "takeAll (n/10 x 10)" $
            iterateTakeAll value by10 10
        , benchIOSrc serially "dropOne (n/10 x 10)" $ iterateDropOne by10 10
        , benchIOSrc serially "dropWhileFalse (n/10 x 10)" $
            iterateDropWhileFalse value by10 10
        , benchIOSrc serially "dropWhileTrue (n/10 x 10)" $
            iterateDropWhileTrue value by10 10
        , benchIOSink value "tail" tail
        , benchIOSink value "nullHeadTail" nullHeadTail
        ]
    ]

    where

    by10 = value `div` 10
    by100 = value `div` 100

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

o_1_space_pipes :: Int -> [Benchmark]
o_1_space_pipes value =
    [ bgroup "pipes"
        [ benchIOSink value "mapM" (transformMapM serially 1)
        , benchIOSink value "compose" (transformComposeMapM serially 1)
        , benchIOSink value "tee" (transformTeeMapM serially 1)
#ifdef DEVBUILD
        -- XXX this take 1 GB memory to compile
        , benchIOSink value "zip" (transformZipMapM serially 1)
#endif
        ]
    ]

o_1_space_pipesX4 :: Int -> [Benchmark]
o_1_space_pipesX4 value =
    [ bgroup "pipesX4"
        [ benchIOSink value "mapM" (transformMapM serially 4)
        , benchIOSink value "compose" (transformComposeMapM serially 4)
        , benchIOSink value "tee" (transformTeeMapM serially 4)
#ifdef DEVBUILD
        -- XXX this take 1 GB memory to compile
        , benchIOSink value "zip" (transformZipMapM serially 4)
#endif
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_grouping size
            , o_1_space_transformations_mixed size
            , o_1_space_transformations_mixedX4 size

            -- pipes
            , o_1_space_pipes size
            , o_1_space_pipesX4 size
            ]
        , bgroup (o_n_stack_prefix moduleName) (o_n_stack_iterated size)
        , bgroup (o_n_heap_prefix moduleName) $ Prelude.concat
            [ o_n_space_grouping size
            , o_n_space_functor size
            , o_n_heap_buffering size
            ]
        ]
