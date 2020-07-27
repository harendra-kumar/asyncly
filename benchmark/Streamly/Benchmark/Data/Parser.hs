-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch)
import Data.Char (ord)
import Data.Word (Word8)
import Data.Foldable (asum)
import Data.Monoid (Sum(..))
import System.Random (randomRIO)
import Prelude
       hiding (any, all, take, sequence, sequence_, sequenceA, takeWhile)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Prelude as IP

import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-- | Generates something like this: { { \{ \{ } }
-- The stream has length 4 times value, the stream consists of three parts,
-- the first part is contains `value` number of `{`. Then the second part
-- contains value `\` and value `{` alternatively (the escaped part). The
-- third (and final) part contains value number of `}`.
{-# INLINE escFrameUnfold #-}
escFrameUnfold :: 
    (S.IsStream t, S.MonadAsync m)
    => Int
    -> Int
    -> t m Word8
escFrameUnfold value n = S.unfoldrM step n
    where

    bs = fromIntegral (ord '\\')
    cbOpen = fromIntegral (ord '{')
    cbClose = fromIntegral (ord '}')

    step cnt | cnt > 4 * value = return Nothing
        | cnt <= value = return $ Just (cbOpen, cnt + 1)
        | cnt > 3 * value = return $ Just (cbClose, cnt + 1)
        | otherwise = 
            if (cnt - value) `mod` 2 == 1
            then return $ Just (bs, cnt + 1)
            else return $ Just (cbOpen, cnt + 1)

{-# INLINE escFrameIOSink #-}
escFrameIOSink :: 
    (IsStream t, NFData b)
    => Int
    -> String
    -> (t IO Word8 -> IO b)
    -> Benchmark
escFrameIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . escFrameUnfold value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE any #-}
any :: (MonadCatch m, Ord a) => a -> SerialT m a -> m Bool
any value = IP.parse (PR.any (> value))

{-# INLINE all #-}
all :: (MonadCatch m, Ord a) => a -> SerialT m a -> m Bool
all value = IP.parse (PR.all (<= value))

{-# INLINE take #-}
take :: MonadCatch m => Int -> SerialT m a -> m ()
take value = IP.parse (PR.take value FL.drain)

{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> SerialT m a -> m ()
takeBetween value = IP.parse (PR.takeBetween 0 value FL.drain)

{-# INLINE takeWhile #-}
takeWhile :: MonadCatch m => Int -> SerialT m Int -> m ()
takeWhile value = IP.parse (PR.takeWhile (<= value) FL.drain)

takeWhileP :: MonadCatch m => Int -> SerialT m Int -> m ()
takeWhileP value = 
    IP.parse (PR.takeWhileP (<= value) (PR.takeWhile (\_ -> True) FL.drain))

{-# INLINE deintercalate #-}
deintercalate :: MonadCatch m => SerialT m Int -> m ((), ())
deintercalate = IP.parse prsr

    where
    
    prsr1 = PR.takeWhile odd FL.drain

    prsr2 = PR.takeWhile even FL.drain

    prsr = PR.deintercalate FL.drain prsr1 FL.drain prsr2

escapedFrameBy :: MonadCatch m => SerialT m Word8 -> m ()
escapedFrameBy strm = IP.parse prsr strm

    where

    bs = fromIntegral (ord '\\')
    isEsc = (== bs)
    cbOpen = fromIntegral (ord '{')
    isBegin = (== cbOpen)
    cbClose = fromIntegral (ord '}')
    isEnd = (== cbClose)
    
    prsr = PR.escapedFrameBy isBegin isEnd isEsc FL.drain

{-# INLINE many #-}
many :: MonadCatch m => SerialT m Int -> m Int
many = IP.parse (PR.many FL.length (PR.satisfy (> 0)))

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => SerialT m Int -> m Int
some = IP.parse (PR.some FL.length (PR.satisfy (> 0)))

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- IP.parse (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE manyTill #-}
manyTill :: MonadCatch m => Int -> SerialT m Int -> m Int
manyTill value =
    IP.parse (PR.manyTill FL.length (PR.satisfy (> 0)) (PR.satisfy (== value)))

{-# INLINE splitAp #-}
splitAp :: MonadCatch m
    => Int -> SerialT m Int -> m (Bool, Bool)
splitAp value =
    IP.parse ((,) <$> PR.all (<= (value `div` 2)) <*> PR.any (> value))

{-# INLINE splitApBefore #-}
splitApBefore :: MonadCatch m
    => Int -> SerialT m Int -> m Bool
splitApBefore value =
    IP.parse (PR.all (<= (value `div` 2)) *> PR.any (> value))

{-# INLINE splitApAfter #-}
splitApAfter :: MonadCatch m
    => Int -> SerialT m Int -> m Bool
splitApAfter value =
    IP.parse (PR.all (<= (value `div` 2)) <* PR.any (> value))

{-# INLINE splitWith #-}
splitWith :: MonadCatch m
    => Int -> SerialT m Int -> m (Bool, Bool)
splitWith value =
    IP.parse (PR.splitWith (,) (PR.all (<= (value `div` 2))) (PR.any (> value)))

{-# INLINE split_ #-}
split_ :: MonadCatch m
    => Int -> SerialT m Int -> m Bool
split_ value =
    IP.parse (PR.split_ (PR.all (<= (value `div` 2))) (PR.any (> value)))

{-# INLINE teeAllAny #-}
teeAllAny :: (MonadCatch m, Ord a)
    => a -> SerialT m a -> m (Bool, Bool)
teeAllAny value =
    IP.parse (PR.teeWith (,) (PR.all (<= value)) (PR.any (> value)))

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: (MonadCatch m, Ord a)
    => a -> SerialT m a -> m (Bool, Bool)
teeFstAllAny value =
    IP.parse (PR.teeWithFst (,) (PR.all (<= value)) (PR.any (> value)))

{-# INLINE shortestAllAny #-}
shortestAllAny :: (MonadCatch m, Ord a)
    => a -> SerialT m a -> m Bool
shortestAllAny value =
    IP.parse (PR.shortest (PR.all (<= value)) (PR.any (> value)))

{-# INLINE longestAllAny #-}
longestAllAny :: (MonadCatch m, Ord a)
    => a -> SerialT m a -> m Bool
longestAllAny value =
    IP.parse (PR.longest (PR.all (<= value)) (PR.any (> value)))

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: MonadCatch m => Int -> SerialT m Int -> m ()
lookAhead value =
    IP.parse (PR.lookAhead (PR.takeWhile (<= value) FL.drain) *> pure ())

{-# INLINE sequenceA #-}
sequenceA :: MonadCatch m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    x <- IP.parse (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequenceA_ value =
    IP.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

{-# INLINE sequence #-}
sequence :: MonadCatch m => Int -> SerialT m Int -> m Int
sequence value xs = do
    x <- IP.parse (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequence_ value =
    IP.parse (F.sequence_ $ replicate value (PR.satisfy (> 0)))

{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value =
    IP.parse (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: MonadCatch m => SerialT m Int -> m ()
parseMany =
      S.drain
    . S.map getSum
    . IP.parseMany (PR.take 2 FL.mconcat)
    . S.map Sum

{-# INLINE parseIterate #-}
parseIterate :: MonadCatch m => SerialT m Int -> m ()
parseIterate =
      S.drain
    . S.map getSum
    . IP.parseIterate (\b -> (PR.take 2 (FL.sconcat b))) (Sum 0)
    . S.map Sum

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "any" $ any value
    , benchIOSink value "all" $ all value
    , benchIOSink value "take" $ take value
    , benchIOSink value "takeBetween" $ takeBetween value
    , benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "takeWhileP" $ takeWhileP value
    , benchIOSink value "deintercalate" $ deintercalate
    , escFrameIOSink value "escapedFrameBy" $ escapedFrameBy
    , benchIOSink value "splitAp" $ splitAp value
    , benchIOSink value "splitApBefore" $ splitApBefore value
    , benchIOSink value "splitApAfter" $ splitApAfter value
    , benchIOSink value "splitWith" $ splitWith value
    , benchIOSink value "many" many
    , benchIOSink value "some" some
    , benchIOSink value "manyTill" $ manyTill value
    , benchIOSink value "tee" $ teeAllAny value
    , benchIOSink value "teeFst" $ teeFstAllAny value
    , benchIOSink value "shortest" $ shortestAllAny value
    , benchIOSink value "longest" $ longestAllAny value
    , benchIOSink value "parseMany" parseMany
    , benchIOSink value "parseIterate" parseIterate
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- lookahead benchmark holds the entire input till end
      benchIOSink value "lookAhead" $ lookAhead value

    -- accumulates the results in a list
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "sequenceA" $ sequenceA value

    -- XXX why should this take O(n) heap, it discards the results?
    , benchIOSink value "sequence_" $ sequence_ value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    -- non-linear time complexity (parserD)
    , benchIOSink value "split_" $ split_ value
    -- XXX why O(n) heap?
    , benchIOSink value "choice" $ choice value

    -- These show non-linear time complexity.
    -- They accumulate the results in a list.
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [
              o_1_space_serial value
            ]
        , bgroup (o_n_heap_prefix moduleName) $ concat
            [
              o_n_heap_serial value
            ]
        ]
