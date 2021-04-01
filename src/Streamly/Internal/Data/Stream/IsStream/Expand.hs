-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Expand
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Expand a stream by combining two or more streams or by combining streams
-- with unfolds.

module Streamly.Internal.Data.Stream.IsStream.Expand
    (
    -- * Binary Combinators (Linear)
    -- | Functions ending in the shape:
    --
    -- @t m a -> t m a -> t m a@.
    --
    -- The functions in this section have a linear or flat n-ary combining
    -- characterstics. It means that when combined @n@ times (e.g. @a `serial`
    -- b `serial` c ...@) the resulting expression will have an @O(n)@
    -- complexity (instead O(n^2) for pair wise combinators described in the
    -- next section. These functions can be used efficiently with
    -- 'concatMapWith' et. al.  combinators that combine streams in a linear
    -- fashion (contrast with 'concatPairsWith' which combines streams as a
    -- binary tree).

      serial
    , ahead
    , async
    , wAsync
    , parallel
    , Par.parallelFst
    , Par.parallelMin

    -- * Binary Combinators (Pair Wise)
    -- | Like the functions in the section above these functions also combine
    -- two streams into a single stream but when used @n@ times linearly they
    -- exhibit O(n^2) complexity. They are best combined in a binary tree
    -- fashion using 'concatPairsWith' giving a @n * log n@ complexity.  Avoid
    -- using these with 'concatMapWith' when combining a large or infinite
    -- number of streams.

    -- ** Append
    , append

    -- ** wSerial
    -- | 'wSerial' is a CPS based stream interleaving functions. It can be
    -- used with 'concatMapWith' as well, however, the interleaving behavior of
    -- @n@ streams would be asymmetric giving exponentially more weightage to
    -- streams that come earlier in the composition.
    --
    , wSerial
    , Serial.wSerialFst
    , Serial.wSerialMin

    -- ** Interleave
    -- | 'interleave' is like 'wSerial'  but using a direct style
    -- implementation instead of CPS. It is faster than 'wSerial' due to stream
    -- fusion but has worse efficiency when used with 'concatMapWith' for large
    -- number of streams.
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    -- ** Round Robin
    , roundrobin

    -- ** Zip
    , Z.zipWith
    , Z.zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- ** Merge
    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- * Combine Streams and Unfolds
    -- |
    -- Expand a stream by repeatedly using an unfold and merging the resulting
    -- streams.  Functions generally ending in the shape:
    --
    -- @Unfold m a b -> t m a -> t m b@

    -- ** Append Many (Unfold)
    -- | Unfold and flatten streams.
    , unfoldMany
    , unfoldManyInterleave
    , unfoldManyRoundRobin

    -- ** Interpose
    -- | Insert effects between streams. Like unfoldMany but intersperses an
    -- effect between the streams. A special case of gintercalate.
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- ** Intercalate
    -- | Insert Streams between Streams.
    -- Like unfoldMany but intersperses streams from another source between
    -- the streams from the first source.
    , intercalate
    , intercalateSuffix
    , gintercalate
    , gintercalateSuffix

    -- * Append Many (concatMap)
    -- | Map and serially append streams. 'concatMapM' is a generalization of
    -- the binary append operation to append many streams.
    , concatMapM
    , concatMap
    , concatM
    , concat

    -- * Flatten Containers
    -- | Flatten 'Foldable' containers using the binary stream merging
    -- operations.
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- * ConcatMapWith
    -- | Map and flatten a stream like 'concatMap' but using a custom binary
    -- stream merging combinator instead of just appending the streams.  The
    -- merging occurs sequentially, it works efficiently for 'serial', 'async',
    -- 'ahead' like merge operations where we consume one stream before the
    -- next or in case of 'wAsync' or 'parallel' where we consume all streams
    -- simultaneously anyway.
    --
    -- However, in cases where the merging consumes streams in a round robin
    -- fashion, a pair wise merging using 'concatPairsWith' would be more
    -- efficient. These cases include operations like 'mergeBy' or 'zipWith'.

    , concatMapWith
    , K.bindWith
    , concatSmapMWith

    -- * ConcatPairsWith
    -- | See the notes about suitable merge functions in the 'concatMapWith'
    -- section.
    , concatPairsWith
    , concatPairsWithD

    -- * IterateMap
    -- | Map and flatten Trees of Streams
    , iterateMapWith
    , iterateSmapMWith
    , iterateMapLeftsWith

    -- * Deprecated
    , concatUnfold
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Ahead (ahead)
import Streamly.Internal.Data.Stream.Async (async, wAsync)
import Streamly.Internal.Data.Stream.IsStream.Common
    (concatM, concatMapM, concatMap, smapM, yield, yieldM)
import Streamly.Internal.Data.Stream.Parallel (parallel)
import Streamly.Internal.Data.Stream.Prelude
       ( concatFoldableWith, concatMapFoldableWith
       , concatForFoldableWith, fromStreamS, toStreamS)
import Streamly.Internal.Data.Stream.Serial (serial, wSerial)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream)
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Internal.Data.Unfold.Type (Unfold)

import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif
import qualified Streamly.Internal.Data.Stream.Zip as Z

import Prelude hiding (concat, concatMap)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Array.Foreign as Array

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

-- XXX Reconcile the names "serial" and "append".
--
-- | Append the outputs of two streams, yielding all the elements from the
-- first stream and then yielding all the elements from the second stream.
--
-- IMPORTANT NOTE: This could be 100x faster than @serial/<>@ for appending a
-- few (say 100) streams because it can fuse via stream fusion. However, it
-- does not scale for a large number of streams (say 1000s) and becomes
-- qudartically slow. Therefore use this for custom appending of a few streams
-- but use 'concatMap' or 'concatMapWith serial' for appending @n@ streams or
-- infinite containers of streams.
--
-- /Pre-release/
{-# INLINE append #-}
append ::(IsStream t, Monad m) => t m b -> t m b -> t m b
append m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

-- XXX Same as 'wSerial'. We should perhaps rename wSerial to interleave.
-- XXX Document the interleaving behavior of side effects in all the
-- interleaving combinators.
-- XXX Write time-domain equivalents of these. In the time domain we can
-- interleave two streams such that the value of second stream is always taken
-- from its last value even if no new value is being yielded, like
-- zipWithLatest. It would be something like interleaveWithLatest.
--
-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. If any of the streams finishes
-- early the other stream continues alone until it too finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleave "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,,,"
-- >>> Stream.interleave "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,cd"
--
-- 'interleave' is dual to 'interleaveMin', it can be called @interleaveMax@.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleave #-}
interleave ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleave m1 m2 = fromStreamD $ D.interleave (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. As soon as the first stream
-- finishes, the output stops, discarding the remaining part of the second
-- stream. In this case, the last element in the resulting stream would be from
-- the second stream. If the second stream finishes early then the first stream
-- still continues to yield elements until it finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveSuffix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c,"
-- >>> Stream.interleaveSuffix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveSuffix' is a dual of 'interleaveInfix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveSuffix #-}
interleaveSuffix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveSuffix m1 m2 =
    fromStreamD $ D.interleaveSuffix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream and ending at the first stream.
-- If the second stream is longer than the first, elements from the second
-- stream are infixed with elements from the first stream. If the first stream
-- is longer then it continues yielding elements even after the second stream
-- has finished.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveInfix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
-- >>> Stream.interleaveInfix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveInfix' is a dual of 'interleaveSuffix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveInfix #-}
interleaveInfix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveInfix m1 m2 =
    fromStreamD $ D.interleaveInfix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. The output stops as soon as any
-- of the two streams finishes, discarding the remaining part of the other
-- stream. The last element of the resulting stream would be from the longer
-- stream.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveMin "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,"
-- >>> Stream.interleaveMin "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
--
-- 'interleaveMin' is dual to 'interleave'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveMin #-}
interleaveMin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveMin m1 m2 = fromStreamD $ D.interleaveMin (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Scheduling
-------------------------------------------------------------------------------

-- | Schedule the execution of two streams in a fair round-robin manner,
-- executing each stream once, alternately. Execution of a stream may not
-- necessarily result in an output, a stream may chose to @Skip@ producing an
-- element until later giving the other stream a chance to run. Therefore, this
-- combinator fairly interleaves the execution of two streams rather than
-- fairly interleaving the output of the two streams. This can be useful in
-- co-operative multitasking without using explicit threads. This can be used
-- as an alternative to `async`.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE roundrobin #-}
roundrobin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
roundrobin m1 m2 = fromStreamD $ D.roundRobin (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Merging (sorted streams)
------------------------------------------------------------------------------

-- | Merge two streams using a comparison function. The head elements of both
-- the streams are compared and the smaller of the two elements is emitted, if
-- both elements are equal then the element from the first stream is used
-- first.
--
-- If the streams are sorted in ascending order, the resulting stream would
-- also remain sorted in ascending order.
--
-- @
-- >>> Stream.toList $ Stream.mergeBy compare (Stream.fromList [1,3,5]) (Stream.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
--
-- @
--
-- @since 0.6.0
{-# INLINABLE mergeBy #-}
mergeBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy f m1 m2 = fromStreamS $ S.mergeBy f (toStreamS m1) (toStreamS m2)

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > S.toList $ S.mergeByM randomly (S.fromList [1,1,1,1]) (S.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Merge two streams in a proportion of 2:1:
--
-- @
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m LT, replicate n GT]
--  return $ \\_ _ -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ head r
--
-- main = do
--  f <- proportionately 2 1
--  xs <- S.toList $ S.mergeByM f (S.fromList [1,1,1,1,1,1]) (S.fromList [2,2,2])
--  print xs
-- @
-- @
-- [1,1,2,1,1,2,1,1,2]
-- @
--
-- @since 0.6.0
{-# INLINABLE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM f m1 m2 = fromStreamS $ S.mergeByM f (toStreamS m1) (toStreamS m2)

{-
-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
{-# INLINABLE mergeEndByAny #-}
mergeEndByAny
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeEndByAny f m1 m2 = fromStreamD $
    D.mergeEndByAny f (toStreamD m1) (toStreamD m2)

-- Like 'mergeByM' but stops merging as soon as the first stream stops.
{-# INLINABLE mergeEndByFirst #-}
mergeEndByFirst
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeEndByFirst f m1 m2 = fromStreamS $
    D.mergeEndByFirst f (toStreamD m1) (toStreamD m2)

-- Holding this back for now, we may want to use the name "merge" differently
-- | Same as @'mergeBy' 'compare'@.
--
-- @
-- > S.toList $ S.merge (S.fromList [1,3,5]) (S.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
-- @
--
-- @since 0.6.0
{-# INLINABLE merge #-}
merge ::
       (IsStream t, Monad m, Ord a) => t m a -> t m a -> t m a
merge = mergeBy compare
-}

-- | Like 'mergeBy' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (IsStream t, MonadAsync m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeAsyncBy f = mergeAsyncByM (\a b -> return $ f a b)

-- | Like 'mergeByM' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (IsStream t, MonadAsync m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeAsyncByM f m1 m2 =
    fromStreamD $
        let par = Par.mkParallelD . toStreamD
        in D.mergeByM f (par m1) (par m2)


-- @since 0.7.0
{-# DEPRECATED concatUnfold "Please use unfoldMany instead." #-}
{-# INLINE concatUnfold #-}
concatUnfold ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
concatUnfold u m = fromStreamD $ D.unfoldMany u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - unfoldMany
------------------------------------------------------------------------------

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
-- @since 0.8.0
{-# INLINE unfoldMany #-}
unfoldMany ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
unfoldMany u m = fromStreamD $ D.unfoldMany u (toStreamD m)

-- | Like 'unfoldMany' but interleaves the streams in the same way as
-- 'interleave' behaves instead of appending them.
--
-- /Pre-release/
{-# INLINE unfoldManyInterleave #-}
unfoldManyInterleave ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
unfoldManyInterleave u m =
    fromStreamD $ D.unfoldManyInterleave u (toStreamD m)

-- | Like 'unfoldMany' but executes the streams in the same way as
-- 'roundrobin'.
--
-- /Pre-release/
{-# INLINE unfoldManyRoundRobin #-}
unfoldManyRoundRobin ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
unfoldManyRoundRobin u m =
    fromStreamD $ D.unfoldManyRoundRobin u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- > unwords = S.interpose ' '
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interpose x unf str =
    D.fromStreamD $ D.interpose (return x) unf (D.toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- > unlines = S.interposeSuffix '\n'
--
-- /Pre-release/
{-# INLINE interposeSuffix #-}
interposeSuffix :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interposeSuffix x unf str =
    D.fromStreamD $ D.interposeSuffix (return x) unf (D.toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - intercalate
------------------------------------------------------------------------------

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of unfoldMany becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > unfoldMany unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())
--
-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalate #-}
gintercalate
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalate unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalate
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- XXX The order of arguments in "intercalate" is consistent with the list
-- intercalate but inconsistent with gintercalate and other stream interleaving
-- combinators. We can change the order of the arguments in other combinators
-- but then 'interleave' combinator may become a bit unintuitive because we
-- will be starting with the second stream.

-- > intercalate seed unf str = gintercalate unf str unf (repeatM seed)
-- > intercalate a unf str = unfoldMany unf $ intersperse a str
--
-- | 'intersperse' followed by unfold and concat.
--
-- > unwords = intercalate " " Unfold.fromList
--
-- >>> Stream.toList $ Stream.intercalate " " Unfold.fromList $ Stream.fromList ["abc", "def", "ghi"]
-- "abc def ghi"
--
-- /Pre-release/
{-# INLINE intercalate #-}
intercalate :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalate seed unf str = D.fromStreamD $
    D.unfoldMany unf $ D.intersperse seed (toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalateSuffix unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalateSuffix
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- > intercalateSuffix seed unf str = gintercalateSuffix unf str unf (repeatM seed)
-- > intercalateSuffix a unf str = unfoldMany unf $ intersperseSuffix a str
--
-- | 'intersperseSuffix' followed by unfold and concat.
--
-- > unlines = intercalateSuffix "\n" Unfold.fromList
--
-- >>> Stream.toList $ Stream.intercalateSuffix "\n" Unfold.fromList $ Stream.fromList ["abc", "def", "ghi"]
-- "abc\ndef\nghi\n"
--
-- /Pre-release/
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalateSuffix seed unf str = fromStreamD $ D.unfoldMany unf
    $ D.intersperseSuffix (return seed) (D.toStreamD str)

{-
{-# INLINE iterateUnfold #-}
iterateUnfold :: (IsStream t, MonadAsync m)
    => Unfold m a a -> t m a -> t m a
iterateUnfold unf xs = undefined
-}

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Flatten a stream of streams to a single stream.
--
-- @
-- concat = concatMap id
-- @
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: (IsStream t, Monad m) => t m (t m a) -> t m a
concat = concatMap id

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | @concatMapWith mixer generator stream@ is a two dimensional looping
-- combinator.  The @generator@ function is used to generate streams from the
-- elements in the input @stream@ and the @mixer@ function is used to merge
-- those streams.
--
-- Note we can merge streams concurrently by using a concurrent merge function.
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE concatMapWith #-}
concatMapWith
    :: IsStream t
    => (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatMapWith = K.concatMapBy

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- @
-- concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial
-- @
--
-- /Pre-release/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: (IsStream t, Monad m)
    => (t m b -> t m b -> t m b)
    -> (s -> a -> m (s, t m b))
    -> m s
    -> t m a
    -> t m b
concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial

-- Keep concating either streams as long as rights are generated, stop as soon
-- as a left is generated and concat the left stream.
--
-- See also: 'handle'
--
-- /Unimplemented/
--
{-
concatMapEitherWith
    :: -- (IsStream t, MonadAsync m) =>
       (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (t m b) b))
    -> t m a
    -> t m b
concatMapEitherWith = undefined
-}

-- XXX Implement a StreamD version for fusion.
--
-- | Combine streams in pairs using a binary stream combinator, then combine
-- the resulting streams in pairs recursively until we get to a single combined
-- stream.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> Stream.toList $ Stream.concatPairsWith (Stream.mergeBy compare) Stream.yield $ Stream.fromList [5,1,7,9,2]
-- [1,2,5,7,9]
--
-- /Caution: the stream of streams must be finite/
--
-- /Pre-release/
--
{-# INLINE concatPairsWith #-}
concatPairsWith :: IsStream t =>
       (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatPairsWith = K.concatPairsWith

{-# INLINE concatPairsWithD #-}
concatPairsWithD :: (IsStream t , Monad m) =>
       (S.Stream m b -> S.Stream m b -> S.Stream m b)
    -> (a -> S.Stream m b)
    -> t m a
    -> t m b
concatPairsWithD c f z = D.fromStreamD $ S.concatPairsWith c f (D.toStreamD z)

------------------------------------------------------------------------------
-- IterateMap - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Like 'iterateM' but iterates after mapping a stream generator on the
-- output.
--
-- Yield an input element in the output stream, map a stream generator on it
-- and then do the same on the resulting stream. This can be used for a depth
-- first traversal of a tree like structure.
--
-- Note that 'iterateM' is a special case of 'iterateMapWith':
--
-- @
-- iterateM f = iterateMapWith serial (yieldM . f) . yieldM
-- @
--
-- It can be used to traverse a tree structure.  For example, to list a
-- directory tree:
--
-- @
-- Stream.iterateMapWith Stream.serial
--     (either Dir.toEither (const nil))
--     (yield (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapWith #-}
iterateMapWith
    :: IsStream t
    => (t m a -> t m a -> t m a)
    -> (a -> t m a)
    -> t m a
    -> t m a
iterateMapWith combine f = concatMapWith combine go
    where
    go x = yield x `combine` concatMapWith combine go (f x)

{-
{-# INLINE iterateUnfold #-}
iterateUnfold :: (IsStream t, MonadAsync m)
    => Unfold m a a -> t m a -> t m a
iterateUnfold unf xs = undefined
-}

------------------------------------------------------------------------------
-- Flattening Graphs
------------------------------------------------------------------------------

-- To traverse graphs we need a state to be carried around in the traversal.
-- For example, we can use a hashmap to store the visited status of nodes.

-- | Like 'iterateMap' but carries a state in the stream generation function.
-- This can be used to traverse graph like structures, we can remember the
-- visited nodes in the state to avoid cycles.
--
-- Note that a combination of 'iterateMap' and 'usingState' can also be used to
-- traverse graphs. However, this function provides a more localized state
-- instead of using a global state.
--
-- See also: 'mfix'
--
-- /Pre-release/
--
{-# INLINE iterateSmapMWith #-}
iterateSmapMWith
    :: (IsStream t, Monad m)
    => (t m a -> t m a -> t m a)
    -> (b -> a -> m (b, t m a))
    -> m b
    -> t m a
    -> t m a
iterateSmapMWith combine f initial stream =
    concatMap (\b -> concatMapWith combine (go b) stream) (yieldM initial)

    where

    go b a = yield a `combine` feedback b a

    feedback b a =
        concatMap
            (\(b1, s) -> concatMapWith combine (go b1) s)
            (yieldM $ f b a)

------------------------------------------------------------------------------
-- iterateMap - Either streams
------------------------------------------------------------------------------

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'iterateMapWith':
--
-- @
-- iterateMapLeftsWith combine f = iterateMapWith combine (either f (const nil))
-- @
--
-- To traverse a directory tree:
--
-- @
-- iterateMapLeftsWith serial Dir.toEither (yield (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: IsStream t
    => (t m (Either a b) -> t m (Either a b) -> t m (Either a b))
    -> (a -> t m (Either a b))
    -> t m (Either a b)
    -> t m (Either a b)
iterateMapLeftsWith combine f = iterateMapWith combine (either f (const K.nil))
