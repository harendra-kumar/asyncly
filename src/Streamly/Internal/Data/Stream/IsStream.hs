{-# OPTIONS_GHC -Wno-orphans  #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is an Internal module consisting of released, unreleased and
-- unimplemented APIs. For stable and released APIs please see
-- "Streamly.Prelude" module. This module provides documentation only for the
-- unreleased and unimplemented APIs. For documentation on released APIs please
-- see "Streamly.Prelude" module.

module Streamly.Internal.Data.Stream.IsStream
    (
    -- * Stream Types
    -- ** Serial Streams
      SerialT
    , Serial
    , WSerialT
    , WSerial

    -- ** Speculative Streams
    , AheadT
    , Ahead

    -- ** Asynchronous Streams
    , AsyncT
    , Async
    , WAsyncT
    , WAsync
    , ParallelT
    , Parallel
    , mkAsync

    -- ** Zipping Streams
    , ZipSerialM
    , ZipSerial
    , ZipAsyncM
    , ZipAsync

    -- * Stream Type Adapters
    , IsStream ()

    , serially
    , wSerially
    , asyncly
    , aheadly
    , wAsyncly
    , parallely
    , zipSerially
    , zipAsyncly
    , adapt

    -- * Type Synonyms
    , MonadAsync

    -- * Construction
    -- ** Primitives
    , K.nil
    , K.nilM
    , K.cons
    , (K..:)

    , consM
    , (|:)

    -- ** From Values
    , yield
    , yieldM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- ** Enumeration
    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- ** Time Enumeration
    , times
    , absTimes
    , relTimes
    , durations
    , ticks
    , timeout
    , currentTime

    -- ** From Generators
    , unfoldr
    , unfoldrM
    , unfold
    , unfold0
    , fromIndices
    , fromIndicesM

    -- ** Iteration
    , iterate
    , iterateM

    -- ** Cyclic Elements
    , K.mfix

    -- ** From Containers
    , P.fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM
    , fromPrimIORef
    , fromCallback

    -- * Elimination

    -- ** Deconstruction
    , uncons
    , tail
    , init

    -- ** Folding
    -- ** Right Folds
    , foldrM
    , foldrS
    , foldrT
    , foldr

    -- ** Left Folds
    , foldl'
    , foldl1'
    , foldlM'

    -- ** Composable Left Folds
    , fold
    , parse
    , parseK
    , parseD

    -- ** Concurrent Folds
    , foldAsync
    , (|$.)
    , (|&.)

    -- ** Full Folds

    -- -- ** To Summary (Full Folds)
    , drain
    , last
    , length
    , sum
    , product
    , mconcat

    -- -- ** To Summary (Maybe) (Full Folds)
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- ** Lazy Folds
    -- -- ** To Containers (Full Folds)
    , toList
    , toListRev
    , toPure
    , toPureRev

    -- ** Composable Left Folds

    , toStream    -- XXX rename to write?
    , toStreamRev -- XXX rename to writeRev?

    -- ** Partial Folds

    -- -- ** To Elements (Partial Folds)
    , drainN
    , drainWhile

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , headElse
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex

    -- -- ** To Boolean (Partial Folds)
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isSuffixOf
    , isInfixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    , stripSuffix
    -- , stripInfix
    , dropPrefix
    , dropInfix
    , dropSuffix

    -- * Transformation
    , transform

    -- ** Mapping
    , Serial.map
    , sequence
    , mapM
    , smapM
    -- $smapM_Notes

    -- ** Special Maps
    , mapM_
    , trace
    , trace_
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts

    -- ** Scanning
    -- ** Left scans
    , scanl'
    , scanlM'
    , scanlMAfter'
    , postscanl'
    , postscanlM'
    , prescanl'
    , prescanlM'
    , scanl1'
    , scanl1M'

    -- ** Scan Using Fold
    , scan
    , postscan

    -- XXX Once we have pipes the contravariant transformations can be
    -- represented by attaching pipes before a transformation.
    --
    -- , lscanl'
    -- , lscanlM'
    -- , lscanl1'
    -- , lscanl1M'
    --
    -- , lpostscanl'
    -- , lpostscanlM'
    -- , lprescanl'
    -- , lprescanlM'

    -- ** Concurrent Transformation
    , D.mkParallel
    -- Par.mkParallel
    , applyAsync
    , (|$)
    , (|&)

    -- ** Filtering

    , filter
    , filterM

    -- ** Deleting Elements
    , deleteBy
    , uniq
    -- , uniqBy -- by predicate e.g. to remove duplicate "/" in a path
    -- , uniqOn -- to remove duplicate sequences
    -- , pruneBy -- dropAround + uniqBy - like words

    -- ** Inserting Elements

    , insertBy
    , intersperseM
    , intersperseM_
    , intersperse
    , intersperseSuffix
    , intersperseSuffix_
    , interspersePrefix_
    , intersperseSuffixBySpan
    -- , intersperseBySpan
    -- , intersperseByIndices -- using an index function/stream

    -- time domain intersperse
    -- , intersperseByTime
    -- , intersperseByEvent
    , interjectSuffix
    , delay
    , delayPost
    , delayPre

    -- ** Indexing
    , indexed
    , indexedR
    , timestamped
    , timeIndexed
    -- , timestampedR -- timer

    -- ** Reordering
    , reverse
    , reverse'

    -- ** Parsing
    , parseMany
    , parseManyD
    , parseManyTill
    , parseIterate

    -- ** Trimming
    , take
    -- , takeGE
    -- , takeBetween
    , takeByTime
    -- , takeEnd
    , takeWhile
    , takeWhileM
    -- , takeWhileEnd
    , drop
    , dropByTime
    -- , dropEnd
    , dropWhile
    , dropWhileM
    -- , dropWhileEnd
    -- , dropAround

    -- ** Breaking

    -- Nary
    , chunksOf
    , chunksOf2
    , arraysOf
    , intervalsOf

    -- ** Searching
    -- -- *** Searching Elements
    , findIndices
    , elemIndices

    -- -- *** Searching Sequences
    -- , seqIndices -- search a sequence in the stream

    -- -- *** Searching Multiple Sequences
    -- , seqIndicesAny -- search any of the given sequence in the stream

    -- -- -- ** Searching Streams
    -- -- | Finding a stream within another stream.

    -- ** Splitting
    -- | Streams can be sliced into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- -- *** Using Element Separators
    , splitOn
    , splitOnSuffix
    -- , splitOnPrefix

    -- , splitBy
    , splitWithSuffix
    -- , splitByPrefix
    , wordsBy -- stripAndCompactBy

    -- -- *** Splitting By Sequences
    , splitOnSeq
    , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    , splitBySeq
    , splitWithSuffixSeq
    -- , splitByPrefixSeq
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    -- , splitOnAnySuffixSeq
    -- , splitOnAnyPrefixSeq

    -- -- *** Splitting By Streams
    -- -- | Splitting a stream using another stream as separator.

    -- Nested splitting
    , splitInnerBy
    , splitInnerBySuffix

    -- ** Grouping
    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- , groupScan

    , groups
    , groupsBy
    , groupsByRolling

    -- ** Group map
    , rollingMapM
    , rollingMap

    -- * Windowed Classification

    -- | Split the stream into windows or chunks in space or time. Each window
    -- can be associated with a key, all events associated with a particular
    -- key in the window can be folded to a single result. The stream is split
    -- into windows of specified size, the window can be terminated early if
    -- the closing flag is specified in the input stream.
    --
    -- The term "chunk" is used for a space window and the term "session" is
    -- used for a time window.

    -- ** Tumbling Windows
    -- | A new window starts after the previous window is finished.

    -- , classifyChunksOf
    , classifySessionsBy
    , classifySessionsOf

    -- ** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.

    -- , classifyKeepAliveChunks
    , classifyKeepAliveSessions

    {-
    -- ** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -}
    -- ** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer

    -- * Combining Streams

    -- ** Appending
    , serial
    , append

    -- ** Interleaving
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    , wSerial
    , Serial.wSerialFst
    , Serial.wSerialMin

    -- ** Scheduling
    , ahead
    , async
    , wAsync
    , roundrobin

    -- ** Parallel
    , parallel
    , Par.parallelFst
    , Par.parallelMin

    -- ** Merging
    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- ** Zipping
    , Z.zipWith
    , Z.zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- ** Flattening a Container of Streams
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- ** Flattening a Stream of Streams
    , concat
    , concatM
    , concatMap
    , concatMapM
    , concatMapWith
    , concatSmapMWith
    -- , bindWith

    -- ** Flattening Using Unfolds
    , concatUnfold
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin

    -- ** Flattening a Tree of Streams
    , iterateMapWith

    -- ** Flattening a Graph of Streams
    , iterateSmapMWith

    -- ** Inserting Streams in Streams
    , gintercalate
    , gintercalateSuffix
    , intercalate
    , intercalateSuffix
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- * Exceptions
    , before
    , after_
    , after
    , bracket_
    , bracket
    , onException
    , finally_
    , finally
    , handle
    , retry

    -- * Generalize Inner Monad
    , hoist
    , generally

    -- * Transform Inner Monad
    , liftInner
    , usingReaderT
    , runReaderT
    , evalStateT
    , usingStateT
    , runStateT

    -- * Maybe Streams
    , catMaybes
    , mapMaybe
    , mapMaybeM

    -- * Either Streams
    , lefts
    , rights
    , iterateMapLeftsWith

    -- * Concurrency Control
    , maxThreads
    , maxBuffer

    -- * Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Diagnostics
    , inspectMode

    -- * Deprecated
    , K.once
    , each
    , scanx
    , foldx
    , foldxM
    , foldr1
    , runStream
    , runN
    , runWhile
    , fromHandle
    , toHandle
    )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, assert)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Heap (Entry(..))
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Map.Strict (Map)
import Data.Void (Void)
import Foreign.Storable (Storable)
import Streamly.Internal.BaseCompat (fromLeft, fromRight)
import Streamly.Internal.Data.Stream.Enumeration
       (Enumerable(..), enumerate, enumerateTo)
import Streamly.Internal.Data.Fold.Types (Fold (..), Fold2 (..))
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.Unfold.Types (Unfold)
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array, writeNUnsafe)
import Streamly.Internal.Data.SVar (MonadAsync, defState, Rate (..))
import Streamly.Internal.Data.Stream.Ahead (AheadT, Ahead, ahead, aheadly)
import Streamly.Internal.Data.Stream.Async
       ( AsyncT, Async, WAsyncT, WAsync, mkAsync, async, asyncly, wAsync
       , wAsyncly)
import Streamly.Internal.Data.Stream.Combinators
      ( inspectMode, maxBuffer, maxThreads, maxYields, rate, avgRate, minRate
      , maxRate, constRate)
import Streamly.Internal.Data.Stream.Parallel
       ( ParallelT, Parallel, parallel, parallely)
import Streamly.Internal.Data.Stream.Prelude
       (fromStreamS, toStreamS, concatFoldableWith, concatMapFoldableWith
       , concatForFoldableWith)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream((|:), consM), adapt)
import Streamly.Internal.Data.Stream.Serial
       ( SerialT, WSerialT, Serial, WSerial, serial, wSerial, serially
       , wSerially)
import Streamly.Internal.Data.Stream.Zip
       ( ZipSerialM, ZipSerial, ZipAsyncM, ZipAsync, zipSerially, zipAsyncly)
import Streamly.Internal.Data.Pipe.Types (Pipe (..))
import Streamly.Internal.Data.Time.Units
       ( AbsTime, MilliSecond64(..), addToAbsTime, toRelTime
       , toAbsTime, TimeUnit64, RelTime64, addToAbsTime64)
import Streamly.Internal.Data.IORef.Prim (Prim, IORef)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Zip as Z
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PRK
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Data.Heap as H
import qualified Data.Map.Strict as Map
import qualified Prelude
import qualified System.IO as IO
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith, foldr
       , foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, foldr1, (!!)
       , scanl, scanl1, replicate, concatMap, span, splitAt, break
       , repeat, concat, mconcat)

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- This is a brute force primitive. Avoid using it as long as possible, use it
-- when no other combinator can do the job. This can be used to do pretty much
-- anything in an imperative manner, as it just breaks down the stream into
-- individual elements and we can loop over them as we deem fit. For example,
-- this can be used to convert a streamly stream into other stream types.
--
-- @since 0.1.0
{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons m = K.uncons (K.adapt m)

------------------------------------------------------------------------------
-- Generation by Unfolding
------------------------------------------------------------------------------

-- |
-- @
-- unfoldr step s =
--     case step s of
--         Nothing -> 'K.nil'
--         Just (a, b) -> a \`cons` unfoldr step b
-- @
--
-- Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- @
-- let f b =
--         if b > 3
--         then Nothing
--         else Just (b, b + 1)
-- in toList $ unfoldr f 0
-- @
-- @
-- [0,1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStreamS (S.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
    forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
-- When run concurrently, the next unfold step can run concurrently with the
-- processing of the output of the previous step.  Note that more than one step
-- cannot run concurrently as the next step depends on the output of the
-- previous step.
--
-- @
-- (asyncly $ S.unfoldrM (\\n -> liftIO (threadDelay 1000000) >> return (Just (n, n + 1))) 0)
--     & S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.0/
{-# INLINE_EARLY unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE_EARLY unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrMSerial = Serial.unfoldrM

{-# RULES "unfoldrM wSerial" unfoldrM = unfoldrMWSerial #-}
{-# INLINE_EARLY unfoldrMWSerial #-}
unfoldrMWSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> WSerialT m a
unfoldrMWSerial = Serial.unfoldrM

{-# RULES "unfoldrM zipSerial" unfoldrM = unfoldrMZipSerial #-}
{-# INLINE_EARLY unfoldrMZipSerial #-}
unfoldrMZipSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> ZipSerialM m a
unfoldrMZipSerial = Serial.unfoldrM

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> unfold (UF.replicateM 10) (putStrLn "hello")
--
-- /Since: 0.7.0/
{-# INLINE unfold #-}
unfold :: (IsStream t, Monad m) => Unfold m a b -> a -> t m b
unfold unf x = fromStreamD $ D.unfold unf x

-- | Convert an 'Unfold' with a closed input end into a stream.
--
-- /Internal/
{-# INLINE unfold0 #-}
unfold0 :: (IsStream t, Monad m) => Unfold m Void b -> t m b
unfold0 unf = unfold unf (error "unfold0: unexpected void evaluation")

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

-- Faster than yieldM because there is no bind.
--
-- |
-- @
-- yield a = a \`cons` nil
-- @
--
-- Create a singleton stream from a pure value.
--
-- The following holds in monadic streams, but not in Zip streams:
--
-- @
-- yield = pure
-- yield = yieldM . pure
-- @
--
-- In Zip applicative streams 'yield' is not the same as 'pure' because in that
-- case 'pure' is equivalent to 'repeat' instead. 'yield' and 'pure' are
-- equally efficient, in other cases 'yield' may be slightly more efficient
-- than the other equivalent definitions.
--
-- @since 0.4.0
{-# INLINE yield #-}
yield :: IsStream t => a -> t m a
yield = K.yield

-- |
-- @
-- yieldM m = m \`consM` nil
-- @
--
-- Create a singleton stream from a monadic action.
--
-- @
-- > toList $ yieldM getLine
-- hello
-- ["hello"]
-- @
--
-- @since 0.4.0
{-# INLINE yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM = K.yieldM

-- |
-- @
-- fromIndices f = let g i = f i \`cons` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a function @f@
-- applied on the corresponding index.  Index starts at 0.
--
-- @
-- > S.toList $ S.take 5 $ S.fromIndices id
-- [0,1,2,3,4]
-- @
--
-- @since 0.6.0
{-# INLINE fromIndices #-}
fromIndices :: (IsStream t, Monad m) => (Int -> a) -> t m a
fromIndices = fromStreamS . S.fromIndices

--
-- |
-- @
-- fromIndicesM f = let g i = f i \`consM` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a monadic
-- function @f@ applied on the corresponding index. Index starts at 0.
--
-- /Concurrent/
--
-- @since 0.6.0
{-# INLINE_EARLY fromIndicesM #-}
fromIndicesM :: (IsStream t, MonadAsync m) => (Int -> m a) -> t m a
fromIndicesM = K.fromIndicesM

{-# RULES "fromIndicesM serial" fromIndicesM = fromIndicesMSerial #-}
{-# INLINE fromIndicesMSerial #-}
fromIndicesMSerial :: MonadAsync m => (Int -> m a) -> SerialT m a
fromIndicesMSerial = fromStreamS . S.fromIndicesM

-- |
-- @
-- replicateM = take n . repeatM
-- @
--
-- Generate a stream by performing a monadic action @n@ times. Same as:
--
-- @
-- drain $ serially $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent/
--
-- @since 0.1.1
{-# INLINE_EARLY replicateM #-}
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM = K.replicateM

{-# RULES "replicateM serial" replicateM = replicateMSerial #-}
{-# INLINE replicateMSerial #-}
replicateMSerial :: MonadAsync m => Int -> m a -> SerialT m a
replicateMSerial n = fromStreamS . S.replicateM n

-- |
-- @
-- replicate = take n . repeat
-- @
--
-- Generate a stream of length @n@ by repeating a value @n@ times.
--
-- @since 0.6.0
{-# INLINE_NORMAL replicate #-}
replicate :: (IsStream t, Monad m) => Int -> a -> t m a
replicate n = fromStreamS . S.replicate n

-- |
-- Generate an infinite stream by repeating a pure value.
--
-- @since 0.4.0
{-# INLINE_NORMAL repeat #-}
repeat :: (IsStream t, Monad m) => a -> t m a
repeat = fromStreamS . S.repeat

-- |
-- @
-- repeatM = fix . consM
-- repeatM = cycle1 . yieldM
-- @
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- @
-- drain $ serially $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent, infinite (do not use with 'parallely')/
--
-- @since 0.2.0
{-# INLINE_EARLY repeatM #-}
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = K.repeatM

{-# RULES "repeatM serial" repeatM = repeatMSerial #-}
{-# INLINE repeatMSerial #-}
repeatMSerial :: MonadAsync m => m a -> SerialT m a
repeatMSerial = fromStreamS . S.repeatM

-- |
-- @
-- iterate f x = x \`cons` iterate f x
-- @
--
-- Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- @
-- > S.toList $ S.take 5 $ S.iterate (+1) 1
-- [1,2,3,4,5]
-- @
--
-- @since 0.1.2
{-# INLINE_NORMAL iterate #-}
iterate :: (IsStream t, Monad m) => (a -> a) -> a -> t m a
iterate step = fromStreamS . S.iterate step

-- |
-- @
-- iterateM f m = m >>= \a -> return a \`consM` iterateM f (f a)
-- @
--
-- Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function
-- @f@ on the previous element.
--
-- When run concurrently, the next iteration can run concurrently with the
-- processing of the previous iteration. Note that more than one iteration
-- cannot run concurrently as the next iteration depends on the output of the
-- previous iteration.
--
-- @
-- drain $ serially $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
--
-- drain $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.2/
--
-- /Since: 0.7.0 (signature change)/
{-# INLINE_EARLY iterateM #-}
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> m a -> t m a
iterateM = K.iterateM

{-# RULES "iterateM serial" iterateM = iterateMSerial #-}
{-# INLINE iterateMSerial #-}
iterateMSerial :: MonadAsync m => (a -> m a) -> m a -> SerialT m a
iterateMSerial step = fromStreamS . S.iterateM step

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromListM = 'Prelude.foldr' 'K.consM' 'K.nil'
-- @
--
-- Construct a stream from a list of monadic actions. This is more efficient
-- than 'fromFoldableM' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromListM #-}
fromListM :: (MonadAsync m, IsStream t) => [m a] -> t m a
fromListM = fromStreamD . D.fromListM
{-# RULES "fromListM fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromListM a) = fromFoldableM a #-}

-- |
-- @
-- fromFoldableM = 'Prelude.foldr' 'consM' 'K.nil'
-- @
--
-- Construct a stream from a 'Foldable' containing monadic actions.
--
-- @
-- drain $ serially $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite containers)/
--
-- @since 0.3.0
{-# INLINE fromFoldableM #-}
fromFoldableM :: (IsStream t, MonadAsync m, Foldable f) => f (m a) -> t m a
fromFoldableM = Prelude.foldr consM K.nil

-- | Same as 'fromFoldable'.
--
-- @since 0.1.0
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = K.fromFoldable

-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
{-# DEPRECATED fromHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = go
  where
  go = K.mkStream $ \_ yld _ stp -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str go

-- | Construct a stream by reading a 'Prim' 'IORef' repeatedly.
--
-- /Internal/
--
{-# INLINE fromPrimIORef #-}
fromPrimIORef :: (IsStream t, MonadIO m, Prim a) => IORef a -> t m a
fromPrimIORef = fromStreamD . D.fromPrimIORef

-- | Takes a callback setter function and provides it with a callback.  The
-- callback when invoked adds a value at the tail of the stream. Returns a
-- stream of values generated by the callback.
--
-- /Internal/
--
{-# INLINE fromCallback #-}
fromCallback :: MonadAsync m => ((a -> m ()) -> m ()) -> SerialT m a
fromCallback setCallback = concatM $ do
    (callback, stream) <- D.newCallbackStream
    setCallback callback
    return stream

------------------------------------------------------------------------------
-- Time related
------------------------------------------------------------------------------

-- | @timesWith g@ returns a stream of time value tuples. The first component
-- of the tuple is an absolute time reference (epoch) denoting the start of the
-- stream and the second component is a time relative to the reference.
--
-- The argument @g@ specifies the granularity of the relative time in seconds.
-- A lower granularity clock gives higher precision but is more expensive in
-- terms of CPU usage. Any granularity lower than 1 ms is treated as 1 ms.
--
-- @
-- >>> S.mapM_ (\x -> print x >> threadDelay 1000000) $ S.timesWith 0.01
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 0))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1002028000))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1996656000))
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE timesWith #-}
timesWith :: (IsStream t, MonadAsync m) => Double -> t m (AbsTime, RelTime64)
timesWith g = fromStreamD $ D.times g

-- | @times@ returns a stream of time value tuples with clock of 10 ms
-- granularity. The first component of the tuple is an absolute time reference
-- (epoch) denoting the start of the stream and the second component is a time
-- relative to the reference.
--
-- @
-- >>> S.mapM_ (\x -> print x >> threadDelay 1000000) $ S.times
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 0))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1002028000))
-- > (AbsTime (TimeSpec {sec = 2496295, nsec = 536223000}),RelTime64 (NanoSecond64 1996656000))
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE times #-}
times :: (IsStream t, MonadAsync m) => t m (AbsTime, RelTime64)
times = timesWith 0.01

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.absTimesWith 0.01
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE absTimesWith #-}
absTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @absTimes@ returns a stream of absolute timestamps using a clock of 10 ms
-- granularity.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.absTimes
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE absTimes #-}
absTimes :: (IsStream t, MonadAsync m, Functor (t m)) => t m AbsTime
absTimes = fmap (uncurry addToAbsTime64) times

{-# DEPRECATED currentTime "Please use absTimes instead" #-}
{-# INLINE currentTime #-}
currentTime :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m AbsTime
currentTime = absTimesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.relTimesWith 0.01
-- > RelTime64 (NanoSecond64 0)
-- > RelTime64 (NanoSecond64 91139000)
-- > RelTime64 (NanoSecond64 204052000)
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE relTimesWith #-}
relTimesWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m RelTime64
relTimesWith = fmap snd . timesWith

-- | @relTimes@ returns a stream of relative time values starting from 0,
-- using a clock of granularity 10 ms.
--
-- @
-- >>> S.mapM_ print $ S.delayPre 1 $ S.relTimes
-- @
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Internal/
--
{-# INLINE relTimes #-}
relTimes :: (IsStream t, MonadAsync m, Functor (t m)) => t m RelTime64
relTimes = fmap snd times

-- | @durations g@ returns a stream of relative time values measuring the time
-- elapsed since the immediate predecessor element of the stream was generated.
-- The first element of the stream is always 0. @durations@ uses a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage. The minimum granularity is 1 millisecond.
-- Durations lower than 1 ms will be 0.
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Unimplemented/
--
{-# INLINE durations #-}
durations :: -- (IsStream t, MonadAsync m) =>
    Double -> t m RelTime64
durations = undefined

-- | Generate ticks at the specified rate. The rate is adaptive, the tick
-- generation speed can be increased or decreased at different times to achieve
-- the specified rate.  The specific behavior for different styles of 'Rate'
-- specifications is documented under 'Rate'.  The effective maximum rate
-- achieved by a stream is governed by the processor speed.
--
-- /Unimplemented/
--
{-# INLINE ticks #-}
ticks :: -- (IsStream t, MonadAsync m) =>
    Rate -> t m ()
ticks = undefined

-- | Generate a singleton event at or after the specified absolute time. Note
-- that this is different from a threadDelay, a threadDelay starts from the
-- time when the action is evaluated, whereas if we use AbsTime based timeout
-- it will immediately expire if the action is evaluated too late.
--
-- /Unimplemented/
--
{-# INLINE timeout #-}
timeout :: -- (IsStream t, MonadAsync m) =>
    AbsTime -> t m ()
timeout = undefined

------------------------------------------------------------------------------
-- Elimination by Folding
------------------------------------------------------------------------------

-- | Right associative/lazy pull fold. @foldrM build final stream@ constructs
-- an output structure using the step function @build@. @build@ is invoked with
-- the next input element and the remaining (lazy) tail of the output
-- structure. It builds a lazy output expression using the two. When the "tail
-- structure" in the output expression is evaluated it calls @build@ again thus
-- lazily consuming the input @stream@ until either the output expression built
-- by @build@ is free of the "tail" or the input is exhausted in which case
-- @final@ is used as the terminating case for the output structure. For more
-- details see the description in the previous section.
--
-- Example, determine if any element is 'odd' in a stream:
--
-- >>> S.foldrM (\x xs -> if odd x then return True else xs) (return False) $ S.fromList (2:4:5:undefined)
-- > True
--
-- /Since: 0.7.0 (signature changed)/
--
-- /Since: 0.2.0 (signature changed)/
--
-- /Since: 0.1.0/
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> SerialT m a -> m b
foldrM = P.foldrM

-- | Right fold to a streaming monad.
--
-- > foldrS S.cons S.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> S.toList $ S.foldrS S.cons S.nil $ S.fromList [1..5]
-- > [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> S.toList $ S.foldrS (\x xs -> if odd x then return True else xs) (return False) $ (S.fromList (2:4:5:undefined) :: SerialT IO Int)
-- > [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> S.toList $ S.foldrS (\x xs -> if odd x then (x + 2) `S.cons` xs else xs) S.nil $ (S.fromList [1..5] :: SerialT IO Int)
-- > [3,5,7]
--
-- 'foldrM' can also be represented in terms of 'foldrS', however, the former
-- is much more efficient:
--
-- > foldrM f z s = runIdentityT $ foldrS (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- /Internal/
{-# INLINE foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS = K.foldrS

-- | Right fold to a transformer monad.  This is the most general right fold
-- function. 'foldrS' is a special case of 'foldrT', however 'foldrS'
-- implementation can be more efficient:
--
-- > foldrS = foldrT
-- > foldrM f z s = runIdentityT $ foldrT (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- 'foldrT' can be used to translate streamly streams to other transformer
-- monads e.g.  to a different streaming type.
--
-- /Internal/
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT f z s = S.foldrT f z (toStreamS s)

-- | Right fold, lazy for lazy monads and pure streams, and strict for strict
-- monads.
--
-- Please avoid using this routine in strict monads like IO unless you need a
-- strict right fold. This is provided only for use in lazy monads (e.g.
-- Identity) or pure streams. Note that with this signature it is not possible
-- to implement a lazy foldr when the monad @m@ is strict. In that case it
-- would be strict in its accumulator and therefore would necessarily consume
-- all its input.
--
-- @since 0.1.0
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
foldr = P.foldr

-- XXX This seems to be of limited use as it cannot be used to construct
-- recursive structures and for reduction foldl1' is better.
--
-- | Lazy right fold for non-empty streams, using first element as the starting
-- value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldr1 #-}
{-# DEPRECATED foldr1 "Use foldrM instead." #-}
foldr1 :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldr1 f m = S.foldr1 f (toStreamS m)

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# DEPRECATED foldx "Please use foldl' followed by fmap instead." #-}
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx = P.foldlx'

-- | Left associative/strict push fold. @foldl' reduce initial stream@ invokes
-- @reduce@ with the accumulator and the next input in the input stream, using
-- @initial@ as the initial value of the current value of the accumulator. When
-- the input is exhausted the current value of the accumulator is returned.
-- Make sure to use a strict data structure for accumulator to not build
-- unnecessary lazy expressions unless that's what you want. See the previous
-- section for more details.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> SerialT m a -> m b
foldl' = P.foldl'

-- | Strict left fold, for non-empty streams, using first element as the
-- starting value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldl1' #-}
foldl1' :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldl1' step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> do
            res <- foldl' step h t
            return $ Just res

-- | Like 'foldx', but with a monadic step function.
--
-- @since 0.2.0
{-# DEPRECATED foldxM "Please use foldlM' followed by fmap instead." #-}
{-# INLINE foldxM #-}
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM = P.foldlMx'

-- | Like 'foldl'' but with a monadic step function.
--
-- @since 0.2.0
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> SerialT m a -> m b
foldlM' step begin m = S.foldlM' step begin $ toStreamS m

------------------------------------------------------------------------------
-- Running a Fold
------------------------------------------------------------------------------

-- | Fold a stream using the supplied left fold.
--
-- >>> S.fold FL.sum (S.enumerateFromTo 1 100)
-- 5050
--
-- @since 0.7.0
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> SerialT m a -> m b
fold = P.runFold

------------------------------------------------------------------------------
-- Running a sink
------------------------------------------------------------------------------

{-
-- | Drain a stream to a 'Sink'.
{-# INLINE runSink #-}
runSink :: Monad m => Sink m a -> SerialT m a -> m ()
runSink = fold . toFold
-}

------------------------------------------------------------------------------
-- Running a Parser
------------------------------------------------------------------------------

-- | Parse a stream using the supplied 'Parser'.
--
-- /Internal/
--
{-# INLINE_NORMAL parseD #-}
parseD :: MonadThrow m => PRD.Parser m a b -> SerialT m a -> m b
parseD (PRD.Parser step initial extract) = P.parselMx' step initial extract

{-# INLINE parseK #-}
parseK :: MonadThrow m => PRK.Parser m a b -> SerialT m a -> m b
parseK = parse

-- | Parse a stream using the supplied 'Parser'.
--
-- /Internal/
--
{-# INLINE [3] parse #-}
parse :: MonadThrow m => Parser m a b -> SerialT m a -> m b
parse = parseD . PRK.fromParserK

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- |
-- > drain = mapM_ (\_ -> return ())
--
-- Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @drain . 'asyncly'@.
--
-- @since 0.7.0
{-# INLINE drain #-}
drain :: Monad m => SerialT m a -> m ()
drain = P.drain

-- | Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @runStream . 'asyncly'@.
--
-- @since 0.2.0
{-# DEPRECATED runStream "Please use \"drain\" instead" #-}
{-# INLINE runStream #-}
runStream :: Monad m => SerialT m a -> m ()
runStream = drain

-- |
-- > drainN n = drain . take n
--
-- Run maximum up to @n@ iterations of a stream.
--
-- @since 0.7.0
{-# INLINE drainN #-}
drainN :: Monad m => Int -> SerialT m a -> m ()
drainN n = drain . take n

-- |
-- > runN n = runStream . take n
--
-- Run maximum up to @n@ iterations of a stream.
--
-- @since 0.6.0
{-# DEPRECATED runN "Please use \"drainN\" instead" #-}
{-# INLINE runN #-}
runN :: Monad m => Int -> SerialT m a -> m ()
runN = drainN

-- |
-- > drainWhile p = drain . takeWhile p
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.7.0
{-# INLINE drainWhile #-}
drainWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
drainWhile p = drain . takeWhile p

-- |
-- > runWhile p = runStream . takeWhile p
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.6.0
{-# DEPRECATED runWhile "Please use \"drainWhile\" instead" #-}
{-# INLINE runWhile #-}
runWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
runWhile = drainWhile

-- | Determine whether the stream is empty.
--
-- @since 0.1.1
{-# INLINE null #-}
null :: Monad m => SerialT m a -> m Bool
null = S.null . toStreamS

-- | Extract the first element of the stream, if any.
--
-- > head = (!! 0)
--
-- @since 0.1.0
{-# INLINE head #-}
head :: Monad m => SerialT m a -> m (Maybe a)
head = S.head . toStreamS

-- | Extract the first element of the stream, if any, otherwise use the
-- supplied default value. It can help avoid one branch in high performance
-- code.
--
-- /Internal/
{-# INLINE headElse #-}
headElse :: Monad m => a -> SerialT m a -> m a
headElse x = D.headElse x . toStreamD

-- |
-- > tail = fmap (fmap snd) . uncons
--
-- Extract all but the first element of the stream, if any.
--
-- @since 0.1.1
{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m = K.tail (K.adapt m)

-- | Extract all but the last element of the stream, if any.
--
-- @since 0.5.0
{-# INLINE init #-}
init :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
init m = K.init (K.adapt m)

-- | Extract the last element of the stream, if any.
--
-- > last xs = xs !! (length xs - 1)
--
-- @since 0.1.1
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last m = S.last $ toStreamS m

-- | Determine whether an element is present in the stream.
--
-- @since 0.1.0
{-# INLINE elem #-}
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = S.elem e (toStreamS m)

-- | Determine whether an element is not present in the stream.
--
-- @since 0.1.0
{-# INLINE notElem #-}
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = S.notElem e (toStreamS m)

-- | Determine the length of the stream.
--
-- @since 0.1.0
{-# INLINE length #-}
length :: Monad m => SerialT m a -> m Int
length = foldl' (\n _ -> n + 1) 0

-- | Determine whether all elements of a stream satisfy a predicate.
--
-- @since 0.1.0
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = S.all p (toStreamS m)

-- | Determine whether any of the elements of a stream satisfy a predicate.
--
-- @since 0.1.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = S.any p (toStreamS m)

-- | Determines if all elements of a boolean stream are True.
--
-- @since 0.5.0
{-# INLINE and #-}
and :: Monad m => SerialT m Bool -> m Bool
and = all (==True)

-- | Determines whether at least one element of a boolean stream is True.
--
-- @since 0.5.0
{-# INLINE or #-}
or :: Monad m => SerialT m Bool -> m Bool
or = any (==True)

-- | Determine the sum of all elements of a stream of numbers. Returns @0@ when
-- the stream is empty. Note that this is not numerically stable for floating
-- point numbers.
--
-- @since 0.1.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl' (+) 0

-- | Determine the product of all elements of a stream of numbers. Returns @1@
-- when the stream is empty.
--
-- @since 0.1.1
{-# INLINE product #-}
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl' (*) 1

-- | Fold a stream of monoid elements by appending them.
--
-- /Internal/
{-# INLINE mconcat #-}
mconcat :: (Monad m, Monoid a) => SerialT m a -> m a
mconcat = foldr mappend mempty

-- |
-- @
-- minimum = 'minimumBy' compare
-- @
--
-- Determine the minimum element in a stream.
--
-- @since 0.1.0
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
minimum m = S.minimum (toStreamS m)

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- @since 0.6.0
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
minimumBy cmp m = S.minimumBy cmp (toStreamS m)

-- |
-- @
-- maximum = 'maximumBy' compare
-- @
--
-- Determine the maximum element in a stream.
--
-- @since 0.1.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
maximum = P.maximum

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- @since 0.6.0
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
maximumBy cmp m = S.maximumBy cmp (toStreamS m)

-- | Lookup the element at the given index.
--
-- @since 0.6.0
{-# INLINE (!!) #-}
(!!) :: Monad m => SerialT m a -> Int -> m (Maybe a)
m !! i = toStreamS m S.!! i

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> find ((==) . fst)
--
-- @since 0.5.0
{-# INLINE lookup #-}
lookup :: (Monad m, Eq a) => a -> SerialT m (a, b) -> m (Maybe b)
lookup a m = S.lookup a (toStreamS m)

-- | Like 'findM' but with a non-monadic predicate.
--
-- > find p = findM (return . p)
--
-- @since 0.5.0
{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe a)
find p m = S.find p (toStreamS m)

-- | Returns the first element that satisfies the given predicate.
--
-- @since 0.6.0
{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> SerialT m a -> m (Maybe a)
findM p m = S.findM p (toStreamS m)

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- @since 0.5.0
{-# INLINE findIndices #-}
findIndices :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m Int
findIndices p m = fromStreamS $ S.findIndices p (toStreamS m)

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.5.0
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe Int)
findIndex p = head . findIndices p

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- @since 0.5.0
{-# INLINE elemIndices #-}
elemIndices :: (IsStream t, Eq a, Monad m) => a -> t m a -> t m Int
elemIndices a = findIndices (==a)

-- | Returns the first index where a given value is found in the stream.
--
-- > elemIndex a = findIndex (== a)
--
-- @since 0.5.0
{-# INLINE elemIndex #-}
elemIndex :: (Monad m, Eq a) => a -> SerialT m a -> m (Maybe Int)
elemIndex a = findIndex (== a)

------------------------------------------------------------------------------
-- Substreams
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- @
-- > S.isPrefixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- @since 0.6.0
{-# INLINE isPrefixOf #-}
isPrefixOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isPrefixOf m1 m2 = D.isPrefixOf (toStreamD m1) (toStreamD m2)

-- Note: isPrefixOf uses the prefix stream only once. In contrast, isSuffixOf
-- may use the suffix stream many times. To run in optimal memory we do not
-- want to buffer the suffix stream in memory therefore  we need an ability to
-- clone (or consume it multiple times) the suffix stream without any side
-- effects so that multiple potential suffix matches can proceed in parallel
-- without buffering the suffix stream. For example, we may create the suffix
-- stream from a file handle, however, if we evaluate the stream multiple
-- times, once for each match, we will need a different file handle each time
-- which may exhaust the file descriptors. Instead, we want to share the same
-- underlying file descriptor, use pread on it to generate the stream and clone
-- the stream for each match. Therefore the suffix stream should be built in
-- such a way that it can be consumed multiple times without any problems.

-- XXX Can be implemented with better space/time complexity.
-- Space: @O(n)@ worst case where @n@ is the length of the suffix.

-- | Returns 'True' if the first stream is a suffix of the second. A stream is
-- considered a suffix of itself.
--
-- @
-- > S.isSuffixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- Space: @O(n)@, buffers entire input stream and the suffix.
--
-- /Internal/
--
-- /Suboptimal/ - Help wanted.
--
{-# INLINE isSuffixOf #-}
isSuffixOf :: (Monad m, Eq a) => SerialT m a -> SerialT m a -> m Bool
isSuffixOf suffix stream = reverse suffix `isPrefixOf` reverse stream

-- | Returns 'True' if the first stream is an infix of the second. A stream is
-- considered an infix of itself.
--
-- @
-- > S.isInfixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- Space: @O(n)@ worst case where @n@ is the length of the infix.
--
-- /Internal/
--
-- /Requires 'Storable' constraint/ - Help wanted.
--
{-# INLINE isInfixOf #-}
isInfixOf :: (MonadIO m, Eq a, Enum a, Storable a)
    => SerialT m a -> SerialT m a -> m Bool
isInfixOf infx stream = do
    arr <- fold A.write infx
    -- XXX can use breakOnSeq instead (when available)
    r <- null $ drop 1 $ splitOnSeq arr FL.drain stream
    return (not r)

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is a subsequence of itself.
--
-- @
-- > S.isSubsequenceOf (S.fromList "hlo") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- @since 0.6.0
{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isSubsequenceOf m1 m2 = D.isSubsequenceOf (toStreamD m1) (toStreamD m2)

-- | Strip prefix if present and tell whether it was stripped or not. Returns
-- 'Nothing' if the stream does not start with the given prefix, stripped
-- stream otherwise. Returns @Just nil@ when the prefix is the same as the
-- stream.
--
-- Space: @O(1)@
--
-- @since 0.6.0
{-# INLINE stripPrefix #-}
stripPrefix
    :: (Eq a, IsStream t, Monad m)
    => t m a -> t m a -> m (Maybe (t m a))
stripPrefix m1 m2 = fmap fromStreamD <$>
    D.stripPrefix (toStreamD m1) (toStreamD m2)

-- Note: If we want to return a Maybe value to know whether the
-- suffix/infix was present or not along with the stripped stream then
-- we need to buffer the whole input stream.
--
-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- It may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Internal/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => SerialT m a -> SerialT m a -> m (Maybe (SerialT m a))
stripSuffix m1 m2 = fmap reverse <$> stripPrefix (reverse m1) (reverse m2)

-- | Drop prefix from the input stream if present.
--
-- Space: @O(1)@
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropPrefix #-}
dropPrefix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropPrefix = error "Not implemented yet!"

-- | Drop all matching infix from the input stream if present. Infix stream
-- may be consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the infix.
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropInfix #-}
dropInfix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropInfix = error "Not implemented yet!"

-- | Drop suffix from the input stream if present. Suffix stream may be
-- consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the suffix.
--
-- /Unimplemented/ - Help wanted.
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropSuffix = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- XXX this can utilize parallel mapping if we implement it as drain . mapM
-- |
-- > mapM_ = drain . mapM
--
-- Apply a monadic action to each element of the stream and discard the output
-- of the action. This is not really a pure transformation operation but a
-- transformation followed by fold.
--
-- @since 0.1.0
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> SerialT m a -> m ()
mapM_ f m = S.mapM_ f $ toStreamS m

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- toList = S.foldr (:) []
-- @
--
-- Convert a stream into a list in the underlying monad. The list can be
-- consumed lazily in a lazy monad (e.g. 'Identity'). In a strict monad (e.g.
-- IO) the whole list is generated and buffered before it can be consumed.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList = P.toList

-- |
-- @
-- toListRev = S.foldl' (flip (:)) []
-- @
--
-- Convert a stream into a list in reverse order in the underlying monad.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- /Internal/
{-# INLINE toListRev #-}
toListRev :: Monad m => SerialT m a -> m [a]
toListRev = D.toListRev . toStreamD

-- |
-- @
-- toHandle h = S.mapM_ $ hPutStrLn h
-- @
--
-- Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
{-# DEPRECATED toHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h = go
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yieldk a r = liftIO (IO.hPutStrLn h a) >> go r
        in K.foldStream defState yieldk single stop m1

-- XXX rename these to write/writeRev to make the naming consistent with folds
-- in other modules.
--
-- | A fold that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Internal/
{-# INLINE toStream #-}
toStream :: Monad m => Fold m a (SerialT Identity a)
toStream = Fold (\f x -> return $ f . (x `K.cons`))
                (return id)
                (return . ($ K.nil))

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Data.Array" instead.
--
-- /Internal/

--  xn : ... : x2 : x1 : []
{-# INLINABLE toStreamRev #-}
toStreamRev :: Monad m => Fold m a (SerialT Identity a)
toStreamRev = Fold (\xs x -> return $ x `K.cons` xs) (return K.nil) return

-- | Convert a stream to a pure stream.
--
-- @
-- toPure = foldr cons nil
-- @
--
-- /Internal/
--
{-# INLINE toPure #-}
toPure :: Monad m => SerialT m a -> m (SerialT Identity a)
toPure = foldr K.cons K.nil

-- | Convert a stream to a pure stream in reverse order.
--
-- @
-- toPureRev = foldl' (flip cons) nil
-- @
--
-- /Internal/
--
{-# INLINE toPureRev #-}
toPureRev :: Monad m => SerialT m a -> m (SerialT Identity a)
toPureRev = foldl' (flip K.cons) K.nil

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

infixr 0 |$
infixr 0 |$.

infixl 1 |&
infixl 1 |&.

-- | Parallel transform application operator; applies a stream transformation
-- function @t m a -> t m b@ to a stream @t m a@ concurrently; the input stream
-- is evaluated asynchronously in an independent thread yielding elements to a
-- buffer and the transformation function runs in another thread consuming the
-- input from the buffer.  '|$' is just like regular function application
-- operator '$' except that it is concurrent.
--
-- If you read the signature as @(t m a -> t m b) -> (t m a -> t m b)@ you can
-- look at it as a transformation that converts a transform function to a
-- buffered concurrent transform function.
--
-- The following code prints a value every second even though each stage adds a
-- 1 second delay.
--
--
-- @
-- drain $
--    S.mapM (\\x -> threadDelay 1000000 >> print x)
--      |$ S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$) #-}
(|$) :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> (t m a -> t m b)
-- (|$) f = f . Async.mkAsync
(|$) f = f . D.mkParallel

-- | Same as '|$'.
--
--  /Internal/
--
{-# INLINE applyAsync #-}
applyAsync :: (IsStream t, MonadAsync m)
    => (t m a -> t m b) -> (t m a -> t m b)
applyAsync = (|$)

-- | Parallel reverse function application operator for streams; just like the
-- regular reverse function application operator '&' except that it is
-- concurrent.
--
-- @
-- drain $
--       S.repeatM (threadDelay 1000000 >> return 1)
--    |& S.mapM (\\x -> threadDelay 1000000 >> print x)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&) #-}
(|&) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> t m b) -> t m b
x |& f = f |$ x

-- | Parallel fold application operator; applies a fold function @t m a -> m b@
-- to a stream @t m a@ concurrently; The the input stream is evaluated
-- asynchronously in an independent thread yielding elements to a buffer and
-- the folding action runs in another thread consuming the input from the
-- buffer.
--
-- If you read the signature as @(t m a -> m b) -> (t m a -> m b)@ you can look
-- at it as a transformation that converts a fold function to a buffered
-- concurrent fold function.
--
-- The @.@ at the end of the operator is a mnemonic for termination of the
-- stream.
--
-- @
--    S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
--       |$. S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$.) #-}
(|$.) :: (IsStream t, MonadAsync m) => (t m a -> m b) -> (t m a -> m b)
-- (|$.) f = f . Async.mkAsync
(|$.) f = f . D.mkParallel

-- | Same as '|$.'.
--
--  /Internal/
--
{-# INLINE foldAsync #-}
foldAsync :: (IsStream t, MonadAsync m) => (t m a -> m b) -> (t m a -> m b)
foldAsync = (|$.)

-- | Parallel reverse function application operator for applying a run or fold
-- functions to a stream. Just like '|$.' except that the operands are reversed.
--
-- @
--        S.repeatM (threadDelay 1000000 >> return 1)
--    |&. S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&.) #-}
(|&.) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> m b) -> m b
x |&. f = f |$. x

------------------------------------------------------------------------------
-- General Transformation
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
--
-- /Internal/
--
{-# INLINE transform #-}
transform :: (IsStream t, Monad m) => Pipe m a b -> t m a -> t m b
transform pipe xs = fromStreamD $ D.transform pipe (toStreamD xs)

------------------------------------------------------------------------------
-- Transformation by Folding (Scans)
------------------------------------------------------------------------------

-- XXX It may be useful to have a version of scan where we can keep the
-- accumulator independent of the value emitted. So that we do not necessarily
-- have to keep a value in the accumulator which we are not using. We can pass
-- an extraction function that will take the accumulator and the current value
-- of the element and emit the next value in the stream. That will also make it
-- possible to modify the accumulator after using it. In fact, the step function
-- can return new accumulator and the value to be emitted. The signature would
-- be more like mapAccumL. Or we can change the signature of scanx to
-- accommodate this.
--
-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- /Since 0.2.0/
--
-- /Since: 0.7.0 (Monad m constraint)/
{-# DEPRECATED scanx "Please use scanl followed by map instead." #-}
{-# INLINE scanx #-}
scanx :: (IsStream t, Monad m) => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx = P.scanlx'

-- XXX this needs to be concurrent
-- XXX because of the use of D.cons for appending, scanlM' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.
--
-- | Like 'scanl'' but with a monadic fold function.
--
-- @since 0.4.0
{-# INLINE scanlM' #-}
scanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
scanlM' step begin m = fromStreamD $ D.scanlM' step begin $ toStreamD m

-- | @scanlMAfter' accumulate initial done stream@ is like 'scanlM'' except
-- that it provides an additional @done@ function to be applied on the
-- accumulator when the stream stops. The result of @done@ is also emitted in
-- the stream.
--
-- This function can be used to allocate a resource in the beginning of the
-- scan and release it when the stream ends or to flush the internal state of
-- the scan at the end.
--
-- /Internal/
--
{-# INLINE scanlMAfter' #-}
scanlMAfter' :: (IsStream t, Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> t m a -> t m b
scanlMAfter' step initial done stream =
    fromStreamD $ D.scanlMAfter' step initial done $ toStreamD stream

-- XXX because of the use of D.cons for appending, scanl' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.

-- | Strict left scan. Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- @
-- > S.toList $ S.scanl' (+) 0 $ fromList [1,2,3,4]
-- [0,1,3,6,10]
-- @
--
-- @
-- > S.toList $ S.scanl' (flip (:)) [] $ S.fromList [1,2,3,4]
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
-- @
--
-- The output of 'scanl'' is the initial value of the accumulator followed by
-- all the intermediate steps and the final result of 'foldl''.
--
-- By streaming the accumulated state after each fold step, we can share the
-- state across multiple stages of stream composition. Each stage can modify or
-- extend the state, do some processing with it and emit it for the next stage,
-- thus modularizing the stream processing. This can be useful in
-- stateful or event-driven programming.
--
-- Consider the following monolithic example, computing the sum and the product
-- of the elements in a stream in one go using a @foldl'@:
--
-- @
-- > S.foldl' (\\(s, p) x -> (s + x, p * x)) (0,1) $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- @
-- >   S.foldl' (\\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ S.scanl' (\\(s, _) x -> (s + x, x)) (0,1)
--   $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- See also: 'usingStateT'
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
scanl' step z m = fromStreamS $ S.scanl' step z $ toStreamS m

-- | Like 'scanl'' but does not stream the initial value of the accumulator.
--
-- > postscanl' f z xs = S.drop 1 $ S.scanl' f z xs
--
-- @since 0.7.0
{-# INLINE postscanl' #-}
postscanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
postscanl' step z m = fromStreamD $ D.postscanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like 'postscanl'' but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE postscanlM' #-}
postscanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
postscanlM' step z m = fromStreamD $ D.postscanlM' step z $ toStreamD m

-- XXX prescanl does not sound very useful, enable only if there is a
-- compelling use case.
--
-- | Like scanl' but does not stream the final value of the accumulator.
--
-- /Internal/
{-# INLINE prescanl' #-}
prescanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
prescanl' step z m = fromStreamD $ D.prescanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like postscanl' but with a monadic step function.
--
-- /Internal/
{-# INLINE prescanlM' #-}
prescanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
prescanlM' step z m = fromStreamD $ D.prescanlM' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like 'scanl1'' but with a monadic step function.
--
-- @since 0.6.0
{-# INLINE scanl1M' #-}
scanl1M' :: (IsStream t, Monad m) => (a -> a -> m a) -> t m a -> t m a
scanl1M' step m = fromStreamD $ D.scanl1M' step $ toStreamD m

-- | Like 'scanl'' but for a non-empty stream. The first element of the stream
-- is used as the initial value of the accumulator. Does nothing if the stream
-- is empty.
--
-- @
-- > S.toList $ S.scanl1 (+) $ fromList [1,2,3,4]
-- [1,3,6,10]
-- @
--
-- @since 0.6.0
{-# INLINE scanl1' #-}
scanl1' :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> t m a
scanl1' step m = fromStreamD $ D.scanl1' step $ toStreamD m

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- | Scan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE scan #-}
scan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
scan (Fold step begin done) = P.scanlMx' step begin done

-- | Postscan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE postscan #-}
postscan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
postscan (Fold step begin done) = P.postscanlMx' step begin done

------------------------------------------------------------------------------
-- Stateful Transformations
------------------------------------------------------------------------------

-- XXX this is not a one-to-one map so calling it map may not be right.
-- We can perhaps call it zipWithTail or rollWith.
--
-- | Apply a function on every two successive elements of a stream. If the
-- stream consists of a single element the output is an empty stream.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Internal/
--
{-# INLINE rollingMap #-}
rollingMap :: (IsStream t, Monad m) => (a -> a -> b) -> t m a -> t m b
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Internal/
--
{-# INLINE rollingMapM #-}
rollingMapM :: (IsStream t, Monad m) => (a -> a -> m b) -> t m a -> t m b
rollingMapM f m = fromStreamD $ D.rollingMapM f $ toStreamD m

------------------------------------------------------------------------------
-- Transformation by Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- @since 0.1.0
{-# INLINE filter #-}
#if __GLASGOW_HASKELL__ != 802
-- GHC 8.2.2 crashes with this code, when used with "stack"
filter :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
filter p m = fromStreamS $ S.filter p $ toStreamS m
#else
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter = K.filter
#endif

-- | Same as 'filter' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE filterM #-}
filterM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Drop repeated elements that are adjacent to each other.
--
-- @since 0.6.0
{-# INLINE uniq #-}
uniq :: (Eq a, IsStream t, Monad m) => t m a -> t m a
uniq = fromStreamD . D.uniq . toStreamD

-- | Ensures that all the elements of the stream are identical and then returns
-- that unique element.
--
-- @since 0.6.0
{-# INLINE the #-}
the :: (Eq a, Monad m) => SerialT m a -> m (Maybe a)
the m = S.the (toStreamS m)

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.1.0
{-# INLINE take #-}
take :: (IsStream t, Monad m) => Int -> t m a -> t m a
take n m = fromStreamS $ S.take n $ toStreamS
    (maxYields (Just (fromIntegral n)) m)

-- | End the stream as soon as the predicate fails on an element.
--
-- @since 0.1.0
{-# INLINE takeWhile #-}
takeWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStreamS $ S.takeWhile p $ toStreamS m

-- | Same as 'takeWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE takeWhileM #-}
takeWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- | @takeByTime duration@ yields stream elements upto specified time
-- @duration@. The duration starts when the stream is evaluated for the first
-- time, before the first element is yielded. The time duration is checked
-- before generating each element, if the duration has expired the stream
-- stops.
--
-- The total time taken in executing the stream is guaranteed to be /at least/
-- @duration@, however, because the duration is checked before generating an
-- element, the upper bound is indeterminate and depends on the time taken in
-- generating and processing the last element.
--
-- No element is yielded if the duration is zero. At least one element is
-- yielded if the duration is non-zero.
--
-- /Internal/
--
{-# INLINE takeByTime #-}
takeByTime ::(MonadIO m, IsStream t, TimeUnit64 d) => d -> t m a -> t m a
takeByTime d = fromStreamD . D.takeByTime d . toStreamD

-- | Discard first 'n' elements from the stream and take the rest.
--
-- @since 0.1.0
{-# INLINE drop #-}
drop :: (IsStream t, Monad m) => Int -> t m a -> t m a
drop n m = fromStreamS $ S.drop n $ toStreamS m

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
-- @since 0.1.0
{-# INLINE dropWhile #-}
dropWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStreamS $ S.dropWhile p $ toStreamS m

-- | Same as 'dropWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE dropWhileM #-}
dropWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

-- | @dropByTime duration@ drops stream elements until specified @duration@ has
-- passed.  The duration begins when the stream is evaluated for the first
-- time. The time duration is checked /after/ generating a stream element, the
-- element is yielded if the duration has expired otherwise it is dropped.
--
-- The time elapsed before starting to generate the first element is /at most/
-- @duration@, however, because the duration expiry is checked after the
-- element is generated, the lower bound is indeterminate and depends on the
-- time taken in generating an element.
--
-- All elements are yielded if the duration is zero.
--
-- /Internal/
--
{-# INLINE dropByTime #-}
dropByTime ::(MonadIO m, IsStream t, TimeUnit64 d) => d -> t m a -> t m a
dropByTime d = fromStreamD . D.dropByTime d . toStreamD

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- @
-- mapM f = sequence . map f
-- @
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- @
-- > drain $ S.mapM putStr $ S.fromList ["a", "b", "c"]
-- abc
--
-- drain $ S.replicateM 10 (return 1)
--           & (serially . S.mapM (\\x -> threadDelay 1000000 >> print x))
--
-- drain $ S.replicateM 10 (return 1)
--           & (asyncly . S.mapM (\\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE_EARLY mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM = K.mapM

{-# RULES "mapM serial" mapM = mapMSerial #-}
{-# INLINE mapMSerial #-}
mapMSerial :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapMSerial = Serial.mapM

-- |
-- @
-- sequence = mapM id
-- @
--
-- Replace the elements of a stream of monadic actions with the outputs of
-- those actions.
--
-- @
-- > drain $ S.sequence $ S.fromList [putStr "a", putStr "b", putStrLn "c"]
-- abc
--
-- drain $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (serially . S.sequence)
--
-- drain $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (asyncly . S.sequence)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = fromStreamS $ S.sequence (toStreamS m)

-- $smapM_Notes
--
-- The stateful step function can be simplified to @(s -> a -> m b)@ to provide
-- a read-only environment. However, that would just be 'mapM'.
--
-- The initial action could be @m (s, Maybe b)@, and we can also add a final
-- action @s -> m (Maybe b)@. This can be used to get pre/post scan like
-- functionality and also to flush the state in the end like scanlMAfter'.
-- We can also use it along with a fusible version of bracket to get
-- scanlMAfter' like functionality. See issue #677.
--
-- This can be further generalized to a type similar to Fold/Parser, giving it
-- filtering and parsing capability as well (this is in fact equivalent to
-- parseMany):
--
-- smapM :: (s -> a -> m (Step s b)) -> m s -> t m a -> t m b
--

-- | A stateful 'mapM', equivalent to a left scan, more like mapAccumL.
-- Hopefully, this is a better alternative to @scan@. Separation of state from
-- the output makes it easier to think in terms of a shared state, and also
-- makes it easier to keep the state fully strict and the output lazy.
--
-- See also: 'scanlM''
--
-- /Internal/
--
{-# INLINE smapM #-}
smapM :: (IsStream t, Monad m) =>
       (s -> a -> m (s, b))
    -> m s
    -> t m a
    -> t m b
smapM step initial stream =
    -- XXX implement this directly instead of using scanlM'
    -- Once we have postscanlM' with monadic initial we can use this code
    -- let r = postscanlM'
    --              (\(s, _) a -> step s a)
    --              (fmap (,undefined) initial)
    --              stream
    let r = concatMap
                (\s0 ->
                    postscanlM' (\(s, _) a -> step s a) (s0, undefined) stream
                )
                (yieldM initial)
     in Serial.map snd r

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- @
-- mapMaybe f = S.map 'fromJust' . S.filter 'isJust' . S.map f
-- @
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: (IsStream t, Monad m) => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = fromStreamS $ S.mapMaybe f $ toStreamS m

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- @
-- mapMaybeM f = S.map 'fromJust' . S.filter 'isJust' . S.mapM f
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.3.0
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: (IsStream t, MonadAsync m, Functor (t m))
          => (a -> m (Maybe b)) -> t m a -> t m b
mapMaybeM f = fmap fromJust . filter isJust . K.mapM f

{-# RULES "mapMaybeM serial" mapMaybeM = mapMaybeMSerial #-}
{-# INLINE mapMaybeMSerial #-}
mapMaybeMSerial :: Monad m => (a -> m (Maybe b)) -> SerialT m a -> SerialT m b
mapMaybeMSerial f m = fromStreamD $ D.mapMaybeM f $ toStreamD m

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- /Internal/
--
catMaybes :: (IsStream t, Monad m, Functor (t m)) => t m (Maybe a) -> t m a
catMaybes = fmap fromJust . filter isJust

------------------------------------------------------------------------------
-- Transformation by Reordering
------------------------------------------------------------------------------

-- XXX Use a compact region list to temporarily store the list, in both reverse
-- as well as in reverse'.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- > reverse = S.foldlT (flip S.cons) S.nil
--
-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- /Since 0.7.0 (Monad m constraint)/
--
-- /Since: 0.1.1/
{-# INLINE reverse #-}
reverse :: (IsStream t, Monad m) => t m a -> t m a
reverse s = fromStreamS $ S.reverse $ toStreamS s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- /Internal/
{-# INLINE reverse' #-}
reverse' :: (IsStream t, MonadIO m, Storable a) => t m a -> t m a
reverse' s = fromStreamD $ D.reverse' $ toStreamD s

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseBySpan 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- @
-- >>> S.toList $ S.trace putChar $ S.intersperseM (putChar '.' >> return ',') $ S.fromList "hello"
-- > h.,e.,l.,l.,o"h,e,l,l,o"
-- @
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM m = fromStreamS . S.intersperseM m . toStreamS

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- @
-- >>> S.drain $ S.trace putChar $ S.intersperseM_ (putChar '.') $ S.fromList "hello"
-- > h.e.l.l.o
-- @
--
-- /Internal/
{-# INLINE intersperseM_ #-}
intersperseM_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseM_ m = fromStreamD . D.intersperseM_ m . toStreamD

-- | Insert a pure value between successive elements of a stream.
--
-- @
-- > S.toList $ S.intersperse ',' $ S.fromList "hello"
-- "h,e,l,l,o"
-- @
--
-- @since 0.7.0
{-# INLINE intersperse #-}
intersperse :: (IsStream t, MonadAsync m) => a -> t m a -> t m a
intersperse a = fromStreamS . S.intersperse a . toStreamS

-- | Insert an effect and its output after consuming an element of a stream.
--
-- @
-- >>> S.toList $ S.trace putChar $ S.intersperseSuffix (putChar '.' >> return ',') $ S.fromList "hello"
-- > h.,e.,l.,l.,o.,"h,e,l,l,o,"
-- @
--
-- /Internal/
{-# INLINE intersperseSuffix #-}
intersperseSuffix :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseSuffix m = fromStreamD . D.intersperseSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- @
-- > S.mapM_ putChar $ S.intersperseSuffix_ (threadDelay 1000000) $ S.fromList "hello"
-- @
--
-- /Internal/
--
{-# INLINE intersperseSuffix_ #-}
intersperseSuffix_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseSuffix_ m = fromStreamD . D.intersperseSuffix_ m . toStreamD

-- | Insert a side effect before consuming an element of a stream.
--
-- @
-- >>> S.toList $ S.trace putChar $ S.interspersePrefix_ (putChar '.' >> return ',') $ S.fromList "hello"
-- > .h.e.l.l.o"hello"
-- @
--
-- See also: 'trace_'
--
-- /Concurrent/
--
-- /Internal/
--
{-# INLINE interspersePrefix_ #-}
interspersePrefix_ :: (IsStream t, MonadAsync m) => m b -> t m a -> t m a
interspersePrefix_ m = mapM (\x -> void m >> return x)

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds before consuming an element of the
-- stream except the first one.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delay 1 $ S.enumerateFromTo 1 3
-- > (AbsTime (TimeSpec {sec = 2502706, nsec = 751137000}),1)
-- > (AbsTime (TimeSpec {sec = 2502707, nsec = 743535000}),2)
-- > (AbsTime (TimeSpec {sec = 2502708, nsec = 749758000}),3)
-- @
--
-- /Internal/
--
{-# INLINE delay #-}
delay :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delayPost 1 $ S.enumerateFromTo 1 3
-- > (AbsTime (TimeSpec {sec = 2502826, nsec = 119030000}),1)
-- > (AbsTime (TimeSpec {sec = 2502827, nsec = 111393000}),2)
-- > (AbsTime (TimeSpec {sec = 2502828, nsec = 112221000}),3)
-- @
--
-- /Internal/
--
{-# INLINE delayPost #-}
delayPost :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPost n = intersperseSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial, that's why 'trace_' is used.
--
-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delayPre 1 $ S.enumerateFromTo 1 3
-- > (AbsTime (TimeSpec {sec = 2502207, nsec = 533177000}),1)
-- > (AbsTime (TimeSpec {sec = 2502208, nsec = 530859000}),2)
-- > (AbsTime (TimeSpec {sec = 2502209, nsec = 531619000}),3)
-- @
--
-- /Internal/
--
{-# INLINE delayPre #-}
delayPre :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPre n = trace_ $ liftIO $ threadDelay $ round $ n * 1000000

-- | Like 'intersperseSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- @
-- > S.toList $ S.intersperseSuffixBySpan 2 (return ',') $ S.fromList "hello"
-- "he,ll,o,"
-- @
--
-- /Internal/
--
{-# INLINE intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: (IsStream t, MonadAsync m)
    => Int -> m a -> t m a -> t m a
intersperseSuffixBySpan n eff =
    fromStreamD . D.intersperseSuffixBySpan n eff . toStreamD

{-
-- | Intersperse a monadic action into the input stream after every @n@
-- elements.
--
-- @
-- > S.toList $ S.intersperseBySpan 2 (return ',') $ S.fromList "hello"
-- "he,ll,o"
-- @
--
-- @since 0.7.0
{-# INLINE intersperseBySpan #-}
intersperseBySpan :: IsStream t => Int -> m a -> t m a -> t m a
intersperseBySpan _n _f _xs = undefined
-}

-- | Intersperse a monadic action into the input stream after every @n@
-- seconds.
--
-- @
-- > S.drain $ S.interjectSuffix 1 (putChar ',') $ S.mapM (\\x -> threadDelay 1000000 >> putChar x) $ S.fromList "hello"
-- "h,e,l,l,o"
-- @
--
-- /Internal/
{-# INLINE interjectSuffix #-}
interjectSuffix
    :: (IsStream t, MonadAsync m)
    => Double -> m a -> t m a -> t m a
interjectSuffix n f xs = xs `Par.parallelFst` repeatM timed
    where timed = liftIO (threadDelay (round $ n * 1000000)) >> f

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- @
-- insertBy cmp x = 'mergeBy' cmp ('yield' x)
-- @
--
-- @
-- > S.toList $ S.insertBy compare 2 $ S.fromList [1,3,5]
-- [1,2,3,5]
-- @
--
-- @since 0.6.0
{-# INLINE insertBy #-}
insertBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = fromStreamS $ S.insertBy cmp x (toStreamS m)

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- @
-- > S.toList $ S.deleteBy (==) 3 $ S.fromList [1,3,3,5]
-- [1,3,5]
-- @
--
-- @since 0.6.0
{-# INLINE deleteBy #-}
deleteBy :: (IsStream t, Monad m) => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy cmp x m = fromStreamS $ S.deleteBy cmp x (toStreamS m)

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- |
-- > indexed = S.postscanl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- > indexed = S.zipWith (,) (S.enumerateFrom 0)
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- @
-- > S.toList $ S.indexed $ S.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
-- @
--
-- @since 0.6.0
{-# INLINE indexed #-}
indexed :: (IsStream t, Monad m) => t m a -> t m (Int, a)
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- > indexedR n = S.postscanl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- > indexedR n = S.zipWith (,) (S.enumerateFromThen n (n - 1))
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- @
-- > S.toList $ S.indexedR 10 $ S.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
-- @
--
-- @since 0.6.0
{-# INLINE indexedR #-}
indexedR :: (IsStream t, Monad m) => Int -> t m a -> t m (Int, a)
indexedR n = fromStreamD . D.indexedR n . toStreamD

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- S.mapM_ print $ S.timestamped $ S.delay $ S.enumerateFromTo 1 3
--
-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- specified granularity.  The timestamp is generated just before the element
-- is consumed.
--
-- @
-- >>> S.mapM_ print $ S.timestampWith 0.01 $ S.delay 1 $ S.enumerateFromTo 1 3
-- @
--
-- /Internal/
--
{-# INLINE timestampWith #-}
timestampWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (AbsTime, a)
timestampWith g stream = Z.zipWith (flip (,)) stream (absTimesWith g)

-- TBD: check performance vs a custom implementation without using zipWith.
--
-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- 10 ms granularity.  The timestamp is generated just before the element is
-- consumed.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delay 1 $ S.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = 2460689, nsec = 641121000}),1)
-- (AbsTime (TimeSpec {sec = 2460690, nsec = 639334000}),2)
-- (AbsTime (TimeSpec {sec = 2460691, nsec = 644479000}),3)
-- @
--
-- /Internal/
--
{-# INLINE timestamped #-}
timestamped :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (AbsTime, a)
timestamped = timestampWith 0.01

-- | Pair each element in a stream with relative times starting from 0, using a
-- clock with the specified granularity. The time is measured just before the
-- element is consumed.
--
-- @
-- >>> S.mapM_ print $ S.timeIndexWith 0.01 $ S.delay 1 $ S.enumerateFromTo 1 3
-- @
--
-- /Internal/
--
{-# INLINE timeIndexWith #-}
timeIndexWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (RelTime64, a)
timeIndexWith g stream = Z.zipWith (flip (,)) stream (relTimesWith g)

-- | Pair each element in a stream with relative times starting from 0, using a
-- 10 ms granularity clock. The time is measured just before the element is
-- consumed.
--
-- @
-- >>> S.mapM_ print $ S.timeIndexed $ S.delay 1 $ S.enumerateFromTo 1 3
-- (RelTime64 (NanoSecond64 0),1)
-- (RelTime64 (NanoSecond64 996239000),2)
-- (RelTime64 (NanoSecond64 1996712000),3)
-- @
--
-- /Internal/
--
{-# INLINE timeIndexed #-}
timeIndexed :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (RelTime64, a)
timeIndexed = timeIndexWith 0.01

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality using an equality function.
--
-- @since 0.6.0
{-# INLINABLE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy = P.eqBy

-- | Compare two streams lexicographically using a comparison function.
--
-- @since 0.6.0
{-# INLINABLE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy = P.cmpBy

------------------------------------------------------------------------------
-- Merge
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
-- > S.toList $ S.mergeBy compare (S.fromList [1,3,5]) (S.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
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
-}

-- Holding this back for now, we may want to use the name "merge" differently
{-
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
mergeAsyncByM f m1 m2 = fromStreamD $
    D.mergeByM f (D.mkParallelD $ toStreamD m1) (D.mkParallelD $ toStreamD m2)

------------------------------------------------------------------------------
-- Flattening Lists
------------------------------------------------------------------------------

-- | @concatMapWith merge map stream@ is a two dimensional looping combinator.
-- The first argument specifies a merge or concat function that is used to
-- merge the streams generated by applying the second argument i.e. the @map@
-- function to each element of the input stream. The concat function could be
-- 'serial', 'parallel', 'async', 'ahead' or any other zip or merge function
-- and the second argument could be any stream generation function using a
-- seed.
--
-- /Compare 'foldMapWith'/
--
-- @since 0.7.0
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
-- /Internal/
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

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- @
-- concatMap f = 'concat . map f'
-- concatMap = 'concatMapWith' 'Serial.serial'
-- concatMap f = 'concatMapM' (return . f)
-- concatMap f = 'concatUnfold' (UF.lmap f UF.fromStream)
-- @
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(IsStream t, Monad m) => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Flatten a stream of streams to a single stream.
--
-- @
-- concat = concatMap id
-- @
--
-- /Internal/
{-# INLINE concat #-}
concat :: (IsStream t, Monad m) => t m (t m a) -> t m a
concat = concatMap id

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
-- /Internal/
{-# INLINE append #-}
append ::(IsStream t, Monad m) => t m b -> t m b -> t m b
append m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

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
-- >>> interleave "ab" ",,,," :: SerialT Identity Char
-- fromList "a,b,,,"
-- >>> interleave "abcd" ",," :: SerialT Identity Char
-- fromList "a,b,cd"
--
-- 'interleave' is dual to 'interleaveMin', it can be called @interleaveMax@.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
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
-- >>> interleaveSuffix "abc" ",,,," :: SerialT Identity Char
-- fromList "a,b,c,"
-- >>> interleaveSuffix "abc" "," :: SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveSuffix' is a dual of 'interleaveInfix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
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
-- >>> interleaveInfix "abc" ",,,," :: SerialT Identity Char
-- fromList "a,b,c"
-- >>> interleaveInfix "abc" "," :: SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveInfix' is a dual of 'interleaveSuffix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
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
-- >>> interleaveMin "ab" ",,,," :: SerialT Identity Char
-- fromList "a,b,"
-- >>> interleaveMin "abcd" ",," :: SerialT Identity Char
-- fromList "a,b,c"
--
-- 'interleaveMin' is dual to 'interleave'.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
{-# INLINE interleaveMin #-}
interleaveMin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveMin m1 m2 = fromStreamD $ D.interleaveMin (toStreamD m1) (toStreamD m2)

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
-- /Internal/
{-# INLINE roundrobin #-}
roundrobin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
roundrobin m1 m2 = fromStreamD $ D.roundRobin (toStreamD m1) (toStreamD m2)

-- | Map a stream producing monadic function on each element of the stream
-- and then flatten the results into a single stream. Since the stream
-- generation function is monadic, unlike 'concatMap', it can produce an
-- effect at the beginning of each iteration of the inner loop.
--
-- @since 0.6.0
{-# INLINE concatMapM #-}
concatMapM :: (IsStream t, Monad m) => (a -> m (t m b)) -> t m a -> t m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- @
-- concatM = concat . yieldM
-- concatM = concat . lift    -- requires @(MonadTrans t)@
-- concatM = join . lift      -- requires @(MonadTrans t@, @Monad (t m))@
-- @
--
-- See also: 'concat', 'sequence'
--
--  /Internal/
--
{-# INLINE concatM #-}
concatM :: (IsStream t, Monad m) => m (t m a) -> t m a
concatM generator = concatMapM (\() -> generator) (yield ())

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
-- @since 0.7.0
{-# INLINE concatUnfold #-}
concatUnfold ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
concatUnfold u m = fromStreamD $ D.concatMapU u (toStreamD m)

-- | Like 'concatUnfold' but interleaves the streams in the same way as
-- 'interleave' behaves instead of appending them.
--
-- /Internal/
{-# INLINE concatUnfoldInterleave #-}
concatUnfoldInterleave ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
concatUnfoldInterleave u m =
    fromStreamD $ D.concatUnfoldInterleave u (toStreamD m)

-- | Like 'concatUnfold' but executes the streams in the same way as
-- 'roundrobin'.
--
-- /Internal/
{-# INLINE concatUnfoldRoundrobin #-}
concatUnfoldRoundrobin ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
concatUnfoldRoundrobin u m =
    fromStreamD $ D.concatUnfoldRoundrobin u (toStreamD m)

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of concatUnfold becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > concatUnfold unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())
--
-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Internal/
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
-- > intercalate a unf str = concatUnfold unf $ intersperse a str
--
-- | 'intersperse' followed by unfold and concat.
--
-- > unwords = intercalate " " UF.fromList
--
-- >>> intercalate " " UF.fromList ["abc", "def", "ghi"]
-- > "abc def ghi"
--
-- /Internal/
{-# INLINE intercalate #-}
intercalate :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalate seed unf str = D.fromStreamD $
    D.concatMapU unf $ D.intersperse seed (toStreamD str)

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- > unwords = S.interpose ' '
--
-- /Internal/
{-# INLINE interpose #-}
interpose :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interpose x unf str =
    D.fromStreamD $ D.interpose (return x) unf (D.toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Internal/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalateSuffix unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalateSuffix
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- > intercalateSuffix seed unf str = gintercalateSuffix unf str unf (repeatM seed)
-- > intercalateSuffix a unf str = concatUnfold unf $ intersperseSuffix a str
--
-- | 'intersperseSuffix' followed by unfold and concat.
--
-- > unlines = intercalateSuffix "\n" UF.fromList
--
-- >>> intercalate "\n" UF.fromList ["abc", "def", "ghi"]
-- > "abc\ndef\nghi\n"
--
-- /Internal/
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalateSuffix seed unf str = fromStreamD $ D.concatMapU unf
    $ D.intersperseSuffix (return seed) (D.toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- > unlines = S.interposeSuffix '\n'
--
-- /Internal/
{-# INLINE interposeSuffix #-}
interposeSuffix :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interposeSuffix x unf str =
    D.fromStreamD $ D.interposeSuffix (return x) unf (D.toStreamD str)

------------------------------------------------------------------------------
-- Flattening Trees
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
-- /Internal/
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
-- /Internal/
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
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Internal/
--
lefts :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Internal/
--
rights :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m b
rights = fmap (fromRight undefined) . filter isRight

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
-- /Internal/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: IsStream t
    => (t m (Either a b) -> t m (Either a b) -> t m (Either a b))
    -> (a -> t m (Either a b))
    -> t m (Either a b)
    -> t m (Either a b)
iterateMapLeftsWith combine f = iterateMapWith combine (either f (const K.nil))

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- Splitting operations that take a predicate and a Fold can be
-- expressed using parseMany. Operations like chunksOf, intervalsOf, split*,
-- can be expressed using parseMany when used with an appropriate Parser.
--
-- XXX We need takeGE/takeBetween to implement "some" using "many".

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- This is the streaming equivalent of the 'Streamly.Internal.Data.Parser.many'
-- parse combinator.
--
-- >>> S.toList $ S.parseMany (PR.take 2 $ PR.fromFold FL.sum) $ S.fromList [1..10]
-- > [3,7,11,15,19]
--
-- >>> S.toList $ S.parseMany (PR.line FL.toList) $ S.fromList "hello\nworld"
-- > ["hello\n","world"]
--
-- /Internal
--
{-# INLINE parseMany #-}
parseMany
    :: (IsStream t, MonadThrow m)
    => Parser m a b
    -> t m a
    -> t m b
parseMany p m =
    D.fromStreamD $ D.parseMany (PRK.fromParserK p) (D.toStreamD m)

{-# INLINE parseManyD #-}
parseManyD
    :: (IsStream t, MonadThrow m)
    => PRD.Parser m a b
    -> t m a
    -> t m b
parseManyD p m =
    D.fromStreamD $ D.parseMany p (D.toStreamD m)

-- | @parseManyTill collect test stream@ tries the parser @test@ on the input,
-- if @test@ fails it backtracks and tries @collect@, after @collect@ succeeds
-- @test@ is tried again and so on. The parser stops when @test@ succeeds.  The
-- output of @test@ is discarded and the output of @collect@ is emitted in the
-- output stream. The parser fails if @collect@ fails.
--
-- /Unimplemented/
--
{-# INLINE parseManyTill #-}
parseManyTill ::
    -- (IsStream t, MonadThrow m) =>
       Parser m a b
    -> Parser m a x
    -> t m a
    -> t m b
parseManyTill = undefined

-- | Iterate a parser generating function on a stream. The initial value @b@ is
-- used to generate the first parser, the parser is applied on the stream and
-- the result is used to generate the next parser and so on.
--
-- >>> S.toList $ S.map getSum $ S.parseIterate (\b -> PR.take 2 (FL.mconcatTo b)) 0 $ S.map Sum $ S.fromList [1..10]
-- > [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Internal/
--
{-# INLINE parseIterate #-}
parseIterate
    :: (IsStream t, MonadThrow m)
    => (b -> Parser m a b)
    -> b
    -> t m a
    -> t m b
parseIterate f i m = D.fromStreamD $
    D.parseIterate (PRK.fromParserK . f) i (D.toStreamD m)

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------
--

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Generalized grouping
------------------------------------------------------------------------------

-- This combinator is the most general grouping combinator and can be used to
-- implement all other grouping combinators.
--
-- XXX check if this can implement the splitOn combinator i.e. we can slide in
-- new elements, slide out old elements and incrementally compute the hash.
-- Also, can we implement the windowed classification combinators using this?
--
-- In fact this is a parse. Instead of using a special return value in the fold
-- we are using a mapping function.
--
-- Note that 'scanl'' (usually followed by a map to extract the desired value
-- from the accumulator) can be used to realize many implementations e.g. a
-- sliding window implementation. A scan followed by a mapMaybe is also a good
-- pattern to express many problems where we want to emit a filtered output and
-- not emit an output on every input.
--
-- Passing on of the initial accumulator value to the next fold is equivalent
-- to returning the leftover concept.

{-
-- | @groupScan splitter fold stream@ folds the input stream using @fold@.
-- @splitter@ is applied on the accumulator of the fold every time an item is
-- consumed by the fold. The fold continues until @splitter@ returns a 'Just'
-- value.  A 'Just' result from the @splitter@ specifies a result to be emitted
-- in the output stream and the initial value of the accumulator for the next
-- group's fold. This allows us to control whether to start fresh for the next
-- fold or to continue from the previous fold's output.
--
{-# INLINE groupScan #-}
groupScan
    :: (IsStream t, Monad m)
    => (x -> m (Maybe (b, x))) -> Fold m a x -> t m a -> t m b
groupScan split fold m = undefined
-}

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >> S.toList $ S.chunksOf 2 FL.sum (S.enumerateFromTo 1 10)
-- > [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'ltake' where we apply
-- 'ltake' repeatedly on the leftover stream until the stream exhausts.
--
-- @since 0.7.0
{-# INLINE chunksOf #-}
chunksOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
chunksOf n f m = D.fromStreamD $ D.groupsOf n f (D.toStreamD m)

-- |
--
-- /Internal/
{-# INLINE chunksOf2 #-}
chunksOf2
    :: (IsStream t, Monad m)
    => Int -> m c -> Fold2 m c a b -> t m a -> t m b
chunksOf2 n action f m = D.fromStreamD $ D.groupsOf2 n action f (D.toStreamD m)

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- > arraysOf n = S.chunksOf n (A.writeN n)
--
-- /Internal/
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n = chunksOf n (writeNUnsafe n)

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ intervalsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE intervalsOf #-}
intervalsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
intervalsOf n f xs =
    splitWithSuffix isNothing (FL.lcatMaybes f)
        (interjectSuffix n (return Nothing) (Serial.map Just xs))

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------
--
-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @b \`cmp` a@ is 'True' then @b@ is also assigned to the same
-- group.  If @c \`cmp` a@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@ and the result of the fold is emitted in
-- the output stream.
--
-- >>> S.toList $ S.groupsBy (>) FL.toList $ S.fromList [1,3,7,0,2,5]
-- > [[1,3,7],[0,2,5]]
--
-- @since 0.7.0
{-# INLINE groupsBy #-}
groupsBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsBy cmp f m = D.fromStreamD $ D.groupsBy cmp f (D.toStreamD m)

-- | Unlike @groupsBy@ this function performs a rolling comparison of two
-- successive elements in the input stream. @groupsByRolling cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> S.toList $ S.groupsByRolling (\a b -> a + 1 == b) FL.toList $ S.fromList [1,2,3,7,8,9]
-- > [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
{-# INLINE groupsByRolling #-}
groupsByRolling
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsByRolling cmp f m =  D.fromStreamD $ D.groupsRollingBy cmp f (D.toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsByRolling (==)
--
-- Groups contiguous spans of equal elements together in individual groups.
--
-- >>> S.toList $ S.groups FL.toList $ S.fromList [1,1,2,2]
-- > [[1,1],[2,2]]
--
-- @since 0.7.0
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- N-ary split on a predicate
------------------------------------------------------------------------------

-- TODO: Use a Splitter configuration similar to the "split" package to make it
-- possible to express all splitting combinations. In general, we can have
-- infix/suffix/prefix/condensing of separators, dropping both leading/trailing
-- separators. We can have a single split operation taking the splitter config
-- as argument.

-- | Split on an infixed separator element, dropping the separator. Splits the
-- stream on separator elements determined by the supplied predicate, separator
-- is considered as infixed between two segments, if one side of the separator
-- is missing then it is parsed as an empty stream.  The supplied 'Fold' is
-- applied on the split segments. With '-' representing non-separator elements
-- and '.' as separator, 'splitOn' splits as follows:
--
-- @
-- "--.--" => "--" "--"
-- "--."   => "--" ""
-- ".--"   => ""   "--"
-- @
--
-- @splitOn (== x)@ is an inverse of @intercalate (S.yield x)@
--
-- Let's use the following definition for illustration:
--
-- > splitOn' p xs = S.toList $ S.splitOn p (FL.toList) (S.fromList xs)
--
-- >>> splitOn' (== '.') ""
-- [""]
--
-- >>> splitOn' (== '.') "."
-- ["",""]
--
-- >>> splitOn' (== '.') ".a"
-- > ["","a"]
--
-- >>> splitOn' (== '.') "a."
-- > ["a",""]
--
-- >>> splitOn' (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitOn' (== '.') "a..b"
-- > ["a","","b"]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakOn' where we apply
-- 'breakOn' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOn predicate f m =
    D.fromStreamD $ D.splitBy predicate f (D.toStreamD m)

-- | Like 'splitOn' but the separator is considered as suffixed to the segments
-- in the stream. A missing suffix at the end is allowed. A separator at the
-- beginning is parsed as empty segment.  With '-' representing elements and
-- '.' as separator, 'splitOnSuffix' splits as follows:
--
-- @
--  "--.--." => "--" "--"
--  "--.--"  => "--" "--"
--  ".--."   => "" "--"
-- @
--
-- > splitOnSuffix' p xs = S.toList $ S.splitSuffixBy p (FL.toList) (S.fromList xs)
--
-- >>> splitOnSuffix' (== '.') ""
-- []
--
-- >>> splitOnSuffix' (== '.') "."
-- [""]
--
-- >>> splitOnSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitOnSuffix' (== '.') ".a"
-- > ["","a"]
--
-- >>> splitOnSuffix' (== '.') "a."
-- > ["a"]
--
-- >>> splitOnSuffix' (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a.b."
-- > ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitOnSuffix (== '\n')
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnSuffix predicate f m =
    D.fromStreamD $ D.splitSuffixBy predicate f (D.toStreamD m)

-- | Like 'splitOn' after stripping leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ with '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- > wordsBy' p xs = S.toList $ S.wordsBy p (FL.toList) (S.fromList xs)
--
-- >>> wordsBy' (== ',') ""
-- > []
--
-- >>> wordsBy' (== ',') ","
-- > []
--
-- >>> wordsBy' (== ',') ",a,,b,"
-- > ["a","b"]
--
-- > words = wordsBy isSpace
--
-- @since 0.7.0

-- It is equivalent to splitting in any of the infix/prefix/suffix styles
-- followed by removal of empty segments.
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    D.fromStreamD $ D.wordsBy predicate f (D.toStreamD m)

-- | Like 'splitOnSuffix' but keeps the suffix attached to the resulting
-- splits.
--
-- > splitWithSuffix' p xs = S.toList $ S.splitWithSuffix p (FL.toList) (S.fromList xs)
--
-- >>> splitWithSuffix' (== '.') ""
-- []
--
-- >>> splitWithSuffix' (== '.') "."
-- ["."]
--
-- >>> splitWithSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitWithSuffix' (== '.') ".a"
-- > [".","a"]
--
-- >>> splitWithSuffix' (== '.') "a."
-- > ["a."]
--
-- >>> splitWithSuffix' (== '.') "a.b"
-- > ["a.","b"]
--
-- >>> splitWithSuffix' (== '.') "a.b."
-- > ["a.","b."]
--
-- >>> splitWithSuffix' (== '.') "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream.
--
{-# INLINE splitWithSuffix #-}
splitWithSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitWithSuffix predicate f m =
    D.fromStreamD $ D.splitSuffixBy' predicate f (D.toStreamD m)

------------------------------------------------------------------------------
-- Split on a delimiter sequence
------------------------------------------------------------------------------

-- Int list examples for splitOn:
--
-- >>> splitList [] [1,2,3,3,4]
-- > [[1],[2],[3],[3],[4]]
--
-- >>> splitList [5] [1,2,3,3,4]
-- > [[1,2,3,3,4]]
--
-- >>> splitList [1] [1,2,3,3,4]
-- > [[],[2,3,3,4]]
--
-- >>> splitList [4] [1,2,3,3,4]
-- > [[1,2,3,3],[]]
--
-- >>> splitList [2] [1,2,3,3,4]
-- > [[1],[3,3,4]]
--
-- >>> splitList [3] [1,2,3,3,4]
-- > [[1,2],[],[4]]
--
-- >>> splitList [3,3] [1,2,3,3,4]
-- > [[1,2],[4]]
--
-- >>> splitList [1,2,3,3,4] [1,2,3,3,4]
-- > [[],[]]

-- | Like 'splitOn' but the separator is a sequence of elements instead of a
-- single element.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- @
-- splitOnSeq' pat xs = S.toList $ S.splitOnSeq (A.fromList pat) (FL.toList) (S.fromList xs)
-- @
--
-- >>> splitOnSeq' "" "hello"
-- > ["h","e","l","l","o"]
--
-- >>> splitOnSeq' "hello" ""
-- > [""]
--
-- >>> splitOnSeq' "hello" "hello"
-- > ["",""]
--
-- >>> splitOnSeq' "x" "hello"
-- > ["hello"]
--
-- >>> splitOnSeq' "h" "hello"
-- > ["","ello"]
--
-- >>> splitOnSeq' "o" "hello"
-- > ["hell",""]
--
-- >>> splitOnSeq' "e" "hello"
-- > ["h","llo"]
--
-- >>> splitOnSeq' "l" "hello"
-- > ["he","","o"]
--
-- >>> splitOnSeq' "ll" "hello"
-- > ["he","o"]
--
-- 'splitOnSeq' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- /Internal/

-- XXX We can use a polymorphic vector implemented by Array# to represent the
-- sequence, that way we can avoid the Storable constraint. If we still need
-- Storable Array for performance, we can use a separate splitOnArray API for
-- that. We can also have an API where the sequence itself is a lazy stream, so
-- that we can search files in files for example.
{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ D.splitOn patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
{-# INLINE splitOnAny #-}
splitOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnAny subseq f m = undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)
-}

-- | Like 'splitSuffixBy' but the separator is a sequence of elements, instead
-- of a predicate for a single element.
--
-- > splitSuffixOn_ pat xs = S.toList $ S.splitSuffixOn (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn_ "." ""
-- [""]
--
-- >>> splitSuffixOn_ "." "."
-- [""]
--
-- >>> splitSuffixOn_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn_ "." ".a"
-- > ["","a"]
--
-- >>> splitSuffixOn_ "." "a."
-- > ["a"]
--
-- >>> splitSuffixOn_ "." "a.b"
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a.b."
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitSuffixOn "\n"
--
-- /Internal/
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSuffixSeq patt f m =
    D.fromStreamD $ D.splitSuffixOn False patt f (D.toStreamD m)

{-
-- | Like 'splitOn' but drops any empty splits.
--
{-# INLINE wordsOn #-}
wordsOn
    :: (IsStream t, Monad m, Storable a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
wordsOn subseq f m = undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)
-}

-- XXX use a non-monadic intersperse to remove the MonadAsync constraint.
--
-- | Like 'splitOnSeq' but splits the separator as well, as an infix token.
--
-- > splitOn'_ pat xs = S.toList $ S.splitOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitOn'_ "" "hello"
-- > ["h","","e","","l","","l","","o"]
--
-- >>> splitOn'_ "hello" ""
-- > [""]
--
-- >>> splitOn'_ "hello" "hello"
-- > ["","hello",""]
--
-- >>> splitOn'_ "x" "hello"
-- > ["hello"]
--
-- >>> splitOn'_ "h" "hello"
-- > ["","h","ello"]
--
-- >>> splitOn'_ "o" "hello"
-- > ["hell","o",""]
--
-- >>> splitOn'_ "e" "hello"
-- > ["h","e","llo"]
--
-- >>> splitOn'_ "l" "hello"
-- > ["he","l","","l","o"]
--
-- >>> splitOn'_ "ll" "hello"
-- > ["he","ll","o"]
--
-- /Internal/
{-# INLINE splitBySeq #-}
splitBySeq
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitBySeq patt f m =
    intersperseM (fold f (A.toStream patt)) $ splitOnSeq patt f m

-- | Like 'splitSuffixOn' but keeps the suffix intact in the splits.
--
-- > splitSuffixOn'_ pat xs = S.toList $ FL.splitSuffixOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn'_ "." ""
-- [""]
--
-- >>> splitSuffixOn'_ "." "."
-- ["."]
--
-- >>> splitSuffixOn'_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn'_ "." ".a"
-- > [".","a"]
--
-- >>> splitSuffixOn'_ "." "a."
-- > ["a."]
--
-- >>> splitSuffixOn'_ "." "a.b"
-- > ["a.","b"]
--
-- >>> splitSuffixOn'_ "." "a.b."
-- > ["a.","b."]
--
-- >>> splitSuffixOn'_ "." "a..b.."
-- > ["a.",".","b.","."]
--
-- /Internal/
{-# INLINE splitWithSuffixSeq #-}
splitWithSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitWithSuffixSeq patt f m =
    D.fromStreamD $ D.splitSuffixOn True patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
{-# INLINE splitSuffixOnAny #-}
splitSuffixOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitSuffixOnAny subseq f m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)
-}

------------------------------------------------------------------------------
-- Nested Split
------------------------------------------------------------------------------

-- | @splitInnerBy splitter joiner stream@ splits the inner containers @f a@ of
-- an input stream @t m (f a)@ using the @splitter@ function. Container
-- elements @f a@ are collected until a split occurs, then all the elements
-- before the split are joined using the @joiner@ function.
--
-- For example, if we have a stream of @Array Word8@, we may want to split the
-- stream into arrays representing lines separated by '\n' byte such that the
-- resulting stream after a split would be one array for each line.
--
-- CAUTION! This is not a true streaming function as the container size after
-- the split and merge may not be bounded.
--
-- /Internal/
{-# INLINE splitInnerBy #-}
splitInnerBy
    :: (IsStream t, Monad m)
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBy splitter joiner xs =
    D.fromStreamD $ D.splitInnerBy splitter joiner $ D.toStreamD xs

-- | Like 'splitInnerBy' but splits assuming the separator joins the segment in
-- a suffix style.
--
-- /Internal/
{-# INLINE splitInnerBySuffix #-}
splitInnerBySuffix
    :: (IsStream t, Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBySuffix splitter joiner xs =
    D.fromStreamD $ D.splitInnerBySuffix splitter joiner $ D.toStreamD xs

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

{-
-- Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
{-# INLINE reassembleBy #-}
reassembleBy
    :: (IsStream t, Monad m)
    => Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined
-}

------------------------------------------------------------------------------
-- Distributing
------------------------------------------------------------------------------

-- | Tap the data flowing through a stream into a 'Fold'. For example, you may
-- add a tap to log the contents flowing through the stream. The fold is used
-- only for effects, its result is discarded.
--
-- @
--                   Fold m a b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- @
-- > S.drain $ S.tap (FL.drainBy print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Compare with 'trace'.
--
-- @since 0.7.0
{-# INLINE tap #-}
tap :: (IsStream t, Monad m) => FL.Fold m a b -> t m a -> t m a
tap f xs = D.fromStreamD $ D.tap f (D.toStreamD xs)

-- | @tapOffsetEvery offset n@ taps every @n@th element in the stream
-- starting at @offset@. @offset@ can be between @0@ and @n - 1@. Offset 0
-- means start at the first element in the stream. If the offset is outside
-- this range then @offset `mod` n@ is used as offset.
--
-- @
-- >>> S.drain $ S.tapOffsetEvery 0 2 (FL.mapM print FL.toList) $ S.enumerateFromTo 0 10
-- > [0,2,4,6,8,10]
-- @
--
-- /Internal/
--
{-# INLINE tapOffsetEvery #-}
tapOffsetEvery :: (IsStream t, Monad m)
    => Int -> Int -> FL.Fold m a b -> t m a -> t m a
tapOffsetEvery offset n f xs =
    D.fromStreamD $ D.tapOffsetEvery offset n f (D.toStreamD xs)

-- | Redirect a copy of the stream to a supplied fold and run it concurrently
-- in an independent thread. The fold may buffer some elements. The buffer size
-- is determined by the prevailing 'maxBuffer' setting.
--
-- @
--               Stream m a -> m b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- @
-- > S.drain $ S.tapAsync (S.mapM_ print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Exceptions from the concurrently running fold are propagated to the current
-- computation.  Note that, because of buffering in the fold, exceptions may be
-- delayed and may not correspond to the current element being processed in the
-- parent stream, but we guarantee that before the parent stream stops the tap
-- finishes and all exceptions from it are drained.
--
--
-- Compare with 'tap'.
--
-- /Internal/
{-# INLINE tapAsync #-}
tapAsync :: (IsStream t, MonadAsync m) => FL.Fold m a b -> t m a -> t m a
tapAsync f xs = D.fromStreamD $ D.tapAsync f (D.toStreamD xs)

-- | @pollCounts predicate transform fold stream@ counts those elements in the
-- stream that pass the @predicate@. The resulting count stream is sent to
-- another thread which transforms it using @transform@ and then folds it using
-- @fold@.  The thread is automatically cleaned up if the stream stops or
-- aborts due to exception.
--
-- For example, to print the count of elements processed every second:
--
-- @
-- > S.drain $ S.pollCounts (const True) (S.rollingMap (-) . S.delayPost 1) (FL.drainBy print)
--           $ S.enumerateFrom 0
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Internal/
--
{-# INLINE pollCounts #-}
pollCounts ::
       (IsStream t, MonadAsync m)
    => (a -> Bool)
    -> (t m Int -> t m Int)
    -> Fold m Int b
    -> t m a
    -> t m a
pollCounts predicate transf f xs =
      D.fromStreamD
    $ D.pollCounts predicate (D.toStreamD . transf . D.fromStreamD) f
    $ D.toStreamD xs

-- | Calls the supplied function with the number of elements consumed
-- every @n@ seconds. The given function is run in a separate thread
-- until the end of the stream. In case there is an exception in the
-- stream the thread is killed during the next major GC.
--
-- Note: The action is not guaranteed to run if the main thread exits.
--
-- @
-- > delay n = threadDelay (round $ n * 1000000) >> return n
-- > S.drain $ S.tapRate 2 (\\n -> print $ show n ++ " elements processed") (delay 1 S.|: delay 0.5 S.|: delay 0.5 S.|: S.nil)
-- 2 elements processed
-- 1 elements processed
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Internal/
{-# INLINE tapRate #-}
tapRate ::
       (IsStream t, MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> t m a
    -> t m a
tapRate n f xs = D.fromStreamD $ D.tapRate n f $ D.toStreamD xs

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- @
-- > S.drain $ S.trace print (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Compare with 'tap'.
--
-- @since 0.7.0
{-# INLINE trace #-}
trace :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m a
trace f = mapM (\x -> void (f x) >> return x)

-- | Perform a side effect before yielding each element of the stream and
-- discard the results.
--
-- @
-- > S.drain $ S.trace_ (print "got here") (S.enumerateFromTo 1 2)
-- "got here"
-- "got here"
-- @
--
-- See also: 'trace', 'interspersePrefix_'
--
-- /Internal/
{-# INLINE trace_ #-}
trace_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
trace_ eff = Serial.mapM (\x -> eff >> return x)

------------------------------------------------------------------------------
-- Windowed classification
------------------------------------------------------------------------------

-- We divide the stream into windows or chunks in space or time and each window
-- can be associated with a key, all events associated with a particular key in
-- the window can be folded to a single result. The stream can be split into
-- windows by size or by using a split predicate on the elements in the stream.
-- For example, when we receive a closing flag, we can close the window.
--
-- A "chunk" is a space window and a "session" is a time window. Are there any
-- other better short words to describe them. An alternative is to use
-- "swindow" and "twindow". Another word for "session" could be "spell".
--
-- TODO: To mark the position in space or time we can have Indexed or
-- TimeStamped types. That can make it easy to deal with the position indices
-- or timestamps.

------------------------------------------------------------------------------
-- Keyed Sliding Windows
------------------------------------------------------------------------------

{-
{-# INLINABLE classifySlidingChunks #-}
classifySlidingChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Int              -- ^ window slide
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifySlidingChunks wsize wslide (Fold step initial extract) str
    = undefined

-- XXX Another variant could be to slide the window on an event, e.g. in TCP we
-- slide the send window when an ack is received and we slide the receive
-- window when a sequence is complete. Sliding is stateful in case of TCP,
-- sliding releases the send buffer or makes data available to the user from
-- the receive buffer.
{-# INLINABLE classifySlidingSessions #-}
classifySlidingSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ time window size
    -> Double         -- ^ window slide
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySlidingSessions tick interval slide (Fold step initial extract) str
    = undefined
-}

------------------------------------------------------------------------------
-- Sliding Window Buffers
------------------------------------------------------------------------------

-- These buffered versions could be faster than concurrent incremental folds of
-- all overlapping windows as in many cases we may not need all the values to
-- compute the fold, we can just compute the result using the old value and new
-- value.  However, we may need the buffer once in a while, for example for
-- string search we usually compute the hash incrementally but when the hash
-- matches the hash of the pattern we need to compare the whole string.
--
-- XXX we should be able to implement sequence based splitting combinators
-- using this combinator.

{-
-- | Buffer n elements of the input in a ring buffer. When t new elements are
-- collected, slide the window to remove the same number of oldest elements,
-- insert the new elements, and apply an incremental fold on the sliding
-- window, supplying the outgoing elements, the new ring buffer as arguments.
slidingChunkBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int -- window size
    -> Int -- window slide
    -> Fold m (Ring a, Array a) b
    -> t m a
    -> t m b
slidingChunkBuffer = undefined

-- Buffer n seconds worth of stream elements of the input in a radix tree.
-- Every t seconds, remove the items that are older than n seconds, and apply
-- an incremental fold on the sliding window, supplying the outgoing elements,
-- and the new radix tree buffer as arguments.
slidingSessionBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int    -- window size
    -> Int    -- tick size
    -> Fold m (RTree a, Array a) b
    -> t m a
    -> t m b
slidingSessionBuffer = undefined
-}

------------------------------------------------------------------------------
-- Keyed Session Windows
------------------------------------------------------------------------------

{-
-- | Keyed variable size space windows. Close the window if we do not receive a
-- window event in the next "spaceout" elements.
{-# INLINABLE classifyChunksBy #-}
classifyChunksBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Bool  -- ^ reset the spaceout when a chunk window element is received
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyChunksBy spanout reset (Fold step initial extract) str = undefined

-- | Like 'classifyChunksOf' but the chunk size is reset if an element is
-- received within the chunk size window. The chunk gets closed only if no
-- element is received within the chunk window.
--
{-# INLINABLE classifyKeepAliveChunks #-}
classifyKeepAliveChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyKeepAliveChunks spanout = classifyChunksBy spanout True
-}

data SessionState t m k a b = SessionState
    { sessionCurTime :: !AbsTime  -- ^ time since last event
    , sessionEventTime :: !AbsTime -- ^ time as per last event
    , sessionCount :: !Int -- ^ total number sessions in progress
    , sessionTimerHeap :: H.Heap (H.Entry AbsTime k) -- ^ heap for timeouts
    , sessionKeyValueMap :: Map.Map k a -- ^ Stored sessions for keys
    , sessionOutputStream :: t (m :: Type -> Type) (k, b) -- ^ Completed sessions
    }

#undef Type

-- | @classifySessionsBy tick timeout idle pred f stream@ groups timestamped
-- events in an input event stream into sessions based on a session key. Each
-- element in the stream is an event consisting of a triple @(session key,
-- sesssion data, timestamp)@.  @session key@ is a key that uniquely identifies
-- the session.  All the events belonging to a session are folded using the
-- fold @f@ until the fold returns a 'Left' result or a timeout has occurred.
-- The session key and the result of the fold are emitted in the output stream
-- when the session is purged.
--
-- When @idle@ is 'False', @timeout@ is the maximum lifetime of a session in
-- seconds, measured from the @timestamp@ of the first event in that session.
-- When @idle@ is 'True' then the timeout is an idle timeout, it is reset after
-- every event received in the session.
--
-- @timestamp@ in an event characterizes the time when the input event was
-- generated, this is an absolute time measured from some @Epoch@.  The notion
-- of current time is maintained by a monotonic event time clock using the
-- timestamps seen in the input stream. The latest timestamp seen till now is
-- used as the base for the current time.  When no new events are seen, a timer
-- is started with a tick duration specified by @tick@. This timer is used to
-- detect session timeouts in the absence of new events.
--
-- The predicate @pred@ is invoked with the current session count, if it
-- returns 'True' a session is ejected from the session cache before inserting
-- a new session. This could be useful to alert or eject sessions when the
-- number of sessions becomes too high.
--
-- /Internal/
--

-- XXX Perhaps we should use an "Event a" type to represent timestamped data.
{-# INLINABLE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ session timeout in seconds
    -> Bool           -- ^ reset the timeout when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Fold m a (Either b b) -- ^ Fold to be applied to session events
    -> t m (k, a, AbsTime) -- ^ session key, data, timestamp
    -> t m (k, b) -- ^ session key, fold result
classifySessionsBy tick tmout reset ejectPred
    (Fold step initial extract) str =
    concatMap sessionOutputStream $
        scanlMAfter' sstep (return szero) flush stream

    where

    timeoutMs = toRelTime (round (tmout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = SessionState
        { sessionCurTime = toAbsTime (0 :: MilliSecond64)
        , sessionEventTime = toAbsTime (0 :: MilliSecond64)
        , sessionCount = 0
        , sessionTimerHeap = H.empty
        , sessionKeyValueMap = Map.empty
        , sessionOutputStream = K.nil
        }

    -- We can eject sessions based on the current session count to limit
    -- memory consumption. There are two possible strategies:
    --
    -- 1) Eject old sessions or sessions beyond a certain/lower timeout
    -- threshold even before timeout, effectively reduce the timeout.
    -- 2) Drop creation of new sessions but keep accepting new events for the
    -- old ones.
    --
    -- We use the first strategy as of now.

    -- Got a new stream input element
    sstep session@SessionState{..} (Just (key, value, timestamp)) = do
        -- XXX we should use a heap in pinned memory to scale it to a large
        -- size
        --
        -- To detect session inactivity we keep a timestamp of the latest event
        -- in the Map along with the fold result.  When we purge the session
        -- from the heap we match the timestamp in the heap with the timestamp
        -- in the Map, if the latest timestamp is newer and has not expired we
        -- reinsert the key in the heap.
        --
        -- XXX if the key is an Int, we can also use an IntMap for slightly
        -- better performance.
        --
        let curTime = max sessionEventTime timestamp
            accumulate v = do
                old <- case v of
                    Nothing -> initial
                    Just (Tuple' _ acc) -> return acc
                new <- step old value
                return $ Tuple' timestamp new
            mOld = Map.lookup key sessionKeyValueMap

        acc@(Tuple' _ fres) <- accumulate mOld
        res <- extract fres
        case res of
            Left x -> do
                -- deleting a key from the heap is expensive, so we never
                -- delete a key from heap, we just purge it from the Map and it
                -- gets purged from the heap on timeout. We just need an extra
                -- lookup in the Map when the key is purged from the heap, that
                -- should not be expensive.
                --
                let (mp, cnt) = case mOld of
                        Nothing -> (sessionKeyValueMap, sessionCount)
                        Just _ -> (Map.delete key sessionKeyValueMap
                                  , sessionCount - 1)
                return $ session
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt
                    , sessionKeyValueMap = mp
                    , sessionOutputStream = yield (key, x)
                    }
            Right _ -> do
                (hp1, mp1, out1, cnt1) <- do
                        let vars = (sessionTimerHeap, sessionKeyValueMap,
                                           K.nil, sessionCount)
                        case mOld of
                            -- inserting new entry
                            Nothing -> do
                                -- Eject a session from heap and map is needed
                                eject <- ejectPred sessionCount
                                (hp, mp, out, cnt) <-
                                    if eject
                                    then ejectOne vars
                                    else return vars

                                -- Insert the new session in heap
                                let expiry = addToAbsTime timestamp timeoutMs
                                    hp' = H.insert (Entry expiry key) hp
                                 in return (hp', mp, out, cnt + 1)
                            -- updating old entry
                            Just _ -> return vars

                let mp2 = Map.insert key acc mp1
                return $ SessionState
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt1
                    , sessionTimerHeap = hp1
                    , sessionKeyValueMap = mp2
                    , sessionOutputStream = out1
                    }

    -- Got a timer tick event
    sstep sessionState@SessionState{..} Nothing =
        let curTime = addToAbsTime sessionCurTime tickMs
        in ejectExpired sessionState curTime

    flush session@SessionState{..} = do
        (hp', mp', out, count) <-
            ejectAll
                ( sessionTimerHeap
                , sessionKeyValueMap
                , K.nil
                , sessionCount
                )
        return $ session
            { sessionCount = count
            , sessionTimerHeap = hp'
            , sessionKeyValueMap = mp'
            , sessionOutputStream = out
            }

    fromEither e =
        case e of
            Left  x -> x
            Right x -> x

    -- delete from map and output the fold accumulator
    ejectEntry hp mp out cnt acc key = do
        sess <- extract acc
        let out1 = (key, fromEither sess) `K.cons` out
        let mp1 = Map.delete key mp
        return (hp, mp1, out1, cnt - 1)

    ejectAll (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry _ key, hp1) -> do
                r <- case Map.lookup key mp of
                    Nothing -> return (hp1, mp, out, cnt)
                    Just (Tuple' _ acc) -> ejectEntry hp1 mp out cnt acc key
                ejectAll r
            Nothing -> do
                assert (Map.null mp) (return ())
                return (hp, mp, out, cnt)

    ejectOne (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) ->
                case Map.lookup key mp of
                    Nothing -> ejectOne (hp1, mp, out, cnt)
                    Just (Tuple' latestTS acc) -> do
                        let expiry1 = addToAbsTime latestTS timeoutMs
                        if not reset || expiry1 <= expiry
                        then ejectEntry hp1 mp out cnt acc key
                        else
                            -- reset the session timeout and continue
                            let hp2 = H.insert (Entry expiry1 key) hp1
                            in ejectOne (hp2, mp, out, cnt)
            Nothing -> do
                assert (Map.null mp) (return ())
                return (hp, mp, out, cnt)

    ejectExpired session@SessionState{..} curTime = do
        (hp', mp', out, count) <-
            ejectLoop sessionTimerHeap sessionKeyValueMap K.nil sessionCount
        return $ session
            { sessionCurTime = curTime
            , sessionCount = count
            , sessionTimerHeap = hp'
            , sessionKeyValueMap = mp'
            , sessionOutputStream = out
            }

        where

        ejectLoop hp mp out !cnt = do
            let hres = H.uncons hp
            case hres of
                Just (Entry expiry key, hp1) -> do
                    (eject, force) <-
                        if curTime >= expiry
                        then return (True, False)
                        else do
                            r <- ejectPred cnt
                            return (r, r)
                    if eject
                    then
                        case Map.lookup key mp of
                            Nothing -> ejectLoop hp1 mp out cnt
                            Just (Tuple' latestTS acc) -> do
                                let expiry1 = addToAbsTime latestTS timeoutMs
                                if expiry1 <= curTime || not reset || force
                                then do
                                    (hp2,mp1,out1,cnt1) <-
                                        ejectEntry hp1 mp out cnt acc key
                                    ejectLoop hp2 mp1 out1 cnt1
                                else
                                    -- reset the session timeout and continue
                                    let hp2 = H.insert (Entry expiry1 key) hp1
                                    in ejectLoop hp2 mp out cnt
                    else return (hp, mp, out, cnt)
                Nothing -> do
                    assert (Map.null mp) (return ())
                    return (hp, mp, out, cnt)

    -- merge timer events in the stream
    stream = Serial.map Just str `Par.parallelFst` repeatM timer
    timer = do
        liftIO $ threadDelay (round $ tick * 1000000)
        return Nothing

-- | Like 'classifySessionsOf' but the session is kept alive if an event is
-- received within the session window. The session times out and gets closed
-- only if no event is received within the specified session window size.
--
-- If the ejection predicate returns 'True', the session that was idle for
-- the longest time is ejected before inserting a new session.
--
-- @
-- classifyKeepAliveSessions timeout pred = classifySessionsBy 1 timeout True pred
-- @
--
-- /Internal/
--
{-# INLINABLE classifyKeepAliveSessions #-}
classifyKeepAliveSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ session inactive timeout
    -> (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Fold m a (Either b b) -- ^ Fold to be applied to session payload data
    -> t m (k, a, AbsTime) -- ^ session key, data, timestamp
    -> t m (k, b)
classifyKeepAliveSessions tmout =
    classifySessionsBy 1 tmout True

------------------------------------------------------------------------------
-- Keyed tumbling windows
------------------------------------------------------------------------------

-- Tumbling windows is a special case of sliding windows where the window slide
-- is the same as the window size. Or it can be a special case of session
-- windows where the reset flag is set to False.

-- XXX instead of using the early termination flag in the stream, we can use an
-- early terminating fold instead.

{-
-- | Split the stream into fixed size chunks of specified size. Within each
-- such chunk fold the elements in buckets identified by the keys. A particular
-- bucket fold can be terminated early if a closing flag is encountered in an
-- element for that key.
--
-- @since 0.7.0
{-# INLINABLE classifyChunksOf #-}
classifyChunksOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifyChunksOf wsize = classifyChunksBy wsize False
-}

-- | Split the stream into fixed size time windows of specified interval in
-- seconds. Within each such window, fold the elements in sessions identified
-- by the session keys. The fold result is emitted in the output stream if the
-- fold returns a 'Left' result or if the time window ends.
--
-- Session @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input element was generated.  To detect
-- session window end, a monotonic event time clock is maintained synced with
-- the timestamps with a clock resolution of 1 second.
--
-- If the ejection predicate returns 'True', the session with the longest
-- lifetime is ejected before inserting a new session.
--
-- @
-- classifySessionsOf interval pred = classifySessionsBy 1 interval False pred
-- @
--
-- @
-- >>> S.mapM_ print
--   $ S.classifySessionsOf 3 (const (return False)) (fmap Right FL.toList)
--   $ S.map (\(ts,(k,a)) -> (k, a, ts))
--   $ S.timestamped
--   $ S.delay 1
--   $ (,) <$> S.fromList [1,2,3] <*> S.fromList [1,2,3]
-- @
--
-- /Internal/
--
{-# INLINABLE classifySessionsOf #-}
classifySessionsOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ time window size
    -> (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Fold m a (Either b b) -- ^ Fold to be applied to session events
    -> t m (k, a, AbsTime) -- ^ session key, data, timestamp
    -> t m (k, b)
classifySessionsOf interval =
    classifySessionsBy 1 interval False

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Run a side effect before the stream yields its first element.
--
-- > before action xs = 'nilM' action <> xs
--
-- @since 0.7.0
{-# INLINE before #-}
before :: (IsStream t, Monad m) => m b -> t m a -> t m a
before action xs = D.fromStreamD $ D.before action $ D.toStreamD xs

-- | Run a side effect at the end of the stream. The side effect won't run if
-- the stream is garbage collected before it reached the end.
--
-- > after_ action xs = xs <> 'nilM' action
--
-- This has slightly better performance than 'after'.
--
-- /Internal/
--
{-# INLINE after_ #-}
after_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
after_ action xs = D.fromStreamD $ D.after_ action $ D.toStreamD xs

-- | Run a side effect at the end of the stream or if it is garbage collected
-- even before reaching the end.
--
-- @since 0.7.0
--
{-# INLINE after #-}
after :: (IsStream t, MonadIO m, MonadBaseControl IO m)
    => m b -> t m a -> t m a
after action xs = D.fromStreamD $ D.after action $ D.toStreamD xs

-- | Run a side effect whenever the stream aborts due to an exception.
--
-- @since 0.7.0
{-# INLINE onException #-}
onException :: (IsStream t, MonadCatch m) => m b -> t m a -> t m a
onException action xs = D.fromStreamD $ D.onException action $ D.toStreamD xs

-- | Run a side effect at the end of the stream or if it aborts due to an
-- exception before it could reach the end. The side effect is not run if the
-- stream is garbage collected before reaching the end.
--
-- This has slightly better performance than 'finally'.
--
-- /Internal/
--
{-# INLINE finally_ #-}
finally_ :: (IsStream t, MonadCatch m) => m b -> t m a -> t m a
finally_ action xs = D.fromStreamD $ D.finally_ action $ D.toStreamD xs

-- | Run a side effect at the end of the stream, or if it aborts due to an
-- exception or if it is garbage collected before it could reach the end.
--
-- @since 0.7.0
--
{-# INLINE finally #-}
finally :: (IsStream t, MonadAsync m, MonadCatch m) => m b -> t m a -> t m a
finally action xs = D.fromStreamD $ D.finally action $ D.toStreamD xs

-- | Run the first action before the stream starts and remember its output,
-- generate a stream using the output, run the second action using the
-- remembered value as an argument whenever the stream ends normally or due to
-- an exception. The second action won't run if the stream is garbage collected
-- before it could reach the end.
--
-- This has slightly better performance than 'bracket'.
--
-- /Internal/
--
{-# INLINE bracket_ #-}
bracket_ :: (IsStream t, MonadCatch m)
    => m b -> (b -> m c) -> (b -> t m a) -> t m a
bracket_ bef aft bet = D.fromStreamD $
    D.bracket_ bef aft (toStreamD . bet)

-- | Run the first action before the stream starts and remember its output,
-- generate a stream using the output, run the second action using the
-- remembered value as an argument whenever the stream ends normally, due to
-- an exception or if it is garbage collected after a partial lazy evaluation.
--
-- @since 0.7.0
--
{-# INLINE bracket #-}
bracket :: (IsStream t, MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> t m a) -> t m a
bracket bef aft bet = D.fromStreamD $
    D.bracket bef aft (toStreamD . bet)

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
--
-- @since 0.7.0
{-# INLINE handle #-}
handle :: (IsStream t, MonadCatch m, Exception e)
    => (e -> t m a) -> t m a -> t m a
handle handler xs =
    D.fromStreamD $ D.handle (D.toStreamD . handler) $ D.toStreamD xs

-- | @retry@ takes 3 arguments
--
-- 1. A map @m@ whose keys are exceptions and values are the number of times to
-- retry the action given that the exception occures.
--
-- 2. A handler @han@ that decides how to handle an exception not existing in
-- the map.
--
-- 3. The stream itself that we want to run this mechanism on.
--
-- When evaluating a stream if an exception occurs,
--
-- 1. The stream evaluation aborts
--
-- 2. The exception is looked up in @m@
--
--    a. If the exception exists and the mapped value is > 0 then,
--
--       i. The value is decreased by 1.
--
--       ii. The stream is resumed from where the exception was called, retrying
--       the action.
--
--    b. If the exception exists and the mapped value is == 0 then the stream
--    evaluation stops.
--
--    c. If the exception does not exist then we handle the exception using
--    @han@.
--
-- /Latest/
--
{-# INLINE retry #-}
retry :: (IsStream t, MonadCatch m, Exception e, Ord e)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> t m a)
       -- ^ default handler for those exceptions that are not in the map
    -> t m a
    -> t m a
retry emap handler inp =
    D.fromStreamD $ D.retry emap (D.toStreamD . handler) $ D.toStreamD inp

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

------------------------------------------------------------------------------
-- Generalize the underlying monad
------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- / Internal/
--
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> SerialT m a -> SerialT n a
hoist f xs = fromStreamS $ S.hoist f (toStreamS xs)

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- / Internal/
--
{-# INLINE generally #-}
generally :: (IsStream t, Monad m) => t Identity a -> t m a
generally xs = fromStreamS $ S.hoist (return . runIdentity) (toStreamS xs)

------------------------------------------------------------------------------
-- Add and remove a monad transformer
------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of a stream @t m a@ to @tr m@ using the monad
-- transformer @tr@.
--
-- / Internal/
--
{-# INLINE liftInner #-}
liftInner :: (Monad m, IsStream t, MonadTrans tr, Monad (tr m))
    => t m a -> t (tr m) a
liftInner xs = fromStreamD $ D.liftInner (toStreamD xs)

------------------------------------------------------------------------------
-- Sharing read only state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'ReaderT'.
--
-- / Internal/
--
{-# INLINE runReaderT #-}
runReaderT :: (IsStream t, Monad m) => m s -> t (ReaderT s m) a -> t m a
runReaderT s xs = fromStreamD $ D.runReaderT s (toStreamD xs)

-- | Run a stream transformation using a given environment.
--
-- See also: 'Serial.map'
--
-- / Internal/
--
{-# INLINE usingReaderT #-}
usingReaderT
    :: (Monad m, IsStream t)
    => m r
    -> (t (ReaderT r m) a -> t (ReaderT r m) a)
    -> t m a
    -> t m a
usingReaderT r f xs = runReaderT r $ f $ liftInner xs

------------------------------------------------------------------------------
-- Sharing read write state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'StateT'.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- / Internal/
--
{-# INLINE evalStateT #-}
evalStateT ::  Monad m => m s -> SerialT (StateT s m) a -> SerialT m a
evalStateT s xs = fromStreamD $ D.evalStateT s (toStreamD xs)

-- | Run a stateful (StateT) stream transformation using a given state.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- See also: 'scanl''
--
-- / Internal/
--
{-# INLINE usingStateT #-}
usingStateT
    :: Monad m
    => m s
    -> (SerialT (StateT s m) a -> SerialT (StateT s m) a)
    -> SerialT m a
    -> SerialT m a
usingStateT s f xs = evalStateT s $ f $ liftInner xs

-- | Evaluate the inner monad of a stream as 'StateT' and emit the resulting
-- state and value pair after each step.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- / Internal/
--
{-# INLINE runStateT #-}
runStateT :: Monad m => m s -> SerialT (StateT s m) a -> SerialT m (s, a)
runStateT s xs = fromStreamD $ D.runStateT s (toStreamD xs)
