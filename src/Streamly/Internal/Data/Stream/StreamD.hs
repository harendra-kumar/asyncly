#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD
-- Copyright   : (c) 2018 Harendra Kumar
--               (c) Roman Leshchinskiy 2008-2010
--               (c) The University of Glasgow, 2009
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style re-implementation of CPS style stream in StreamK module.  The
-- symbol or suffix 'D' in this module denotes the "Direct" style.  GHC is able
-- to INLINE and fuse direct style better, providing better performance than
-- CPS implementation.
--
-- @
-- import qualified Streamly.Internal.Data.Stream.StreamD as D
-- @

-- Some of the functions in this file have been adapted from the vector
-- library,  https://hackage.haskell.org/package/vector.

module Streamly.Internal.Data.Stream.StreamD
    (
    -- * The stream type
      Step (..)
    , Stream (Stream, UnStream)

    -- * Construction
    , nil
    , nilM
    , cons

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM
    , unfold

    -- ** Specialized Generation
    -- | Generate a monadic stream from a seed.
    , repeat
    , repeatM
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , generate
    , generateM
    , iterate
    , iterateM

    -- ** Enumerations
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    , enumerateFromStepNum
    , numFrom
    , numFromThen
    , enumerateFromToFractional
    , enumerateFromThenToFractional

    -- ** Time
    , times

    -- ** Conversions
    -- | Transform an input structure into a stream.
    -- | Direct style stream does not support @fromFoldable@.
    , yield
    , yieldM
    , fromList
    , fromListM
    , fromStreamK
    , fromStreamD
    , fromPrimIORef
    , fromSVar

    -- * Elimination
    -- ** General Folds
    , foldrS
    , foldrT
    , foldrM
    , foldrMx
    , foldr
    , foldr1

    , foldl'
    , foldlM'
    , foldlS
    , foldlT
    , reverse
    , reverse'

    , foldlx'
    , foldlMx'
    , runFold

    , parselMx'
    , parseMany
    , parseIterate

    -- ** Specialized Folds
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts
    , drain
    , null
    , head
    , headElse
    , tail
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)
    , toSVarParallel

    -- ** Flattening nested streams
    , concatMapM
    , concatMap
    , ConcatMapUState (..)
    , concatMapU
    , ConcatUnfoldInterleaveState (..)
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin
    , AppendState(..)
    , append
    , InterleaveState(..)
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix
    , roundRobin -- interleaveFair?/ParallelFair
    , gintercalateSuffix
    , interposeSuffix
    , gintercalate
    , interpose

    -- ** Grouping
    , groupsOf
    , groupsOf2
    , groupsBy
    , groupsRollingBy

    -- ** Splitting
    , splitBy
    , splitSuffixBy
    , wordsBy
    , splitSuffixBy'

    , splitOn
    , splitSuffixOn

    , splitInnerBy
    , splitInnerBySuffix

    -- ** Substreams
    , isPrefixOf
    , isSubsequenceOf
    , stripPrefix

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    -- | Transform a stream into another type.
    , toList
    , toListRev
    , toStreamK
    , toStreamD

    , hoist
    , generally

    , liftInner
    , runReaderT
    , evalStateT
    , runStateT

    -- * Transformation
    , transform

    -- ** By folding (scans)
    , scanlM'
    , scanlMAfter'
    , scanl'
    , scanlM
    , scanl
    , scanl1M'
    , scanl1'
    , scanl1M
    , scanl1

    , prescanl'
    , prescanlM'

    , postscanl
    , postscanlM
    , postscanl'
    , postscanlM'
    , postscanlMAfter'

    , postscanlx'
    , postscanlMx'
    , scanlMx'
    , scanlx'

    -- * Filtering
    , filter
    , filterM
    , uniq
    , take
    , takeByTime
    , takeWhile
    , takeWhileM
    , drop
    , dropByTime
    , dropWhile
    , dropWhileM

    -- * Mapping
    , map
    , mapM
    , sequence
    , rollingMap
    , rollingMapM

    -- * Inserting
    , intersperseM
    , intersperseM_
    , intersperse
    , intersperseSuffix
    , intersperseSuffix_
    , intersperseSuffixBySpan
    , insertBy

    -- * Deleting
    , deleteBy

    -- ** Map and Filter
    , mapMaybe
    , mapMaybeM

    -- * Zipping
    , indexed
    , indexedR
    , zipWith
    , zipWithM

    -- * Comparisons
    , eqBy
    , cmpBy

    -- * Merging
    , mergeBy
    , mergeByM

    -- * Transformation comprehensions
    , the

    -- * Exceptions
    , newFinalizedIORef
    , runIORefFinalizer
    , withIORefFinalizer
    , gbracket_
    , gbracket
    , before
    , after_
    , after
    , bracket_
    , bracket
    , onException
    , finally_
    , finally
    , ghandle
    , handle
    , retry

    -- * Concurrent Application
    , mkParallel
    , mkParallelD
    , newCallbackStream

    , lastN
    )
where

import Control.Concurrent (killThread, myThreadId, takeMVar, threadDelay)
import Control.Exception
       (assert, Exception, SomeException, AsyncException, fromException, mask_)
import Control.Monad (void, when, forever)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_, control)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef, IORef)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Types (SPEC(..))
import System.Mem (performMajorGC)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.IORef.Prim (Prim)
import Streamly.Internal.Data.Time.Units
       (TimeUnit64, toRelTime64, diffAbsTime64, RelTime64)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS_)
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Pipe.Types (Pipe(..), PipeState(..))
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (MicroSecond64(..), fromAbsTime, toAbsTime, AbsTime)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple3'(..))
import Streamly.Internal.Data.Stream.SVar (fromConsumer, pushToFold)

import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.IORef.Prim as Prim
import qualified Streamly.Internal.Data.Pipe.Types as Pipe
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types as MA
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Memory.Ring as RB
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Control.Monad.Catch as MC
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State
import qualified Prelude

import Prelude hiding
       ( map, mapM, mapM_, repeat, foldr, last, take, filter
       , takeWhile, drop, dropWhile, all, any, maximum, minimum, elem
       , notElem, null, head, tail, zipWith, lookup, foldr1, sequence
       , (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat
       , reverse, iterate, splitAt)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | An empty 'Stream'.
{-# INLINE_NORMAL nil #-}
nil :: Monad m => Stream m a
nil = Stream (\_ _ -> return Stop) ()

-- | An empty 'Stream' with a side effect.
{-# INLINE_NORMAL nilM #-}
nilM :: Monad m => m b -> Stream m a
nilM m = Stream (\_ _ -> m >> return Stop) ()

{-# INLINE_NORMAL consM #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing   = m >>= \x -> return $ Yield x (Just state)
    step1 gst (Just st) = do
        r <- step gst st
        return $
          case r of
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop

-- XXX implement in terms of consM?
-- cons x = consM (return x)
--
-- | Can fuse but has O(n^2) complexity.
{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream m a -> Stream m a
cons x (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing   = return $ Yield x (Just state)
    step1 gst (Just st) = do
        r <- step gst st
        return $
          case r of
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

-- Does not fuse, has the same performance as the StreamK version.
{-# INLINE_NORMAL uncons #-}
uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (UnStream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s -> return $ Just (x, Stream step s)
            Skip  s   -> go s
            Stop      -> return Nothing

------------------------------------------------------------------------------
-- Generation by unfold
------------------------------------------------------------------------------

{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream step state
  where
    {-# INLINE_LATE step #-}
    step _ st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing     -> Stop

{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

-- | Convert an 'Unfold' into a 'Stream' by supplying it a seed.
--
{-# INLINE_NORMAL unfold #-}
unfold :: Monad m => Unfold m a b -> a -> Stream m b
unfold (Unfold ustep inject) seed = Stream step Nothing
  where
    {-# INLINE_LATE step #-}
    step _ Nothing = inject seed >>= return . Skip . Just
    step _ (Just st) = do
        r <- ustep st
        return $ case r of
            Yield x s -> Yield x (Just s)
            Skip s    -> Skip (Just s)
            Stop      -> Stop

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

{-# INLINE_NORMAL repeatM #-}
repeatM :: Monad m => m a -> Stream m a
repeatM x = Stream (\_ _ -> x >>= \r -> return $ Yield r ()) ()

{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> Stream m a
repeat x = Stream (\_ _ -> return $ Yield x ()) ()

{-# INLINE_NORMAL iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
iterateM step = Stream (\_ st -> st >>= \x -> return $ Yield x (step x))

{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate step st = iterateM (return . step) (return st)

{-# INLINE_NORMAL replicateM #-}
replicateM :: forall m a. Monad m => Int -> m a -> Stream m a
replicateM n p = Stream step n
  where
    {-# INLINE_LATE step #-}
    step _ (i :: Int)
      | i <= 0    = return Stop
      | otherwise = do
          x <- p
          return $ Yield x (i - 1)

{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n x = replicateM n (return x)

-- This would not work properly for floats, therefore we put an Integral
-- constraint.
-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromStepIntegral from stride =
    from `seq` stride `seq` Stream step from
    where
        {-# INLINE_LATE step #-}
        step _ !x = return $ Yield x $! (x + stride)

-- We are assuming that "to" is constrained by the type to be within
-- max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral from to =
    takeWhile (<= to) $ enumerateFromStepIntegral from 1

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => a -> Stream m a
enumerateFromIntegral from = enumerateFromToIntegral from maxBound

data EnumState a = EnumInit | EnumYield a a a | EnumStop

{-# INLINE_NORMAL enumerateFromThenToIntegralUp #-}
enumerateFromThenToIntegralUp
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralUp from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $
            if to < next
            then if to < from
                 then Stop
                 else Yield from EnumStop
            else -- from <= next <= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x > toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegralDn #-}
enumerateFromThenToIntegralDn
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralDn from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $ if to > next
            then if to > from
                 then Stop
                 else Yield from EnumStop
            else -- from >= next >= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x < toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegral from next to
    | next >= from = enumerateFromThenToIntegralUp from next to
    | otherwise    = enumerateFromThenToIntegralDn from next to

{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenIntegral from next =
    if next > from
    then enumerateFromThenToIntegralUp from next maxBound
    else enumerateFromThenToIntegralDn from next minBound

-- For floating point numbers if the increment is less than the precision then
-- it just gets lost. Therefore we cannot always increment it correctly by just
-- repeated addition.
-- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
-- 9007199254740992 + 2     :: Double => 9.007199254740994e15

-- Instead we accumulate the increment counter and compute the increment
-- every time before adding it to the starting number.
--
-- This works for Integrals as well as floating point numbers, but
-- enumerateFromStepIntegral is faster for integrals.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> a -> Stream m a
enumerateFromStepNum from stride = Stream step 0
    where
    {-# INLINE_LATE step #-}
    step _ !i = return $ (Yield $! (from + i * stride)) $! (i + 1)

{-# INLINE_NORMAL numFrom #-}
numFrom :: (Monad m, Num a) => a -> Stream m a
numFrom from = enumerateFromStepNum from 1

{-# INLINE_NORMAL numFromThen #-}
numFromThen :: (Monad m, Num a) => a -> a -> Stream m a
numFromThen from next = enumerateFromStepNum from (next - from)

-- We cannot write a general function for Num.  The only way to write code
-- portable between the two is to use a 'Real' constraint and convert between
-- Fractional and Integral using fromRational which is horribly slow.
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum from 1

{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> a -> Stream m a
enumerateFromThenToFractional from next to =
    takeWhile predicate $ numFromThen from next
    where
    mid = (next - from) / 2
    predicate | next >= from  = (<= to + mid)
              | otherwise     = (>= to + mid)

-------------------------------------------------------------------------------
-- Generation by Conversion
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
fromIndicesM gen = Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i = do
       x <- gen i
       return $ Yield x (i + 1)

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> Stream m a
fromIndices gen = fromIndicesM (return . gen)

{-# INLINE_NORMAL generateM #-}
generateM :: Monad m => Int -> (Int -> m a) -> Stream m a
generateM n gen = n `seq` Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i | i < n     = do
                           x <- gen i
                           return $ Yield x (i + 1)
             | otherwise = return Stop

{-# INLINE generate #-}
generate :: Monad m => Int -> (Int -> a) -> Stream m a
generate n gen = generateM n (return . gen)

-- XXX we need the MonadAsync constraint because of a rewrite rule.
-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: MonadAsync m => [m a] -> Stream m a
fromListM = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (m:ms) = m >>= \x -> return $ Yield x ms
    step _ []     = return Stop

{-# INLINE toStreamD #-}
toStreamD :: (K.IsStream t, Monad m) => t m a -> Stream m a
toStreamD = fromStreamK . K.toStream

{-# INLINE_NORMAL fromPrimIORef #-}
fromPrimIORef :: (MonadIO m, Prim a) => Prim.IORef a -> Stream m a
fromPrimIORef var = Stream step ()
  where
    {-# INLINE_LATE step #-}
    step _ () = liftIO (Prim.readIORef var) >>= \x -> return $ Yield x ()

-------------------------------------------------------------------------------
-- Generation from SVar
-------------------------------------------------------------------------------

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (SVar t m a)
    | FromSVarLoop (SVar t m a) [ChildEvent a]
    | FromSVarDone (SVar t m a)

{-# INLINE_NORMAL fromSVar #-}
fromSVar :: (MonadAsync m) => SVar t m a -> Stream m a
fromSVar svar = Stream step FromSVarInit
    where

    {-# INLINE_LATE step #-}
    step _ FromSVarInit = do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- when this copy of svar gets garbage collected "ref" will get
        -- garbage collected and our GC hook will be called.
        let sv = svar{svarRef = Just ref}
        return $ Skip (FromSVarRead sv)

        where

        {-# NOINLINE hook #-}
        hook = do
            when (svarInspectMode svar) $ do
                r <- liftIO $ readIORef (svarStopTime (svarStats svar))
                when (isNothing r) $
                    printSVar svar "SVar Garbage Collected"
            cleanupSVar svar
            -- If there are any SVars referenced by this SVar a GC will prompt
            -- them to be cleaned up quickly.
            when (svarInspectMode svar) performMajorGC

    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = do
        done <- postProcess sv
        return $ Skip $ if done
                      then (FromSVarDone sv)
                      else (FromSVarRead sv)

    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        stop <- shouldStop tid
                        if stop
                        then do
                            liftIO (cleanupSVar sv)
                            return $ Skip (FromSVarDone sv)
                        else return $ Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ Skip (FromSVarLoop sv es)
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex
        where

        shouldStop tid =
            case svarStopStyle sv of
                StopNone -> return False
                StopAny -> return True
                StopBy -> do
                    sid <- liftIO $ readIORef (svarStopBy sv)
                    return $ if tid == sid then True else False

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

-------------------------------------------------------------------------------
-- Process events received by a fold consumer from a stream producer
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromProducer #-}
fromProducer :: (MonadAsync m) => SVar t m a -> Stream m a
fromProducer svar = Stream step (FromSVarRead svar)
    where

    {-# INLINE_LATE step #-}
    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = return $ Skip $ FromSVarRead sv
    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        sendStopToProducer sv
                        return $ Skip (FromSVarDone sv)
                    Just _ -> error "Bug: fromProducer: received exception"

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

    step _ FromSVarInit = undefined

-------------------------------------------------------------------------------
-- Hoisting the inner monad
-------------------------------------------------------------------------------

{-# INLINE_NORMAL hoist #-}
hoist :: Monad n => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f (Stream step state) = (Stream step' state)
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- f $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip  s   -> Skip s
            Stop      -> Stop

{-# INLINE generally #-}
generally :: Monad m => Stream Identity a -> Stream m a
generally = hoist (return . runIdentity)

{-# INLINE_NORMAL liftInner #-}
liftInner :: (Monad m, MonadTrans t, Monad (t m))
    => Stream m a -> Stream (t m) a
liftInner (Stream step state) = Stream step' state
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- lift $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip s    -> Skip s
            Stop      -> Stop

{-# INLINE_NORMAL runReaderT #-}
runReaderT :: Monad m => m s -> Stream (ReaderT s m) a -> Stream m a
runReaderT env (Stream step state) = Stream step' (state, env)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        r <- Reader.runReaderT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield x (s, return sv)
            Skip  s   -> Skip (s, return sv)
            Stop      -> Stop

{-# INLINE_NORMAL evalStateT #-}
evalStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m a
evalStateT initial (Stream step state) = Stream step' (state, initial)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        (r, sv') <- State.runStateT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield x (s, return sv')
            Skip  s   -> Skip (s, return sv')
            Stop      -> Stop

{-# INLINE_NORMAL runStateT #-}
runStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m (s, a)
runStateT initial (Stream step state) = Stream step' (state, initial)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        (r, sv') <- State.runStateT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield (sv', x) (s, return sv')
            Skip  s   -> Skip (s, return sv')
            Stop      -> Stop

------------------------------------------------------------------------------
-- Elimination by Folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m (Maybe a)
foldr1 f m = do
     r <- uncons m
     case r of
         Nothing   -> return Nothing
         Just (h, t) -> fmap Just (foldr f h t)

------------------------------------------------------------------------------
-- Left Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> Stream m a -> s m b
foldlT fstep begin (Stream step state) = go SPEC begin state
  where
    go !_ acc st = do
        r <- lift $ step defState st
        case r of
            Yield x s -> go SPEC (fstep acc x) s
            Skip s -> go SPEC acc s
            Stop   -> acc

-- Note, this is going to have horrible performance, because of the nature of
-- the stream type (i.e. direct stream vs CPS). Its only for reference, it is
-- likely be practically unusable.
{-# INLINE_NORMAL foldlS #-}
foldlS :: Monad m
    => (Stream m b -> a -> Stream m b) -> Stream m b -> Stream m a -> Stream m b
foldlS fstep begin (Stream step state) = Stream step' (Left (state, begin))
  where
    step' gst (Left (st, acc)) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (Left (s, fstep acc x))
            Skip s -> Skip (Left (s, acc))
            Stop   -> Skip (Right acc)

    step' gst (Right (Stream stp stt)) = do
        r <- stp (adaptState gst) stt
        return $ case r of
            Yield x s -> Yield x (Right (Stream stp s))
            Skip s -> Skip (Right (Stream stp s))
            Stop   -> Stop

------------------------------------------------------------------------------
-- Parses
------------------------------------------------------------------------------

-- Inlined definition. Without the inline "serially/parser/take" benchmark
-- degrades and parseMany does not fuse. Even using "inline" at the callsite
-- does not help.
{-# INLINE splitAt #-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- | Run a 'Parse' over a stream.
{-# INLINE_NORMAL parselMx' #-}
parselMx'
    :: MonadThrow m
    => (s -> a -> m (PR.Step s b))
    -> m s
    -> (s -> m b)
    -> Stream m a
    -> m b
parselMx' pstep initial extract (Stream step state) = do
    initial >>= go SPEC state []

    where

    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st buf !pst = do
        r <- step defState st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 -> go SPEC s [] pst1
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s [] src pst1
                    PR.Continue 0 pst1 -> go SPEC s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s buf1 src pst1
                    PR.Done _ b -> return b
                    PR.Error err -> throwM $ ParseError err
            Skip s -> go SPEC s buf pst
            Stop   -> extract pst

    gobuf !_ s buf [] !pst = go SPEC s buf pst
    gobuf !_ s buf (x:xs) !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                gobuf SPEC s [] xs pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s [] src pst1
            PR.Continue 0 pst1 -> gobuf SPEC s (x:buf) xs pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s buf1 src pst1
            PR.Done _ b -> return b
            PR.Error err -> throwM $ ParseError err

------------------------------------------------------------------------------
-- Repeated parsing
------------------------------------------------------------------------------

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit inpBuf st
    | ParseChunksInitLeftOver inpBuf
    | ParseChunksStream st inpBuf pst
    | ParseChunksBuf inpBuf st inpBuf pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

{-# INLINE_NORMAL parseMany #-}
parseMany
    :: MonadThrow m
    => PRD.Parser m a b
    -> Stream m a
    -> Stream m b
parseMany (PRD.Parser pstep initial extract) (Stream step state) =
    Stream stepOuter (ParseChunksInit [] state)

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit [] st) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> initial >>= return . Skip . ParseChunksBuf [x] s []
            Skip s -> return $ Skip $ ParseChunksInit [] s
            Stop   -> return Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit src st) = do
        initial >>= return . Skip . ParseChunksBuf src st []

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver _) = return Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream st buf pst) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ Skip $ ParseChunksStream s [] pst1
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf src s [] pst1
                    -- PR.Continue 0 pst1 ->
                    --     return $ Skip $ ParseChunksStream s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf src s buf1 pst1
                    -- XXX Specialize for Stop 0 common case?
                    PR.Done n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ParseChunksYield b (ParseChunksInit src s)
                    PR.Error err -> throwM $ ParseError err
            Skip s -> return $ Skip $ ParseChunksStream s buf pst
            Stop   -> do
                b <- extract pst
                let src = Prelude.reverse buf
                return $ Skip $
                    ParseChunksYield b (ParseChunksInitLeftOver src)

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf [] s buf pst) =
        return $ Skip $ ParseChunksStream s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf (x:xs) s buf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ParseChunksBuf xs s [] pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf src s [] pst1
         -- PR.Continue 0 pst1 -> return $ Skip $ ParseChunksBuf xs s (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf src s buf1 pst1
            -- XXX Specialize for Stop 0 common case?
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip $ ParseChunksYield b (ParseChunksInit src s)
            PR.Error err -> throwM $ ParseError err

    stepOuter _ (ParseChunksYield a next) = return $ Yield a next

{-# ANN type ConcatParseState Fuse #-}
data ConcatParseState x inpBuf st p =
      ConcatParseInit inpBuf st p
    | ConcatParseInitLeftOver inpBuf
    | ConcatParseStream st inpBuf p
    | ConcatParseBuf inpBuf st inpBuf p
    | ConcatParseYield x (ConcatParseState x inpBuf st p)

{-# INLINE_NORMAL parseIterate #-}
parseIterate
    :: MonadThrow m
    => (b -> PRD.Parser m a b)
    -> b
    -> Stream m a
    -> Stream m b
parseIterate func seed (Stream step state) =
    Stream stepOuter (ConcatParseInit [] state (func seed))

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, go to stream processing loop
    stepOuter _ (ConcatParseInit [] st (PRD.Parser pstep initial extract)) = do
        initial >>= \r -> return $ Skip $ ConcatParseStream st []
            (PRD.Parser pstep (return r) extract)

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ConcatParseInit src st
                    (PRD.Parser pstep initial extract)) = do
        initial >>= \r -> return $ Skip $ ConcatParseBuf src st []
            (PRD.Parser pstep (return r) extract)

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ConcatParseInitLeftOver _) = return Stop

    -- Buffer is empty process elements from the stream
    stepOuter gst (ConcatParseStream st buf
                    p@(PRD.Parser pstep initial extract)) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pst <- initial
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ Skip $ ConcatParseStream s []
                            (PRD.Parser pstep (return pst1) extract)
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ConcatParseBuf src s []
                            (PRD.Parser pstep (return pst1) extract)
                    -- PR.Continue 0 pst1 ->
                    --     return $ Skip $ ConcatParseStream s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ConcatParseBuf src s buf1
                            (PRD.Parser pstep (return pst1) extract)
                    -- XXX Specialize for Stop 0 common case?
                    PR.Done n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ConcatParseYield b (ConcatParseInit src s (func b))
                    PR.Error err -> throwM $ ParseError err
            Skip s -> return $ Skip $ ConcatParseStream s buf p
            Stop   -> do
                pst <- initial
                b <- extract pst
                let src = Prelude.reverse buf
                return $ Skip $
                    ConcatParseYield b (ConcatParseInitLeftOver src)

    -- go back to stream processing mode
    stepOuter _ (ConcatParseBuf [] s buf p) =
        return $ Skip $ ConcatParseStream s buf p

    -- buffered processing loop
    stepOuter _ (ConcatParseBuf (x:xs) s buf
                    (PRD.Parser pstep initial extract)) = do
        pst <- initial
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ConcatParseBuf xs s []
                            (PRD.Parser pstep (return pst1) extract)
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseBuf src s []
                            (PRD.Parser pstep (return pst1) extract)
         -- PR.Continue 0 pst1 -> return $ Skip $ ConcatParseBuf xs s (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseBuf src s buf1
                            (PRD.Parser pstep (return pst1) extract)
            -- XXX Specialize for Stop 0 common case?
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip $ ConcatParseYield b
                                    (ConcatParseInit src s (func b))
            PR.Error err -> throwM $ ParseError err

    stepOuter _ (ConcatParseYield a next) = return $ Yield a next

------------------------------------------------------------------------------
-- Specialized Folds
------------------------------------------------------------------------------

-- | Run a streaming composition, discard the results.
{-# INLINE_LATE drain #-}
drain :: Monad m => Stream m a -> m ()
-- drain = foldrM (\_ xs -> xs) (return ())
drain (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> go SPEC s
            Skip s    -> go SPEC s
            Stop      -> return ()

{-# INLINE_NORMAL null #-}
null :: Monad m => Stream m a -> m Bool
null m = foldrM (\_ _ -> return False) (return True) m

{-# INLINE_NORMAL head #-}
head :: Monad m => Stream m a -> m (Maybe a)
head m = foldrM (\x _ -> return (Just x)) (return Nothing) m

{-# INLINE_NORMAL headElse #-}
headElse :: Monad m => a -> Stream m a -> m a
headElse a m = foldrM (\x _ -> return x) (return a) m

-- Does not fuse, has the same performance as the StreamK version.
{-# INLINE_NORMAL tail #-}
tail :: Monad m => Stream m a -> m (Maybe (Stream m a))
tail (UnStream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield _ s -> return (Just $ Stream step s)
            Skip  s   -> go s
            Stop      -> return Nothing

-- XXX will it fuse? need custom impl?
{-# INLINE_NORMAL last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = foldl' (\_ y -> Just y) Nothing

{-# INLINE_NORMAL elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
-- elem e m = foldrM (\x xs -> if x == e then return True else xs) (return False) m
elem e (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | x == e    -> return True
              | otherwise -> go s
            Skip s -> go s
            Stop   -> return False

{-# INLINE_NORMAL notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e s = fmap not (elem e s)

{-# INLINE_NORMAL all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
-- all p m = foldrM (\x xs -> if p x then xs else return False) (return True) m
all p (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | p x       -> go s
              | otherwise -> return False
            Skip s -> go s
            Stop   -> return True

{-# INLINE_NORMAL any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
-- any p m = foldrM (\x xs -> if p x then return True else xs) (return False) m
any p (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s
              | p x       -> return True
              | otherwise -> go s
            Skip s -> go s
            Stop   -> return False

{-# INLINE_NORMAL maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
maximum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go (Just x) s
              | otherwise -> go (Just acc) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
maximumBy cmp (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go (Just acc) s
                _  -> go (Just x) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
minimum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go (Just acc) s
              | otherwise -> go (Just x) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
minimumBy cmp (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step defState st
        case r of
            Yield x s -> go (Just x) s
            Skip  s   -> go Nothing s
            Stop      -> return Nothing
    go (Just acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go (Just x) s
                _  -> go (Just acc) s
            Skip s -> go (Just acc) s
            Stop   -> return (Just acc)

{-# INLINE_NORMAL (!!) #-}
(!!) :: (Monad m) => Stream m a -> Int -> m (Maybe a)
(Stream step state) !! i = go i state
  where
    go n st = do
        r <- step defState st
        case r of
            Yield x s | n < 0 -> return Nothing
                      | n == 0 -> return $ Just x
                      | otherwise -> go (n - 1) s
            Skip s -> go n s
            Stop   -> return Nothing

{-# INLINE_NORMAL lookup #-}
lookup :: (Monad m, Eq a) => a -> Stream m (a, b) -> m (Maybe b)
lookup e m = foldrM (\(a, b) xs -> if e == a then return (Just b) else xs)
                   (return Nothing) m

{-# INLINE_NORMAL findM #-}
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
findM p m = foldrM (\x xs -> p x >>= \r -> if r then return (Just x) else xs)
                   (return Nothing) m

{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE_NORMAL findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Stream m a -> Stream m Int
findIndices p (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
      r <- step (adaptState gst) st
      return $ case r of
          Yield x s -> if p x then Yield i (s, i+1) else Skip (s, i+1)
          Skip s -> Skip (s, i)
          Stop   -> Stop

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m a -> m [a]
toListRev = foldl' (flip (:)) []

-- We can implement reverse as:
--
-- > reverse = foldlS (flip cons) nil
--
-- However, this implementation is unusable because of the horrible performance
-- of cons. So we just convert it to a list first and then stream from the
-- list.
--
-- XXX Maybe we can use an Array instead of a list here?
{-# INLINE_NORMAL reverse #-}
reverse :: Monad m => Stream m a -> Stream m a
reverse m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        xs <- toListRev m
        return $ Skip (Just xs)
    step _ (Just (x:xs)) = return $ Yield x (Just xs)
    step _ (Just []) = return Stop

-- Much faster reverse for Storables
{-# INLINE_NORMAL reverse' #-}
reverse' :: forall m a. (MonadIO m, Storable a) => Stream m a -> Stream m a
{-
-- This commented implementation copies the whole stream into one single array
-- and then streams from that array, this is 3-4x faster than the chunked code
-- that follows.  Though this could be problematic due to unbounded large
-- allocations. We need to figure out why the chunked code is slower and if we
-- can optimize the chunked code to work as fast as this one. It may be a
-- fusion issue?
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, plusPtr)
reverse' m = Stream step Nothing
    where
    {-# INLINE_LATE step #-}
    step _ Nothing = do
        arr <- A.fromStreamD m
        let p = aEnd arr `plusPtr` negate (sizeOf (undefined :: a))
        return $ Skip $ Just (aStart arr, p)

    step _ (Just (start, p)) | p < unsafeForeignPtrToPtr start = return Stop

    step _ (Just (start, p)) = do
        let !x = A.unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr start
                    return r
            next = p `plusPtr` negate (sizeOf (undefined :: a))
        return $ Yield x (Just (start, next))
-}
reverse' m =
          A.flattenArraysRev
        $ fromStreamK
        $ K.reverse
        $ toStreamK
        $ A.fromStreamDArraysOf A.defaultChunkSize m


------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

{-# INLINE_NORMAL splitSuffixBy' #-}
splitSuffixBy' :: Monad m
    => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitSuffixBy' predicate f (Stream step state) =
    Stream (stepOuter f) (Just state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                if (predicate x)
                then done acc' >>= \val -> return $ Yield val (Just s)
                else go SPEC s acc'

            Skip s    -> return $ Skip $ Just s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    acc' <- fstep acc x
                    if (predicate x)
                    then done acc' >>= \val -> return $ Yield val (Just s)
                    else go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \val -> return $ Yield val Nothing

    stepOuter _ _ Nothing = return Stop

{-# INLINE_NORMAL groupsBy #-}
groupsBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsBy cmp f (Stream step state) = Stream (stepOuter f) (Just state, Nothing)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st, Nothing) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                go SPEC x s acc'

            Skip s    -> return $ Skip $ (Just s, Nothing)
            Stop      -> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp x prev
                    then do
                        acc' <- fstep acc x
                        go SPEC prev s acc'
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC prev s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

    stepOuter (Fold fstep initial done) gst (Just st, Just prev) = do
        acc <- initial
        acc' <- fstep acc prev
        go SPEC st acc'

        where

        -- XXX code duplicated from the previous equation
        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp x prev
                    then do
                        acc' <- fstep acc x
                        go SPEC s acc'
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

    stepOuter _ _ (Nothing,_) = return Stop

{-# INLINE_NORMAL groupsRollingBy #-}
groupsRollingBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsRollingBy cmp f (Stream step state) =
    Stream (stepOuter f) (Just state, Nothing)
    where

      {-# INLINE_LATE stepOuter #-}
      stepOuter (Fold fstep initial done) gst (Just st, Nothing) = do
          res <- step (adaptState gst) st
          case res of
              Yield x s -> do
                  acc <- initial
                  acc' <- fstep acc x
                  go SPEC x s acc'

              Skip s    -> return $ Skip $ (Just s, Nothing)
              Stop      -> return Stop

        where
          go !_ prev stt !acc = do
              res <- step (adaptState gst) stt
              case res of
                  Yield x s -> do
                      if cmp prev x
                        then do
                          acc' <- fstep acc x
                          go SPEC x s acc'
                        else
                          done acc >>= \r -> return $ Yield r (Just s, Just x)
                  Skip s -> go SPEC prev s acc
                  Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

      stepOuter (Fold fstep initial done) gst (Just st, Just prev') = do
          acc <- initial
          acc' <- fstep acc prev'
          go SPEC prev' st acc'

        where
          go !_ prevv stt !acc = do
              res <- step (adaptState gst) stt
              case res of
                  Yield x s -> do
                      if cmp prevv x
                      then do
                          acc' <- fstep acc x
                          go SPEC x s acc'
                      else done acc >>= \r -> return $ Yield r (Just s, Just x)
                  Skip s -> go SPEC prevv s acc
                  Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)

      stepOuter _ _ (Nothing, _) = return Stop

{-# INLINE_NORMAL splitBy #-}
splitBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitBy predicate f (Stream step state) = Stream (step' f) (Just state)

    where

    {-# INLINE_LATE step' #-}
    step' (Fold fstep initial done) gst (Just st) = initial >>= go SPEC st

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (Just s)
                    else do
                        acc' <- fstep acc x
                        go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r Nothing

    step' _ _ Nothing = return Stop

-- XXX requires -funfolding-use-threshold=150 in lines-unlines benchmark
{-# INLINE_NORMAL splitSuffixBy #-}
splitSuffixBy :: Monad m
    => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitSuffixBy predicate f (Stream step state) = Stream (step' f) (Just state)

    where

    {-# INLINE_LATE step' #-}
    step' (Fold fstep initial done) gst (Just st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                if predicate x
                then done acc >>= \val -> return $ Yield val (Just s)
                else do
                    acc' <- fstep acc x
                    go SPEC s acc'

            Skip s    -> return $ Skip $ Just s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (Just s)
                    else do
                        acc' <- fstep acc x
                        go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r Nothing

    step' _ _ Nothing = return Stop

{-# INLINE_NORMAL wordsBy #-}
wordsBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
wordsBy predicate f (Stream step state) = Stream (stepOuter f) (Just state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                if predicate x
                then return $ Skip (Just s)
                else do
                    acc <- initial
                    acc' <- fstep acc x
                    go SPEC s acc'

            Skip s    -> return $ Skip $ Just s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (Just s)
                    else do
                        acc' <- fstep acc x
                        go SPEC s acc'
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r Nothing

    stepOuter _ _ Nothing = return Stop

-- String search algorithms:
-- http://www-igm.univ-mlv.fr/~lecroq/string/index.html

{-
-- TODO can we unify the splitting operations using a splitting configuration
-- like in the split package.
--
data SplitStyle = Infix | Suffix | Prefix deriving (Eq, Show)

data SplitOptions = SplitOptions
    { style    :: SplitStyle
    , withSep  :: Bool  -- ^ keep the separators in output
    -- , compact  :: Bool  -- ^ treat multiple consecutive separators as one
    -- , trimHead :: Bool  -- ^ drop blank at head
    -- , trimTail :: Bool  -- ^ drop blank at tail
    }
-}

data SplitOnState s a =
      GO_START
    | GO_EMPTY_PAT s
    | GO_SINGLE_PAT s a
    | GO_SHORT_PAT s
    | GO_KARP_RABIN s !(RB.Ring a) !(Ptr a)
    | GO_DONE

{-# INLINE_NORMAL splitOn #-}
splitOn
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitOn patArr (Fold fstep initial done) (Stream step state) =
    Stream stepOuter GO_START

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ GO_START =
        if patLen == 0
        then return $ Skip $ GO_EMPTY_PAT state
        else if patLen == 1
            then do
                r <- liftIO $ (A.unsafeIndexIO patArr 0)
                return $ Skip $ GO_SINGLE_PAT state r
            else if sizeOf (undefined :: a) * patLen
                    <= sizeOf (undefined :: Word)
                then return $ Skip $ GO_SHORT_PAT state
                else do
                    (rb, rhead) <- liftIO $ RB.new patLen
                    return $ Skip $ GO_KARP_RABIN state rb rhead

    stepOuter gst (GO_SINGLE_PAT stt pat) = initial >>= go SPEC stt

        where

        go !_ st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    if pat == x
                    then do
                        r <- done acc
                        return $ Yield r (GO_SINGLE_PAT s pat)
                    else fstep acc x >>= go SPEC s
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r GO_DONE

    stepOuter gst (GO_SHORT_PAT stt) = initial >>= go0 SPEC 0 (0 :: Word) stt

        where

        mask :: Word
        mask = (1 `shiftL` (elemBits * patLen)) - 1

        addToWord wrd a = (wrd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

        patWord :: Word
        patWord = mask .&. A.foldl' addToWord 0 patArr

        go0 !_ !idx wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                    if idx == maxIndex
                    then do
                        if wrd' .&. mask == patWord
                        then do
                            r <- done acc
                            return $ Yield r (GO_SHORT_PAT s)
                        else go1 SPEC wrd' s acc
                    else go0 SPEC (idx + 1) wrd' s acc
                Skip s -> go0 SPEC idx wrd s acc
                Stop -> do
                    acc' <- if idx /= 0
                            then go2 wrd idx acc
                            else return acc
                    done acc' >>= \r -> return $ Yield r GO_DONE

        {-# INLINE go1 #-}
        go1 !_ wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                        old = (mask .&. wrd) `shiftR` (elemBits * (patLen - 1))
                    acc' <- fstep acc (toEnum $ fromIntegral old)
                    if wrd' .&. mask == patWord
                    then done acc' >>= \r -> return $ Yield r (GO_SHORT_PAT s)
                    else go1 SPEC wrd' s acc'
                Skip s -> go1 SPEC wrd s acc
                Stop -> do
                    acc' <- go2 wrd patLen acc
                    done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !wrd !n !acc | n > 0 = do
            let old = (mask .&. wrd) `shiftR` (elemBits * (n - 1))
            fstep acc (toEnum $ fromIntegral old) >>= go2 wrd (n - 1)
        go2 _ _ acc = return acc

    stepOuter gst (GO_KARP_RABIN stt rb rhead) = do
        initial >>= go0 SPEC 0 rhead stt

        where

        k = 2891336453 :: Word32
        coeff = k ^ patLen
        addCksum cksum a = cksum * k + fromIntegral (fromEnum a)
        deltaCksum cksum old new =
            addCksum cksum new - coeff * fromIntegral (fromEnum old)

        -- XXX shall we use a random starting hash or 1 instead of 0?
        patHash = A.foldl' addCksum 0 patArr

        -- rh == ringHead
        go0 !_ !idx !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    rh' <- liftIO $ RB.unsafeInsert rb rh x
                    if idx == maxIndex
                    then do
                        let fold = RB.unsafeFoldRing (RB.ringBound rb)
                        let !ringHash = fold addCksum 0 rb
                        if ringHash == patHash
                        then go2 SPEC ringHash rh' s acc
                        else go1 SPEC ringHash rh' s acc
                    else go0 SPEC (idx + 1) rh' s acc
                Skip s -> go0 SPEC idx rh s acc
                Stop -> do
                    !acc' <- if idx /= 0
                             then RB.unsafeFoldRingM rh fstep acc rb
                             else return acc
                    done acc' >>= \r -> return $ Yield r GO_DONE

        -- XXX Theoretically this code can do 4 times faster if GHC generates
        -- optimal code. If we use just "(cksum' == patHash)" condition it goes
        -- 4x faster, as soon as we add the "RB.unsafeEqArray rb v" condition
        -- the generated code changes drastically and becomes 4x slower. Need
        -- to investigate what is going on with GHC.
        {-# INLINE go1 #-}
        go1 !_ !cksum !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum' = deltaCksum cksum old x
                    acc' <- fstep acc old

                    if (cksum' == patHash)
                    then do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go2 SPEC cksum' rh' s acc'
                    else do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go1 SPEC cksum' rh' s acc'
                Skip s -> go1 SPEC cksum rh s acc
                Stop -> do
                    acc' <- RB.unsafeFoldRingFullM rh fstep acc rb
                    done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !_ !cksum' !rh' s !acc' = do
            if RB.unsafeEqArray rb rh' patArr
            then do
                r <- done acc'
                return $ Yield r (GO_KARP_RABIN s rb rhead)
            else go1 SPEC cksum' rh' s acc'

    stepOuter gst (GO_EMPTY_PAT st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                done acc' >>= \r -> return $ Yield r (GO_EMPTY_PAT s)
            Skip s -> return $ Skip (GO_EMPTY_PAT s)
            Stop -> return Stop

    stepOuter _ GO_DONE = return Stop

{-# INLINE_NORMAL splitSuffixOn #-}
splitSuffixOn
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitSuffixOn withSep patArr (Fold fstep initial done)
                (Stream step state) =
    Stream stepOuter GO_START

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ GO_START =
        if patLen == 0
        then return $ Skip $ GO_EMPTY_PAT state
        else if patLen == 1
             then do
                r <- liftIO $ (A.unsafeIndexIO patArr 0)
                return $ Skip $ GO_SINGLE_PAT state r
             else if sizeOf (undefined :: a) * patLen
                    <= sizeOf (undefined :: Word)
                  then return $ Skip $ GO_SHORT_PAT state
                  else do
                    (rb, rhead) <- liftIO $ RB.new patLen
                    return $ Skip $ GO_KARP_RABIN state rb rhead

    stepOuter gst (GO_SINGLE_PAT stt pat) = do
        -- This first part is the only difference between splitOn and
        -- splitSuffixOn.
        -- If the last element is a separator do not issue a blank segment.
        res <- step (adaptState gst) stt
        case res of
            Yield x s -> do
                acc <- initial
                if pat == x
                then do
                    acc' <- if withSep then fstep acc x else return acc
                    done acc' >>= \r -> return $ Yield r (GO_SINGLE_PAT s pat)
                else fstep acc x >>= go SPEC s
            Skip s    -> return $ Skip $ (GO_SINGLE_PAT s pat)
            Stop      -> return Stop

        where

        -- This is identical for splitOn and splitSuffixOn
        go !_ st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    if pat == x
                    then do
                        acc' <- if withSep then fstep acc x else return acc
                        r <- done acc'
                        return $ Yield r (GO_SINGLE_PAT s pat)
                    else fstep acc x >>= go SPEC s
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r GO_DONE

    stepOuter gst (GO_SHORT_PAT stt) = do

        -- Call "initial" only if the stream yields an element, otherwise we
        -- may call "initial" but never yield anything. initial may produce a
        -- side effect, therefore we will end up doing and discard a side
        -- effect.

        let idx = 0
        let wrd = 0
        res <- step (adaptState gst) stt
        case res of
            Yield x s -> do
                acc <- initial
                let wrd' = addToWord wrd x
                acc' <- if withSep then fstep acc x else return acc
                if idx == maxIndex
                then do
                    if wrd' .&. mask == patWord
                    then done acc' >>= \r -> return $ Yield r (GO_SHORT_PAT s)
                    else go0 SPEC (idx + 1) wrd' s acc'
                else go0 SPEC (idx + 1) wrd' s acc'
            Skip s -> return $ Skip (GO_SHORT_PAT s)
            Stop -> return Stop

        where

        mask :: Word
        mask = (1 `shiftL` (elemBits * patLen)) - 1

        addToWord wrd a = (wrd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

        patWord :: Word
        patWord = mask .&. A.foldl' addToWord 0 patArr

        go0 !_ !idx wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                    acc' <- if withSep then fstep acc x else return acc
                    if idx == maxIndex
                    then do
                        if wrd' .&. mask == patWord
                        then do
                            r <- done acc'
                            return $ Yield r (GO_SHORT_PAT s)
                        else go1 SPEC wrd' s acc'
                    else go0 SPEC (idx + 1) wrd' s acc'
                Skip s -> go0 SPEC idx wrd s acc
                Stop -> do
                    if (idx == maxIndex) && (wrd .&. mask == patWord)
                    then return Stop
                    else do
                        acc' <- if idx /= 0 && not withSep
                                then go2 wrd idx acc
                                else return acc
                        done acc' >>= \r -> return $ Yield r GO_DONE

        {-# INLINE go1 #-}
        go1 !_ wrd st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd' = addToWord wrd x
                        old = (mask .&. wrd) `shiftR` (elemBits * (patLen - 1))
                    acc' <- if withSep
                            then fstep acc x
                            else fstep acc (toEnum $ fromIntegral old)
                    if wrd' .&. mask == patWord
                    then done acc' >>= \r -> return $ Yield r (GO_SHORT_PAT s)
                    else go1 SPEC wrd' s acc'
                Skip s -> go1 SPEC wrd s acc
                Stop ->
                    -- If the last sequence is a separator do not issue a blank
                    -- segment.
                    if wrd .&. mask == patWord
                    then return Stop
                    else do
                        acc' <- if withSep
                                then return acc
                                else go2 wrd patLen acc
                        done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !wrd !n !acc | n > 0 = do
            let old = (mask .&. wrd) `shiftR` (elemBits * (n - 1))
            fstep acc (toEnum $ fromIntegral old) >>= go2 wrd (n - 1)
        go2 _ _ acc = return acc

    stepOuter gst (GO_KARP_RABIN stt rb rhead) = do
        let idx = 0
        res <- step (adaptState gst) stt
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- if withSep then fstep acc x else return acc
                rh' <- liftIO (RB.unsafeInsert rb rhead x)
                if idx == maxIndex
                then do
                    let fold = RB.unsafeFoldRing (RB.ringBound rb)
                    let !ringHash = fold addCksum 0 rb
                    if ringHash == patHash
                    then go2 SPEC ringHash rh' s acc'
                    else go0 SPEC (idx + 1) rh' s acc'
                else go0 SPEC (idx + 1) rh' s acc'
            Skip s -> return $ Skip (GO_KARP_RABIN s rb rhead)
            Stop -> return Stop

        where

        k = 2891336453 :: Word32
        coeff = k ^ patLen
        addCksum cksum a = cksum * k + fromIntegral (fromEnum a)
        deltaCksum cksum old new =
            addCksum cksum new - coeff * fromIntegral (fromEnum old)

        -- XXX shall we use a random starting hash or 1 instead of 0?
        patHash = A.foldl' addCksum 0 patArr

        -- rh == ringHead
        go0 !_ !idx !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    acc' <- if withSep then fstep acc x else return acc
                    rh' <- liftIO (RB.unsafeInsert rb rh x)
                    if idx == maxIndex
                    then do
                        let fold = RB.unsafeFoldRing (RB.ringBound rb)
                        let !ringHash = fold addCksum 0 rb
                        if ringHash == patHash
                        then go2 SPEC ringHash rh' s acc'
                        else go1 SPEC ringHash rh' s acc'
                    else go0 SPEC (idx + 1) rh' s acc'
                Skip s -> go0 SPEC idx rh s acc
                Stop -> do
                    -- do not issue a blank segment when we end at pattern
                    if (idx == maxIndex) && RB.unsafeEqArray rb rh patArr
                    then return Stop
                    else do
                        !acc' <- if idx /= 0 && not withSep
                                 then RB.unsafeFoldRingM rh fstep acc rb
                                 else return acc
                        done acc' >>= \r -> return $ Yield r GO_DONE

        -- XXX Theoretically this code can do 4 times faster if GHC generates
        -- optimal code. If we use just "(cksum' == patHash)" condition it goes
        -- 4x faster, as soon as we add the "RB.unsafeEqArray rb v" condition
        -- the generated code changes drastically and becomes 4x slower. Need
        -- to investigate what is going on with GHC.
        {-# INLINE go1 #-}
        go1 !_ !cksum !rh st !acc = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum' = deltaCksum cksum old x
                    acc' <- if withSep
                            then fstep acc x
                            else fstep acc old

                    if (cksum' == patHash)
                    then do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go2 SPEC cksum' rh' s acc'
                    else do
                        rh' <- liftIO (RB.unsafeInsert rb rh x)
                        go1 SPEC cksum' rh' s acc'
                Skip s -> go1 SPEC cksum rh s acc
                Stop -> do
                    if RB.unsafeEqArray rb rh patArr
                    then return Stop
                    else do
                        acc' <- if withSep
                                then return acc
                                else RB.unsafeFoldRingFullM rh fstep acc rb
                        done acc' >>= \r -> return $ Yield r GO_DONE

        go2 !_ !cksum' !rh' s !acc' = do
            if RB.unsafeEqArray rb rh' patArr
            then do
                r <- done acc'
                return $ Yield r (GO_KARP_RABIN s rb rhead)
            else go1 SPEC cksum' rh' s acc'

    stepOuter gst (GO_EMPTY_PAT st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                acc' <- fstep acc x
                done acc' >>= \r -> return $ Yield r (GO_EMPTY_PAT s)
            Skip s -> return $ Skip (GO_EMPTY_PAT s)
            Stop -> return Stop

    stepOuter _ GO_DONE = return Stop

data SplitState s arr
    = SplitInitial s
    | SplitBuffering s arr
    | SplitSplitting s arr
    | SplitYielding arr (SplitState s arr)
    | SplitFinishing

-- XXX An alternative approach would be to use a partial fold (Fold m a b) to
-- split using a splitBy like combinator. The Fold would consume upto the
-- separator and return any leftover which can then be fed to the next fold.
--
-- We can revisit this once we have partial folds/parsers.
--
-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBy #-}
splitInnerBy
    :: Monad m
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBy splitter joiner (Stream step1 state1) =
    (Stream step (SplitInitial state1))

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return $ Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop -> return $ Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return $ Stop

-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBySuffix #-}
splitInnerBySuffix
    :: (Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBySuffix splitter joiner (Stream step1 state1) =
    (Stream step (SplitInitial state1))

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return $ Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop -> return $
                if buf == mempty
                then Stop
                else Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return $ Stop

------------------------------------------------------------------------------
-- Substreams
------------------------------------------------------------------------------

{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isPrefixOf (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return True

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else return False
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return False

{-# INLINE_NORMAL isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return True

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else go (sa, sb', Just x)
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return False

{-# INLINE_NORMAL stripPrefix #-}
stripPrefix
    :: (Eq a, Monad m)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix (Stream stepa ta) (Stream stepb tb) = go (ta, tb, Nothing)
  where
    go (sa, sb, Nothing) = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go (sa', sb, Just x)
            Skip sa'    -> go (sa', sb, Nothing)
            Stop        -> return $ Just (Stream stepb sb)

    go (sa, sb, Just x) = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go (sa, sb', Nothing)
                    else return Nothing
            Skip sb' -> go (sa, sb', Just x)
            Stop     -> return Nothing

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ m = drain . mapM m

-------------------------------------------------------------------------------
-- Stream transformations using Unfolds
-------------------------------------------------------------------------------

-- Define a unique structure to use in inspection testing
data ConcatMapUState o i =
      ConcatMapUOuter o
    | ConcatMapUInner o i

-- | @concatMapU unfold stream@ uses @unfold@ to map the input stream elements
-- to streams and then flattens the generated streams into a single output
-- stream.

-- This is like 'concatMap' but uses an unfold with an explicit state to
-- generate the stream instead of a 'Stream' type generator. This allows better
-- optimization via fusion.  This can be many times more efficient than
-- 'concatMap'.

{-# INLINE_NORMAL concatMapU #-}
concatMapU :: Monad m => Unfold m a b -> Stream m a -> Stream m b
concatMapU (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatMapUOuter ost)
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatMapUOuter o) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatMapUInner o' i))
            Skip o' -> return $ Skip (ConcatMapUOuter o')
            Stop -> return $ Stop

    step _ (ConcatMapUInner o i) = do
        r <- istep i
        return $ case r of
            Yield x i' -> Yield x (ConcatMapUInner o i')
            Skip i'    -> Skip (ConcatMapUInner o i')
            Stop       -> Skip (ConcatMapUOuter o)

data ConcatUnfoldInterleaveState o i =
      ConcatUnfoldInterleaveOuter o [i]
    | ConcatUnfoldInterleaveInner o [i]
    | ConcatUnfoldInterleaveInnerL [i] [i]
    | ConcatUnfoldInterleaveInnerR [i] [i]

-- XXX use arrays to store state instead of lists.
-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.

-- After a yield, switch to the next stream. Do not switch streams on Skip.
-- Yield from outer stream switches to the inner stream.
--
-- There are two choices here, (1) exhaust the outer stream first and then
-- start yielding from the inner streams, this is much simpler to implement,
-- (2) yield at least one element from an inner stream before going back to
-- outer stream and opening the next stream from it.
--
-- Ideally, we need some scheduling bias to inner streams vs outer stream.
-- Maybe we can configure the behavior.
--
{-# INLINE_NORMAL concatUnfoldInterleave #-}
concatUnfoldInterleave :: Monad m => Unfold m a b -> Stream m a -> Stream m b
concatUnfoldInterleave (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatUnfoldInterleaveOuter ost [])
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatUnfoldInterleaveOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatUnfoldInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ConcatUnfoldInterleaveOuter o' ls)
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInner _ []) = undefined
    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveInner o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] rs)

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.
--
-- This could be inefficient if the tasks are too small.
--
-- Compared to concatUnfoldInterleave this one switches streams on Skips.
--
{-# INLINE_NORMAL concatUnfoldRoundrobin #-}
concatUnfoldRoundrobin :: Monad m => Unfold m a b -> Stream m a -> Stream m b
concatUnfoldRoundrobin (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatUnfoldInterleaveOuter ost [])
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatUnfoldInterleaveOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatUnfoldInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ConcatUnfoldInterleaveInner o' ls)
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInner o []) =
            return $ Skip (ConcatUnfoldInterleaveOuter o [])

    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveOuter o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] rs)

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

data AppendState s1 s2 = AppendFirst s1 | AppendSecond s2

-- Note that this could be much faster compared to the CPS stream. However, as
-- the number of streams being composed increases this may become expensive.
-- Need to see where the breaking point is between the two.
--
{-# INLINE_NORMAL append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append (Stream step1 state1) (Stream step2 state2) =
    Stream step (AppendFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (AppendFirst st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (AppendFirst s)
            Skip s -> Skip (AppendFirst s)
            Stop -> Skip (AppendSecond state2)

    step gst (AppendSecond st) = do
        r <- step2 gst st
        return $ case r of
            Yield a s -> Yield a (AppendSecond s)
            Skip s -> Skip (AppendSecond s)
            Stop -> Stop

data InterleaveState s1 s2 = InterleaveFirst s1 s2 | InterleaveSecond s1 s2
    | InterleaveSecondOnly s2 | InterleaveFirstOnly s1

{-# INLINE_NORMAL interleave #-}
interleave :: Monad m => Stream m a -> Stream m a -> Stream m a
interleave (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step gst (InterleaveSecondOnly st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

{-# INLINE_NORMAL interleaveMin #-}
interleaveMin :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveMin (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Stop

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Stop

    step _ (InterleaveFirstOnly _) =  undefined
    step _ (InterleaveSecondOnly _) =  undefined

{-# INLINE_NORMAL interleaveSuffix #-}
interleaveSuffix :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveSuffix (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Stop

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step _ (InterleaveSecondOnly _) =  undefined

data InterleaveInfixState s1 s2 a
    = InterleaveInfixFirst s1 s2
    | InterleaveInfixSecondBuf s1 s2
    | InterleaveInfixSecondYield s1 s2 a
    | InterleaveInfixFirstYield s1 s2 a
    | InterleaveInfixFirstOnly s1

{-# INLINE_NORMAL interleaveInfix #-}
interleaveInfix :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveInfix (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveInfixFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveInfixFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveInfixSecondBuf s st2)
            Skip s -> Skip (InterleaveInfixFirst s st2)
            Stop -> Stop

    step gst (InterleaveInfixSecondBuf st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Skip (InterleaveInfixSecondYield st1 s a)
            Skip s -> Skip (InterleaveInfixSecondBuf st1 s)
            Stop -> Skip (InterleaveInfixFirstOnly st1)

    step gst (InterleaveInfixSecondYield st1 st2 x) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield x (InterleaveInfixFirstYield s st2 a)
            Skip s -> Skip (InterleaveInfixSecondYield s st2 x)
            Stop -> Stop

    step _ (InterleaveInfixFirstYield st1 st2 x) = do
        return $ Yield x (InterleaveInfixSecondBuf st1 st2)

    step gst (InterleaveInfixFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveInfixFirstOnly s)
            Skip s -> Skip (InterleaveInfixFirstOnly s)
            Stop -> Stop

{-# INLINE_NORMAL roundRobin #-}
roundRobin :: Monad m => Stream m a -> Stream m a -> Stream m a
roundRobin (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveSecond s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveFirst st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveSecondOnly st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

data ICUState s1 s2 i1 i2 =
      ICUFirst s1 s2
    | ICUSecond s1 s2
    | ICUSecondOnly s2
    | ICUFirstOnly s1
    | ICUFirstInner s1 s2 i1
    | ICUSecondInner s1 s2 i2
    | ICUFirstOnlyInner s1 i1
    | ICUSecondOnlyInner s2 i2

-- | Interleave streams (full streams, not the elements) unfolded from two
-- input streams and concat. Stop when the first stream stops. If the second
-- stream ends before the first one then first stream still keeps running alone
-- without any interleaving with the second stream.
--
--    [a1, a2, ... an]                   [b1, b2 ...]
-- => [streamA1, streamA2, ... streamAn] [streamB1, streamB2, ...]
-- => [streamA1, streamB1, streamA2...StreamAn, streamBn]
-- => [a11, a12, ...a1j, b11, b12, ...b1k, a21, a22, ...]
--
{-# INLINE_NORMAL gintercalateSuffix #-}
gintercalateSuffix
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalateSuffix
    (Unfold istep1 inject1) (Stream step1 state1)
    (Unfold istep2 inject2) (Stream step2 state2) =
    Stream step (ICUFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (ICUFirst s1 s2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICUFirstInner s s2 i))
            Skip s -> return $ Skip (ICUFirst s s2)
            Stop -> return Stop

    step gst (ICUFirstOnly s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICUFirstOnlyInner s i))
            Skip s -> return $ Skip (ICUFirstOnly s)
            Stop -> return Stop

    step _ (ICUFirstInner s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICUFirstInner s1 s2 i')
            Skip i'    -> Skip (ICUFirstInner s1 s2 i')
            Stop       -> Skip (ICUSecond s1 s2)

    step _ (ICUFirstOnlyInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICUFirstOnlyInner s1 i')
            Skip i'    -> Skip (ICUFirstOnlyInner s1 i')
            Stop       -> Skip (ICUFirstOnly s1)

    step gst (ICUSecond s1 s2) = do
        r <- step2 (adaptState gst) s2
        case r of
            Yield a s -> do
                i <- inject2 a
                i `seq` return (Skip (ICUSecondInner s1 s i))
            Skip s -> return $ Skip (ICUSecond s1 s)
            Stop -> return $ Skip (ICUFirstOnly s1)

    step _ (ICUSecondInner s1 s2 i2) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICUSecondInner s1 s2 i')
            Skip i'    -> Skip (ICUSecondInner s1 s2 i')
            Stop       -> Skip (ICUFirst s1 s2)

    step _ (ICUSecondOnly _s2) = undefined
    step _ (ICUSecondOnlyInner _s2 _i2) = undefined

data InterposeSuffixState s1 i1 =
      InterposeSuffixFirst s1
    -- | InterposeSuffixFirstYield s1 i1
    | InterposeSuffixFirstInner s1 i1
    | InterposeSuffixSecond s1

-- Note that if an unfolded layer turns out to be nil we still emit the
-- separator effect. An alternate behavior could be to emit the separator
-- effect only if at least one element has been yielded by the unfolding.
-- However, that becomes a bit complicated, so we have chosen the former
-- behvaior for now.
{-# INLINE_NORMAL interposeSuffix #-}
interposeSuffix
    :: Monad m
    => m c -> Unfold m b c -> Stream m b -> Stream m c
interposeSuffix
    action
    (Unfold istep1 inject1) (Stream step1 state1) =
    Stream step (InterposeSuffixFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (InterposeSuffixFirst s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (InterposeSuffixFirstInner s i))
                -- i `seq` return (Skip (InterposeSuffixFirstYield s i))
            Skip s -> return $ Skip (InterposeSuffixFirst s)
            Stop -> return Stop

    {-
    step _ (InterposeSuffixFirstYield s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeSuffixFirstInner s1 i')
            Skip i'    -> Skip (InterposeSuffixFirstYield s1 i')
            Stop       -> Skip (InterposeSuffixFirst s1)
    -}

    step _ (InterposeSuffixFirstInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeSuffixFirstInner s1 i')
            Skip i'    -> Skip (InterposeSuffixFirstInner s1 i')
            Stop       -> Skip (InterposeSuffixSecond s1)

    step _ (InterposeSuffixSecond s1) = do
        r <- action
        return $ Yield r (InterposeSuffixFirst s1)

data ICALState s1 s2 i1 i2 a =
      ICALFirst s1 s2
    -- | ICALFirstYield s1 s2 i1
    | ICALFirstInner s1 s2 i1
    | ICALFirstOnly s1
    | ICALFirstOnlyInner s1 i1
    | ICALSecondInject s1 s2
    | ICALFirstInject s1 s2 i2
    -- | ICALFirstBuf s1 s2 i1 i2
    | ICALSecondInner s1 s2 i1 i2
    -- -- | ICALSecondInner s1 s2 i1 i2 a
    -- -- | ICALFirstResume s1 s2 i1 i2 a

-- | Interleave streams (full streams, not the elements) unfolded from two
-- input streams and concat. Stop when the first stream stops. If the second
-- stream ends before the first one then first stream still keeps running alone
-- without any interleaving with the second stream.
--
--    [a1, a2, ... an]                   [b1, b2 ...]
-- => [streamA1, streamA2, ... streamAn] [streamB1, streamB2, ...]
-- => [streamA1, streamB1, streamA2...StreamAn, streamBn]
-- => [a11, a12, ...a1j, b11, b12, ...b1k, a21, a22, ...]
--
{-# INLINE_NORMAL gintercalate #-}
gintercalate
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalate
    (Unfold istep1 inject1) (Stream step1 state1)
    (Unfold istep2 inject2) (Stream step2 state2) =
    Stream step (ICALFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (ICALFirst s1 s2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICALFirstInner s s2 i))
                -- i `seq` return (Skip (ICALFirstYield s s2 i))
            Skip s -> return $ Skip (ICALFirst s s2)
            Stop -> return Stop

    {-
    step _ (ICALFirstYield s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICALFirstInner s1 s2 i')
            Skip i'    -> Skip (ICALFirstYield s1 s2 i')
            Stop       -> Skip (ICALFirst s1 s2)
    -}

    step _ (ICALFirstInner s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICALFirstInner s1 s2 i')
            Skip i'    -> Skip (ICALFirstInner s1 s2 i')
            Stop       -> Skip (ICALSecondInject s1 s2)

    step gst (ICALFirstOnly s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICALFirstOnlyInner s i))
            Skip s -> return $ Skip (ICALFirstOnly s)
            Stop -> return Stop

    step _ (ICALFirstOnlyInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICALFirstOnlyInner s1 i')
            Skip i'    -> Skip (ICALFirstOnlyInner s1 i')
            Stop       -> Skip (ICALFirstOnly s1)

    -- We inject the second stream even before checking if the first stream
    -- would yield any more elements. There is no clear choice whether we
    -- should do this before or after that. Doing it after may make the state
    -- machine a bit simpler though.
    step gst (ICALSecondInject s1 s2) = do
        r <- step2 (adaptState gst) s2
        case r of
            Yield a s -> do
                i <- inject2 a
                i `seq` return (Skip (ICALFirstInject s1 s i))
            Skip s -> return $ Skip (ICALSecondInject s1 s)
            Stop -> return $ Skip (ICALFirstOnly s1)

    step gst (ICALFirstInject s1 s2 i2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICALSecondInner s s2 i i2))
                -- i `seq` return (Skip (ICALFirstBuf s s2 i i2))
            Skip s -> return $ Skip (ICALFirstInject s s2 i2)
            Stop -> return Stop

    {-
    step _ (ICALFirstBuf s1 s2 i1 i2) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Skip (ICALSecondInner s1 s2 i' i2 x)
            Skip i'    -> Skip (ICALFirstBuf s1 s2 i' i2)
            Stop       -> Stop

    step _ (ICALSecondInner s1 s2 i1 i2 v) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICALSecondInner s1 s2 i1 i' v)
            Skip i'    -> Skip (ICALSecondInner s1 s2 i1 i' v)
            Stop       -> Skip (ICALFirstResume s1 s2 i1 i2 v)
    -}

    step _ (ICALSecondInner s1 s2 i1 i2) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICALSecondInner s1 s2 i1 i')
            Skip i'    -> Skip (ICALSecondInner s1 s2 i1 i')
            Stop       -> Skip (ICALFirstInner s1 s2 i1)
            -- Stop       -> Skip (ICALFirstResume s1 s2 i1 i2)

    {-
    step _ (ICALFirstResume s1 s2 i1 i2 x) = do
        return $ Yield x (ICALFirstInner s1 s2 i1 i2)
    -}

data InterposeState s1 i1 a =
      InterposeFirst s1
    -- | InterposeFirstYield s1 i1
    | InterposeFirstInner s1 i1
    | InterposeFirstInject s1
    -- | InterposeFirstBuf s1 i1
    | InterposeSecondYield s1 i1
    -- -- | InterposeSecondYield s1 i1 a
    -- -- | InterposeFirstResume s1 i1 a

-- Note that this only interposes the pure values, we may run many effects to
-- generate those values as some effects may not generate anything (Skip).
{-# INLINE_NORMAL interpose #-}
interpose :: Monad m => m c -> Unfold m b c -> Stream m b -> Stream m c
interpose
    action
    (Unfold istep1 inject1) (Stream step1 state1) =
    Stream step (InterposeFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (InterposeFirst s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (InterposeFirstInner s i))
                -- i `seq` return (Skip (InterposeFirstYield s i))
            Skip s -> return $ Skip (InterposeFirst s)
            Stop -> return Stop

    {-
    step _ (InterposeFirstYield s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeFirstInner s1 i')
            Skip i'    -> Skip (InterposeFirstYield s1 i')
            Stop       -> Skip (InterposeFirst s1)
    -}

    step _ (InterposeFirstInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeFirstInner s1 i')
            Skip i'    -> Skip (InterposeFirstInner s1 i')
            Stop       -> Skip (InterposeFirstInject s1)

    step gst (InterposeFirstInject s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                -- i `seq` return (Skip (InterposeFirstBuf s i))
                i `seq` return (Skip (InterposeSecondYield s i))
            Skip s -> return $ Skip (InterposeFirstInject s)
            Stop -> return Stop

    {-
    step _ (InterposeFirstBuf s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Skip (InterposeSecondYield s1 i' x)
            Skip i'    -> Skip (InterposeFirstBuf s1 i')
            Stop       -> Stop
    -}

    {-
    step _ (InterposeSecondYield s1 i1 v) = do
        r <- action
        return $ Yield r (InterposeFirstResume s1 i1 v)
    -}
    step _ (InterposeSecondYield s1 i1) = do
        r <- action
        return $ Yield r (InterposeFirstInner s1 i1)

    {-
    step _ (InterposeFirstResume s1 i1 v) = do
        return $ Yield v (InterposeFirstInner s1 i1)
    -}

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

data GbracketState s1 s2 v
    = GBracketInit
    | GBracketNormal s1 v
    | GBracketException s2

-- | The most general bracketing and exception combinator. All other
-- combinators can be expressed in terms of this combinator. This can also be
-- used for cases which are not covered by the standard combinators.
--
-- /Internal/
--
{-# INLINE_NORMAL gbracket_ #-}
gbracket_
    :: Monad m
    => m c                                  -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop
    -> (c -> e -> Stream m b -> Stream m b) -- ^ on exception
    -> (c -> Stream m b)                    -- ^ stream generator
    -> Stream m b
gbracket_ bef exc aft fexc fnormal =
    Stream step GBracketInit

    where

    {-# INLINE_LATE step #-}
    step _ GBracketInit = do
        r <- bef
        return $ Skip $ GBracketNormal (fnormal r) r

    step gst (GBracketNormal (UnStream step1 st) v) = do
        res <- exc $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketNormal (Stream step1 s) v)
                Skip s -> return $ Skip (GBracketNormal (Stream step1 s) v)
                Stop -> aft v >> return Stop
            Left e ->
                return $ Skip (GBracketException (fexc v e (UnStream step1 st)))
    step gst (GBracketException (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s -> return $ Yield x (GBracketException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketException (Stream step1 s))
            Stop      -> return Stop

------------------------------------------------------------------------------
-- Finalizers
------------------------------------------------------------------------------

-- | Make a finalizer from a monadic action @m a@ that can run in IO monad.
mkIOFinalizer :: MonadBaseControl IO m => m b -> m (IO ())
mkIOFinalizer f = do
    mrun <- captureMonadState
    return $
        void $ do
            _ <- runInIO mrun f
            return ()

-- | Run an IO action stored in a finalized IORef.
runIORefFinalizerGC :: IORef (Maybe (IO ())) -> IO ()
runIORefFinalizerGC ref = do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just f -> f

-- | Create an IORef holding a finalizer that is called automatically when
-- the IORef is garbage collected. The IORef can be written to with a 'Nothing'
-- value to deactivate the finalizer.
--
-- Note: The finalizer is always run with the state of the monad captured at
-- the time of calling newFinalizedIORef. To run it on garbage collection we
-- have no option but to take a snapshot of the monadic state at some point of
-- time. For normal case we could run it with the current state of the monad
-- but we want to keep both the cases consistent.
--
newFinalizedIORef :: (MonadIO m, MonadBaseControl IO m)
    => m a -> m (IORef (Maybe (IO ())))
newFinalizedIORef finalizer = do
    f <- mkIOFinalizer finalizer
    ref <- liftIO $ newIORef $ Just f
    _ <- liftIO $ mkWeakIORef ref (runIORefFinalizerGC ref)
    return ref

-- | Run the finalizer stored in an IORef and deactivate it so that it is run
-- only once. Note, the action runs with async exceptions masked.
--
runIORefFinalizer :: MonadIO m => IORef (Maybe (IO ())) -> m ()
runIORefFinalizer ref = liftIO $ do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just action -> do
            -- if an async exception comes after writing 'Nothing' then the
            -- finalizing action will never be run. We need to do this
            -- atomically wrt async exceptions.
            mask_ $ do
                writeIORef ref Nothing
                action

-- | Run an action clearing the finalizer IORef atomically wrt async
-- exceptions. The action is run with async exceptions masked.
withIORefFinalizer :: MonadBaseControl IO m
    => IORef (Maybe (IO ())) -> m a -> m a
withIORefFinalizer ref action = do
    control $ \runinio ->
        mask_ $ do
            writeIORef ref Nothing
            runinio action

------------------------------------------------------------------------------

data GbracketIOState s1 s2 v wref
    = GBracketIOInit
    | GBracketIONormal s1 v wref
    | GBracketIOException s2

-- | The most general bracketing and exception combinator. All other
-- combinators can be expressed in terms of this combinator. This can also be
-- used for cases which are not covered by the standard combinators.
--
-- | It uses a finalizer to make sure we run the finalizing action
-- when the stream is garbage collected.
--
-- /Internal/
--
{-# INLINE_NORMAL gbracket #-}
gbracket
    :: (MonadIO m, MonadBaseControl IO m)
    => m c                                  -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop or GC
    -> (c -> e -> Stream m b -> m (Stream m b)) -- ^ on exception
    -> (c -> Stream m b)                    -- ^ stream generator
    -> Stream m b
gbracket bef exc aft fexc fnormal =
    Stream step GBracketIOInit

    where

    -- If the stream is never evaluated the "aft" action will never be
    -- called. For that to occur we will need the user of this API to pass a
    -- weak pointer to us.
    {-# INLINE_LATE step #-}
    step _ GBracketIOInit = do
        -- We mask asynchronous exceptions to make the execution
        -- of 'bef' and the registration of 'aft' atomic.
        -- A similar thing is done in the resourcet package: https://git.io/JvKV3
        -- Tutorial: https://markkarpov.com/tutorial/exceptions.html
        (r, ref) <- liftBaseOp_ mask_ $ do
            r <- bef
            ref <- newFinalizedIORef (aft r)
            return (r, ref)
        return $ Skip $ GBracketIONormal (fnormal r) r ref

    step gst (GBracketIONormal (UnStream step1 st) v ref) = do
        res <- exc $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketIONormal (Stream step1 s) v ref)
                Skip s ->
                    return $ Skip (GBracketIONormal (Stream step1 s) v ref)
                Stop -> do
                    runIORefFinalizer ref
                    return Stop
            Left e -> do
                -- Clearing of finalizer and running of exception handler must
                -- be atomic wrt async exceptions. Otherwise if we have cleared
                -- the finalizer and have not run the exception handler then we
                -- may leak the resource.
                stream <- withIORefFinalizer ref (fexc v e (UnStream step1 st))
                return $ Skip (GBracketIOException stream)
    step gst (GBracketIOException (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s ->
                return $ Yield x (GBracketIOException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketIOException (Stream step1 s))
            Stop      -> return Stop

-- | Run a side effect before the stream yields its first element.
{-# INLINE_NORMAL before #-}
before :: Monad m => m b -> Stream m a -> Stream m a
before action (Stream step state) = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = action >> return (Skip (Just state))

    step' gst (Just st) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just s)
            Skip s    -> return $ Skip (Just s)
            Stop      -> return Stop

-- | Run a side effect whenever the stream stops normally.
{-# INLINE_NORMAL after_ #-}
after_ :: Monad m => m b -> Stream m a -> Stream m a
after_ action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> action >> return Stop

{-# INLINE_NORMAL after #-}
after :: (MonadIO m, MonadBaseControl IO m)
    => m b -> Stream m a -> Stream m a
after action (Stream step state) = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        ref <- newFinalizedIORef action
        return $ Skip $ Just (state, ref)
    step' gst (Just (st, ref)) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just (s, ref))
            Skip s    -> return $ Skip (Just (s, ref))
            Stop      -> do
                runIORefFinalizer ref
                return Stop

-- XXX These combinators are expensive due to the call to
-- onException/handle/try on each step. Therefore, when possible, they should
-- be called in an outer loop where we perform less iterations. For example, we
-- cannot call them on each iteration in a char stream, instead we can call
-- them when doing an IO on an array.
--
-- XXX For high performance error checks in busy streams we may need another
-- Error constructor in step.
--
-- | Run a side effect whenever the stream aborts due to an exception. The
-- exception is not caught, simply rethrown.
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action str =
    gbracket_ (return ()) MC.try return
        (\_ (e :: MC.SomeException) _ -> nilM (action >> MC.throwM e))
        (\_ -> str)

{-# INLINE_NORMAL _onException #-}
_onException :: MonadCatch m => m b -> Stream m a -> Stream m a
_onException action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st `MC.onException` action
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

-- XXX bracket is like concatMap, it generates a stream and then flattens it.
-- Like concatMap it has 10x worse performance compared to linear fused
-- compositions.
--
-- | Run the first action before the stream starts and remember its output,
-- generate a stream using the output, run the second action providing the
-- remembered value as an argument whenever the stream ends normally or due to
-- an exception.
{-# INLINE_NORMAL bracket_ #-}
bracket_ :: MonadCatch m => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket_ bef aft bet =
    gbracket_ bef MC.try aft
        (\a (e :: SomeException) _ -> nilM (aft a >> MC.throwM e)) bet

-- | Run the first action before the stream starts and remember its output,
-- generate a stream using the output, run the second action providing the
-- remembered value as an argument whenever the stream ends normally or due to
-- an exception or gets garbage collected.
--
{-# INLINE_NORMAL bracket #-}
bracket :: (MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket bef aft bet =
    gbracket bef MC.try aft
        (\a (e :: SomeException) _ -> aft a >> return (nilM (MC.throwM e))) bet

data BracketState s v = BracketInit | BracketRun s v

{-# INLINE_NORMAL _bracket #-}
_bracket :: MonadCatch m => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
_bracket bef aft bet = Stream step' BracketInit

    where

    {-# INLINE_LATE step' #-}
    step' _ BracketInit = bef >>= \x -> return (Skip (BracketRun (bet x) x))

    -- NOTE: It is important to use UnStream instead of the Stream pattern
    -- here, otherwise we get huge perf degradation, see note in concatMap.
    step' gst (BracketRun (UnStream step state) v) = do
        -- res <- step gst state `MC.onException` aft v
        res <- MC.try $ step gst state
        case res of
            Left (e :: SomeException) -> aft v >> MC.throwM e >> return Stop
            Right r -> case r of
                Yield x s -> return $ Yield x (BracketRun (Stream step s) v)
                Skip s    -> return $ Skip (BracketRun (Stream step s) v)
                Stop      -> aft v >> return Stop

-- | Run a side effect whenever the stream stops normally or aborts due to an
-- exception.
{-# INLINE finally_ #-}
finally_ :: MonadCatch m => m b -> Stream m a -> Stream m a
-- finally action xs = after action $ onException action xs
finally_ action xs = bracket_ (return ()) (\_ -> action) (const xs)

-- | Run a side effect whenever the stream stops normally or aborts due to an
-- exception or gets garbage collected.
--
{-# INLINE finally #-}
finally :: (MonadAsync m, MonadCatch m) => m b -> Stream m a -> Stream m a
finally action xs = bracket (return ()) (\_ -> action) (const xs)

-- | When evaluating a stream if an exception occurs, stream
-- evaluation aborts and the specified exception handler is run with
-- the exception and the Stream which threw the exception as argument.
--
{-# INLINE_NORMAL ghandle #-}
ghandle :: (MonadCatch m, Exception e)
    => (e -> Stream m a -> Stream m a) -> Stream m a -> Stream m a
ghandle f str =
    gbracket_ (return ()) MC.try return (\_ -> f) (\_ -> str)

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
handle f str =
    gbracket_ (return ()) MC.try return (\_ e _ -> f e) (\_ -> str)

{-# INLINE_NORMAL _handle #-}
_handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
_handle f (Stream step state) = Stream step' (Left state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Left st) = do
        res <- MC.try $ step gst st
        case res of
            Left e -> return $ Skip $ Right (f e)
            Right r -> case r of
                Yield x s -> return $ Yield x (Left s)
                Skip s    -> return $ Skip (Left s)
                Stop      -> return Stop

    step' gst (Right (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s -> return $ Yield x (Right (Stream step1 s))
            Skip s    -> return $ Skip (Right (Stream step1 s))
            Stop      -> return Stop

data RetryState m s1 s2 = WithOldStream m s1 | WithNewStream s2

{-# INLINE_NORMAL retry #-}
retry
    :: forall e m a. (Exception e, Ord e, MonadCatch m)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> Stream m a)
       -- ^ default handler for those exceptions that are not in the map
    -> Stream m a
    -> Stream m a
retry emap0 han0 (Stream step0 state0) = Stream step state

    where

    state = WithOldStream emap0 state0

    {-# INLINE_LATE step #-}
    step gst (WithOldStream emap st) = do
        -- XXX gtry? Whats the difference?
        eres <- MC.try $ step0 gst st
        case eres of
            Left e -> return $ han e emap st
            Right res ->
                return
                    $ case res of
                          Yield x st1 -> Yield x $ WithOldStream emap st1
                          Skip st1 -> Skip $ WithOldStream emap st1
                          Stop -> Stop
    step gst (WithNewStream (Stream step1 state1)) = do
        -- XXX gtry? Whats the difference?
        res <- step1 gst state1
        return
            $ case res of
                  Yield x st1 -> Yield x $ WithNewStream (Stream step1 st1)
                  Skip st1 -> Skip $ WithNewStream (Stream step1 st1)
                  Stop -> Stop

    {-# INLINE han #-}
    han e emap st =
        case Map.lookup e emap of
            Just i
                | i > 0 ->
                    let emap1 = Map.insert e (i - 1) emap
                     in Skip $ WithOldStream emap1 st
                | otherwise -> Skip $ WithNewStream $ han0 e
            Nothing -> Skip $ WithNewStream $ han0 e

-------------------------------------------------------------------------------
-- General transformation
-------------------------------------------------------------------------------

{-# INLINE_NORMAL transform #-}
transform :: Monad m => Pipe m a b -> Stream m a -> Stream m b
transform (Pipe pstep1 pstep2 pstate) (Stream step state) =
    Stream step' (Consume pstate, state)

  where

    {-# INLINE_LATE step' #-}

    step' gst (Consume pst, st) = pst `seq` do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                res <- pstep1 pst x
                case res of
                    Pipe.Yield b pst' -> return $ Yield b (pst', s)
                    Pipe.Continue pst' -> return $ Skip (pst', s)
            Skip s -> return $ Skip (Consume pst, s)
            Stop   -> return Stop

    step' _ (Produce pst, st) = pst `seq` do
        res <- pstep2 pst
        case res of
            Pipe.Yield b pst' -> return $ Yield b (pst', st)
            Pipe.Continue pst' -> return $ Skip (pst', st)

------------------------------------------------------------------------------
-- Transformation by Folding (Scans)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Prescans
------------------------------------------------------------------------------

-- XXX Is a prescan useful, discarding the last step does not sound useful?  I
-- am not sure about the utility of this function, so this is implemented but
-- not exposed. We can expose it if someone provides good reasons why this is
-- useful.
--
-- XXX We have to execute the stream one step ahead to know that we are at the
-- last step.  The vector implementation of prescan executes the last fold step
-- but does not yield the result. This means we have executed the effect but
-- discarded value. This does not sound right. In this implementation we are
-- not executing the last fold step.
{-# INLINE_NORMAL prescanlM' #-}
prescanlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
prescanlM' f mz (Stream step state) = Stream step' (state, mz)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, prev) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                acc <- prev
                return $ Yield acc (s, f acc x)
            Skip s -> return $ Skip (s, prev)
            Stop   -> return Stop

{-# INLINE prescanl' #-}
prescanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
prescanl' f z = prescanlM' (\a b -> return (f a b)) (return z)

------------------------------------------------------------------------------
-- Monolithic postscans (postscan followed by a map)
------------------------------------------------------------------------------

-- The performance of a modular postscan followed by a map seems to be
-- equivalent to this monolithic scan followed by map therefore we may not need
-- this implementation. We just have it for performance comparison and in case
-- modular version does not perform well in some situation.
--
{-# INLINE_NORMAL postscanlMx' #-}
postscanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
postscanlMx' fstep begin done (Stream step state) = do
    Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                y <- fstep old x
                v <- done y
                v `seq` y `seq` return (Yield v (s, return y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanlx' #-}
postscanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
postscanlx' fstep begin done s =
    postscanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

-- XXX do we need consM strict to evaluate the begin value?
{-# INLINE scanlMx' #-}
scanlMx' :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> Stream m b
scanlMx' fstep begin done s =
    (begin >>= \x -> x `seq` done x) `consM` postscanlMx' fstep begin done s

{-# INLINE scanlx' #-}
scanlx' :: Monad m
    => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> Stream m b
scanlx' fstep begin done s =
    scanlMx' (\b a -> return (fstep b a)) (return begin) (return . done) s

------------------------------------------------------------------------------
-- postscans
------------------------------------------------------------------------------

{-# INLINE_NORMAL postscanlM' #-}
postscanlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
postscanlM' fstep begin (Stream step state) =
    begin `seq` Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = acc `seq` do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                y `seq` return (Yield y (s, y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl' #-}
postscanl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl' f = postscanlM' (\a b -> return (f a b))

-- We can possibly have the "done" function as a Maybe to provide an option to
-- emit or not emit the accumulator when the stream stops.
--
-- TBD: use a single Yield point
--
{-# INLINE_NORMAL postscanlMAfter' #-}
postscanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
postscanlMAfter' fstep initial done (Stream step1 state1) = do
    Stream step (Just (state1, initial))

    where

    {-# INLINE_LATE step #-}
    step gst (Just (st, acc)) = do
        r <- step1 (adaptState gst) st
        case r of
            Yield x s -> do
                old <- acc
                y <- fstep old x
                y `seq` return (Yield y (Just (s, return y)))
            Skip s -> return $ Skip $ Just (s, acc)
            Stop -> acc >>= done >>= \res -> return (Yield res Nothing)
    step _ Nothing = return Stop

{-# INLINE_NORMAL postscanlM #-}
postscanlM :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
postscanlM fstep begin (Stream step state) = Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, acc) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                y <- fstep acc x
                return (Yield y (s, y))
            Skip s -> return $ Skip (s, acc)
            Stop   -> return Stop

{-# INLINE_NORMAL postscanl #-}
postscanl :: Monad m => (a -> b -> a) -> a -> Stream m b -> Stream m a
postscanl f = postscanlM (\a b -> return (f a b))

{-# INLINE_NORMAL scanlM' #-}
scanlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
scanlM' fstep begin s = begin `seq` (begin `cons` postscanlM' fstep begin s)

{-# INLINE scanlMAfter' #-}
scanlMAfter' :: Monad m
    => (b -> a -> m b) -> m b -> (b -> m b) -> Stream m a -> Stream m b
scanlMAfter' fstep initial done s =
    (initial >>= \x -> x `seq` return x) `consM`
        postscanlMAfter' fstep initial done s

{-# INLINE scanl' #-}
scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' f = scanlM' (\a b -> return (f a b))

{-# INLINE_NORMAL scanlM #-}
scanlM :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
scanlM fstep begin s = begin `cons` postscanlM fstep begin s

{-# INLINE scanl #-}
scanl :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl f = scanlM (\a b -> return (f a b))

{-# INLINE_NORMAL scanl1M #-}
scanl1M :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M fstep (Stream step state) = Stream step' (state, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Nothing) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (s, Just x)
            Skip s -> return $ Skip (s, Nothing)
            Stop   -> return Stop

    step' gst (st, Just acc) = do
        r <- step gst st
        case r of
            Yield y s -> do
                z <- fstep acc y
                return $ Yield z (s, Just z)
            Skip s -> return $ Skip (s, Just acc)
            Stop   -> return Stop

{-# INLINE scanl1 #-}
scanl1 :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1 f = scanl1M (\x y -> return (f x y))

{-# INLINE_NORMAL scanl1M' #-}
scanl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M' fstep (Stream step state) = Stream step' (state, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Nothing) = do
        r <- step gst st
        case r of
            Yield x s -> x `seq` return $ Yield x (s, Just x)
            Skip s -> return $ Skip (s, Nothing)
            Stop   -> return Stop

    step' gst (st, Just acc) = acc `seq` do
        r <- step gst st
        case r of
            Yield y s -> do
                z <- fstep acc y
                z `seq` return $ Yield z (s, Just z)
            Skip s -> return $ Skip (s, Just acc)
            Stop   -> return Stop

{-# INLINE scanl1' #-}
scanl1' :: Monad m => (a -> a -> a) -> Stream m a -> Stream m a
scanl1' f = scanl1M' (\x y -> return (f x y))

------------------------------------------------------------------------------
-- Stateful map/scan
------------------------------------------------------------------------------

data RollingMapState s a = RollingMapInit s | RollingMapGo s a

{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (a -> a -> m b) -> Stream m a -> Stream m b
rollingMapM f (Stream step1 state1) = Stream step (RollingMapInit state1)
    where
    step gst (RollingMapInit st) = do
        r <- step1 (adaptState gst) st
        return $ case r of
            Yield x s -> Skip $ RollingMapGo s x
            Skip s -> Skip $ RollingMapInit s
            Stop   -> Stop

    step gst (RollingMapGo s1 x1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield x s -> do
                !res <- f x x1
                return $ Yield res $ RollingMapGo s x
            Skip s -> return $ Skip $ RollingMapGo s x1
            Stop   -> return $ Stop

{-# INLINE rollingMap #-}
rollingMap :: Monad m => (a -> a -> b) -> Stream m a -> Stream m b
rollingMap f = rollingMapM (\x y -> return $ f x y)

------------------------------------------------------------------------------
-- Tapping/Distributing
------------------------------------------------------------------------------

{-# INLINE tap #-}
tap :: Monad m => Fold m a b -> Stream m a -> Stream m a
tap (Fold fstep initial extract) (Stream step state) = Stream step' Nothing

    where

    step' _ Nothing = do
        r <- initial
        return $ Skip (Just (r, state))

    step' gst (Just (acc, st)) = acc `seq` do
        r <- step gst st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                return $ Yield x (Just (acc', s))
            Skip s    -> return $ Skip (Just (acc, s))
            Stop      -> do
                void $ extract acc
                return $ Stop

{-# INLINE_NORMAL tapOffsetEvery #-}
tapOffsetEvery :: Monad m
    => Int -> Int -> Fold m a b -> Stream m a -> Stream m a
tapOffsetEvery offset n (Fold fstep initial extract) (Stream step state) =
    Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        r <- initial
        return $ Skip (Just (r, state, offset `mod` n))

    step' gst (Just (acc, st, count)) | count <= 0 = do
        r <- step gst st
        case r of
            Yield x s -> do
                !acc' <- fstep acc x
                return $ Yield x (Just (acc', s, n - 1))
            Skip s    -> return $ Skip (Just (acc, s, count))
            Stop      -> do
                void $ extract acc
                return $ Stop

    step' gst (Just (acc, st, count)) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (Just (acc, s, count - 1))
            Skip s    -> return $ Skip (Just (acc, s, count))
            Stop      -> do
                void $ extract acc
                return $ Stop

{-# INLINE_NORMAL pollCounts #-}
pollCounts
    :: MonadAsync m
    => (a -> Bool)
    -> (Stream m Int -> Stream m Int)
    -> Fold m Int b
    -> Stream m a
    -> Stream m a
pollCounts predicate transf fld (Stream step state) = Stream step' Nothing
  where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        -- As long as we are using an "Int" for counts lockfree reads from
        -- Var should work correctly on both 32-bit and 64-bit machines.
        -- However, an Int on a 32-bit machine may overflow quickly.
        countVar <- liftIO $ Prim.newIORef (0 :: Int)
        tid <- forkManaged
            $ void $ runFold fld
            $ transf $ fromPrimIORef countVar
        return $ Skip (Just (countVar, tid, state))

    step' gst (Just (countVar, tid, st)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                when (predicate x) $ liftIO $ Prim.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s))
            Skip s -> return $ Skip (Just (countVar, tid, s))
            Stop -> do
                liftIO $ killThread tid
                return Stop

{-# INLINE_NORMAL tapRate #-}
tapRate ::
       (MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> Stream m a
    -> Stream m a
tapRate samplingRate action (Stream step state) = Stream step' Nothing
  where
    {-# NOINLINE loop #-}
    loop countVar prev = do
        i <-
            MC.catch
                (do liftIO $ threadDelay (round $ samplingRate * 1000000)
                    i <- liftIO $ Prim.readIORef countVar
                    let !diff = i - prev
                    void $ action diff
                    return i)
                (\(e :: AsyncException) -> do
                     i <- liftIO $ Prim.readIORef countVar
                     let !diff = i - prev
                     void $ action diff
                     throwM (MC.toException e))
        loop countVar i

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        countVar <- liftIO $ Prim.newIORef 0
        tid <- fork $ loop countVar 0
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref (killThread tid)
        return $ Skip (Just (countVar, tid, state, ref))

    step' gst (Just (countVar, tid, st, ref)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                liftIO $ Prim.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s, ref))
            Skip s -> return $ Skip (Just (countVar, tid, s, ref))
            Stop -> do
                liftIO $ killThread tid
                return Stop


-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
takeWhileM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile f = takeWhileM (return . f)

{-# INLINE_NORMAL drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop n (Stream step state) = Stream step' (state, Just n)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, Just i)
      | i > 0 = do
          r <- step gst st
          return $
            case r of
              Yield _ s -> Skip (s, Just (i - 1))
              Skip s    -> Skip (s, Just i)
              Stop      -> Stop
      | otherwise = return $ Skip (st, Nothing)

    step' gst (st, Nothing) = do
      r <- step gst st
      return $
        case r of
          Yield x s -> Yield x (s, Nothing)
          Skip  s   -> Skip (s, Nothing)
          Stop      -> Stop

data DropWhileState s a
    = DropWhileDrop s
    | DropWhileYield a s
    | DropWhileNext s

{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
dropWhileM f (Stream step state) = Stream step' (DropWhileDrop state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (DropWhileDrop st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                if b
                then return $ Skip (DropWhileDrop s)
                else return $ Skip (DropWhileYield x s)
            Skip s -> return $ Skip (DropWhileDrop s)
            Stop -> return Stop

    step' gst (DropWhileNext st) =  do
        r <- step gst st
        case r of
            Yield x s -> return $ Skip (DropWhileYield x s)
            Skip s    -> return $ Skip (DropWhileNext s)
            Stop      -> return Stop

    step' _ (DropWhileYield x st) = return $ Yield x (DropWhileNext st)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile f = dropWhileM (return . f)

{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step gst st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b
                         then Yield x s
                         else Skip s
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter f = filterM (return . f)

{-# INLINE_NORMAL uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
uniq (Stream step state) = Stream step' (Nothing, state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Nothing, st) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield x (Just x, s)
            Skip  s   -> return $ Skip  (Nothing, s)
            Stop      -> return Stop
    step' gst (Just x, st)  = do
         r <- step gst st
         case r of
             Yield y s | x == y   -> return $ Skip (Just x, s)
                       | otherwise -> return $ Yield y (Just y, s)
             Skip  s   -> return $ Skip (Just x, s)
             Stop      -> return Stop

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

{-# INLINE_NORMAL sequence #-}
sequence :: Monad m => Stream m (m a) -> Stream m a
sequence (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> x >>= \a -> return (Yield a s)
             Skip s    -> return $ Skip s
             Stop      -> return Stop

------------------------------------------------------------------------------
-- Inserting
------------------------------------------------------------------------------

data LoopState x s = FirstYield s
                   | InterspersingYield s
                   | YieldAndCarry x s

{-# INLINE_NORMAL intersperseM #-}
intersperseM :: Monad m => m a -> Stream m a -> Stream m a
intersperseM m (Stream step state) = Stream step' (FirstYield state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (FirstYield st) = do
        r <- step gst st
        return $
            case r of
                Yield x s -> Skip (YieldAndCarry x s)
                Skip s -> Skip (FirstYield s)
                Stop -> Stop

    step' gst (InterspersingYield st) = do
        r <- step gst st
        case r of
            Yield x s -> do
                a <- m
                return $ Yield a (YieldAndCarry x s)
            Skip s -> return $ Skip $ InterspersingYield s
            Stop -> return Stop

    step' _ (YieldAndCarry x st) = return $ Yield x (InterspersingYield st)

{-# INLINE_NORMAL intersperseM_ #-}
intersperseM_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseM_ m (Stream step1 state1) = Stream step (Left (pure (), state1))
  where
    {-# INLINE_LATE step #-}
    step gst (Left (eff, st)) = do
        r <- step1 gst st
        case r of
            Yield x s -> eff >> return (Yield x (Right s))
            Skip s -> return $ Skip (Left (eff, s))
            Stop -> return Stop

    step _ (Right st) = return $ Skip $ Left (void m, st)

data SuffixState s a
    = SuffixElem s
    | SuffixSuffix s
    | SuffixYield a (SuffixState s a)

{-# INLINE_NORMAL intersperseSuffix #-}
intersperseSuffix :: forall m a. Monad m => m a -> Stream m a -> Stream m a
intersperseSuffix action (Stream step state) = Stream step' (SuffixElem state)
    where
    {-# INLINE_LATE step' #-}
    step' gst (SuffixElem st) = do
        r <- step gst st
        return $ case r of
            Yield x s -> Skip (SuffixYield x (SuffixSuffix s))
            Skip s -> Skip (SuffixElem s)
            Stop -> Stop

    step' _ (SuffixSuffix st) = do
        action >>= \r -> return $ Skip (SuffixYield r (SuffixElem st))

    step' _ (SuffixYield x next) = return $ Yield x next

{-# INLINE_NORMAL intersperseSuffix_ #-}
intersperseSuffix_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseSuffix_ m (Stream step1 state1) = Stream step (Left state1)
  where
    {-# INLINE_LATE step #-}
    step gst (Left st) = do
        r <- step1 gst st
        case r of
            Yield x s -> return $ Yield x (Right s)
            Skip s -> return $ Skip $ Left s
            Stop -> return Stop

    step _ (Right st) = m >> return (Skip (Left st))

data SuffixSpanState s a
    = SuffixSpanElem s Int
    | SuffixSpanSuffix s
    | SuffixSpanYield a (SuffixSpanState s a)
    | SuffixSpanLast
    | SuffixSpanStop

-- | intersperse after every n items
{-# INLINE_NORMAL intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: forall m a. Monad m
    => Int -> m a -> Stream m a -> Stream m a
intersperseSuffixBySpan n action (Stream step state) =
    Stream step' (SuffixSpanElem state n)
    where
    {-# INLINE_LATE step' #-}
    step' gst (SuffixSpanElem st i) | i > 0 = do
        r <- step gst st
        return $ case r of
            Yield x s -> Skip (SuffixSpanYield x (SuffixSpanElem s (i - 1)))
            Skip s -> Skip (SuffixSpanElem s i)
            Stop -> if i == n then Stop else Skip SuffixSpanLast
    step' _ (SuffixSpanElem st _) = return $ Skip (SuffixSpanSuffix st)

    step' _ (SuffixSpanSuffix st) = do
        action >>= \r -> return $ Skip (SuffixSpanYield r (SuffixSpanElem st n))

    step' _ (SuffixSpanLast) = do
        action >>= \r -> return $ Skip (SuffixSpanYield r SuffixSpanStop)

    step' _ (SuffixSpanYield x next) = return $ Yield x next

    step' _ (SuffixSpanStop) = return Stop

{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = intersperseM (return a)

{-# INLINE_NORMAL insertBy #-}
insertBy :: Monad m => (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp a (Stream step state) = Stream step' (state, False, Nothing)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, False, _) = do
        r <- step gst st
        case r of
            Yield x s -> case cmp a x of
                GT -> return $ Yield x (s, False, Nothing)
                _  -> return $ Yield a (s, True, Just x)
            Skip s -> return $ Skip (s, False, Nothing)
            Stop   -> return $ Yield a (st, True, Nothing)

    step' _ (_, True, Nothing) = return Stop

    step' gst (st, True, Just prev) = do
        r <- step gst st
        case r of
            Yield x s -> return $ Yield prev (s, True, Just x)
            Skip s    -> return $ Skip (s, True, Just prev)
            Stop      -> return $ Yield prev (st, True, Nothing)

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

{-# INLINE_NORMAL deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
deleteBy eq x (Stream step state) = Stream step' (state, False)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, False) = do
        r <- step gst st
        case r of
            Yield y s -> return $
                if eq x y then Skip (s, True) else Yield y (s, False)
            Skip s -> return $ Skip (s, False)
            Stop   -> return Stop

    step' gst (st, True) = do
        r <- step gst st
        case r of
            Yield y s -> return $ Yield y (s, True)
            Skip s -> return $ Skip (s, True)
            Stop   -> return Stop

------------------------------------------------------------------------------
-- Transformation by Map and Filter
------------------------------------------------------------------------------

-- XXX Will this always fuse properly?
{-# INLINE_NORMAL mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = fmap fromJust . filter isJust . map f

{-# INLINE_NORMAL mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

{-# INLINE_NORMAL indexed #-}
indexed :: Monad m => Stream m a -> Stream m (Int, a)
indexed (Stream step state) = Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> return $ Yield (i, x) (s, i+1)
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

{-# INLINE_NORMAL indexedR #-}
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int, a)
indexedR m (Stream step state) = Stream step' (state, m)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) = i `seq` do
         r <- step (adaptState gst) st
         case r of
             Yield x s -> let i' = i - 1
                          in return $ Yield (i, x) (s, i')
             Skip    s -> return $ Skip (s, i)
             Stop      -> return Stop

{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f (Stream stepa ta) (Stream stepb tb) = Stream step (ta, tb, Nothing)
  where
    {-# INLINE_LATE step #-}
    step gst (sa, sb, Nothing) = do
        r <- stepa (adaptState gst) sa
        return $
          case r of
            Yield x sa' -> Skip (sa', sb, Just x)
            Skip sa'    -> Skip (sa', sb, Nothing)
            Stop        -> Stop

    step gst (sa, sb, Just x) = do
        r <- stepb (adaptState gst) sb
        case r of
            Yield y sb' -> do
                z <- f x y
                return $ Yield z (sa, sb', Nothing)
            Skip sb' -> return $ Skip (sa, sb', Just x)
            Stop     -> return Stop

#if __GLASGOW_HASKELL__ >= 801
{-# RULES "zipWithM xs xs"
    forall f xs. zipWithM @Identity f xs xs = mapM (\x -> f x x) xs #-}
#endif

{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f = zipWithM (\a b -> return (f a b))

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

{-# INLINE_NORMAL mergeByM #-}
mergeByM
    :: (Monad m)
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM cmp (Stream stepa ta) (Stream stepb tb) =
    Stream step (Just ta, Just tb, Nothing, Nothing)
  where
    {-# INLINE_LATE step #-}

    -- one of the values is missing, and the corresponding stream is running
    step gst (Just sa, sb, Nothing, b) = do
        r <- stepa gst sa
        return $ case r of
            Yield a sa' -> Skip (Just sa', sb, Just a, b)
            Skip sa'    -> Skip (Just sa', sb, Nothing, b)
            Stop        -> Skip (Nothing, sb, Nothing, b)

    step gst (sa, Just sb, a, Nothing) = do
        r <- stepb gst sb
        return $ case r of
            Yield b sb' -> Skip (sa, Just sb', a, Just b)
            Skip sb'    -> Skip (sa, Just sb', a, Nothing)
            Stop        -> Skip (sa, Nothing, a, Nothing)

    -- both the values are available
    step _ (sa, sb, Just a, Just b) = do
        res <- cmp a b
        return $ case res of
            GT -> Yield b (sa, sb, Just a, Nothing)
            _  -> Yield a (sa, sb, Nothing, Just b)

    -- one of the values is missing, corresponding stream is done
    step _ (Nothing, sb, Nothing, Just b) =
            return $ Yield b (Nothing, sb, Nothing, Nothing)

    step _ (sa, Nothing, Just a, Nothing) =
            return $ Yield a (sa, Nothing, Nothing, Nothing)

    step _ (Nothing, Nothing, Nothing, Nothing) = return Stop

{-# INLINE mergeBy #-}
mergeBy
    :: (Monad m)
    => (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeBy cmp = mergeByM (\a b -> return $ cmp a b)

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE_NORMAL the #-}
the :: (Eq a, Monad m) => Stream m a -> m (Maybe a)
the (Stream step state) = go state
  where
    go st = do
        r <- step defState st
        case r of
            Yield x s -> go' x s
            Skip s    -> go s
            Stop      -> return Nothing
    go' n st = do
        r <- step defState st
        case r of
            Yield x s | x == n -> go' n s
                      | otherwise -> return Nothing
            Skip s -> go' n s
            Stop   -> return (Just n)

{-# INLINE runFold #-}
runFold :: (Monad m) => Fold m a b -> Stream m a -> m b
runFold (Fold step begin done) = foldlMx' step begin done

-------------------------------------------------------------------------------
-- Concurrent application and fold
-------------------------------------------------------------------------------

-- XXX These functions should be moved to Stream/Parallel.hs
--
-- Using StreamD the worker stream producing code can fuse with the code to
-- queue output to the SVar giving some perf boost.
--
-- Note that StreamD can only be used in limited situations, specifically, we
-- cannot implement joinStreamVarPar using this.
--
-- XXX make sure that the SVar passed is a Parallel style SVar.

-- | Fold the supplied stream to the SVar asynchronously using Parallel
-- concurrency style.
-- {-# INLINE_NORMAL toSVarParallel #-}
{-# INLINE toSVarParallel #-}
toSVarParallel :: MonadAsync m
    => State t m a -> SVar t m a -> Stream m a -> m ()
toSVarParallel st sv xs =
    if svarInspectMode sv
    then forkWithDiag
    else do
        tid <-
                case getYieldLimit st of
                    Nothing -> doFork (work Nothing)
                                      (svarMrun sv)
                                      (handleChildException sv)
                    Just _  -> doFork (workLim Nothing)
                                      (svarMrun sv)
                                      (handleChildException sv)
        modifyThread sv tid

    where

    {-# NOINLINE work #-}
    work info = (runFold (FL.toParallelSVar sv info) xs)

    {-# NOINLINE workLim #-}
    workLim info = runFold (FL.toParallelSVarLimited sv info) xs

    {-# NOINLINE forkWithDiag #-}
    forkWithDiag = do
        -- We do not use workerCount in case of ParallelVar but still there is
        -- no harm in maintaining it correctly.
        liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
        recordMaxWorkers sv
        -- This allocation matters when significant number of workers are being
        -- sent. We allocate it only when needed. The overhead increases by 4x.
        winfo <-
            case yieldRateInfo sv of
                Nothing -> return Nothing
                Just _ -> liftIO $ do
                    cntRef <- newIORef 0
                    t <- getTime Monotonic
                    lat <- newIORef (0, t)
                    return $ Just WorkerInfo
                        { workerYieldMax = 0
                        , workerYieldCount = cntRef
                        , workerLatencyStart = lat
                        }
        tid <-
            case getYieldLimit st of
                Nothing -> doFork (work winfo)
                                  (svarMrun sv)
                                  (handleChildException sv)
                Just _  -> doFork (workLim winfo)
                                  (svarMrun sv)
                                  (handleChildException sv)
        modifyThread sv tid

{-# INLINE_NORMAL mkParallelD #-}
mkParallelD :: MonadAsync m => Stream m a -> Stream m a
mkParallelD m = Stream step Nothing
    where

    step gst Nothing = do
        sv <- newParallelVar StopNone gst
        toSVarParallel gst sv m
        -- XXX use unfold instead?
        return $ Skip $ Just $ fromSVar sv

    step gst (Just (UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (Just $ Stream step1 s)
            Skip s    -> Skip (Just $ Stream step1 s)
            Stop      -> Stop

-- Compare with mkAsync. mkAsync uses an Async style SVar whereas this uses a
-- parallel style SVar for evaluation. Currently, parallel style cannot use
-- rate control whereas Async style can use rate control. In async style SVar
-- the worker thread terminates when the buffer is full whereas in Parallel
-- style it blocks.
--
-- | Make the stream producer and consumer run concurrently by introducing a
-- buffer between them. The producer thread evaluates the input stream until
-- the buffer fills, it blocks if the buffer is full until there is space in
-- the buffer. The consumer consumes the stream lazily from the buffer.
--
-- /Internal/
--
{-# INLINE_NORMAL mkParallel #-}
mkParallel :: (K.IsStream t, MonadAsync m) => t m a -> t m a
mkParallel = fromStreamD . mkParallelD . toStreamD

-- Note: we can use another API with two callbacks stop and yield if we want
-- the callback to be able to indicate end of stream.
--
-- | Generates a callback and a stream pair. The callback returned is used to
-- queue values to the stream.  The stream is infinite, there is no way for the
-- callback to indicate that it is done now.
--
-- /Internal/
--
{-# INLINE_NORMAL newCallbackStream #-}
newCallbackStream :: (K.IsStream t, MonadAsync m) => m ((a -> m ()), t m a)
newCallbackStream = do
    sv <- newParallelVar StopNone defState

    -- XXX Add our own thread-id to the SVar as we can not know the callback's
    -- thread-id and the callback is not run in a managed worker. We need to
    -- handle this better.
    liftIO myThreadId >>= modifyThread sv

    let callback a = liftIO $ void $ send sv (ChildYield a)
    -- XXX we can return an SVar and then the consumer can unfold from the
    -- SVar?
    return (callback, fromStreamD (fromSVar sv))

-------------------------------------------------------------------------------
-- Concurrent tap
-------------------------------------------------------------------------------

-- | Create an SVar with a fold consumer that will fold any elements sent to it
-- using the supplied fold function.
{-# INLINE newFoldSVar #-}
newFoldSVar :: MonadAsync m => State t m a -> Fold m a b -> m (SVar t m a)
newFoldSVar stt f = do
    -- Buffer size for the SVar is derived from the current state
    sv <- newParallelVar StopAny (adaptState stt)
    -- Add the producer thread-id to the SVar.
    liftIO myThreadId >>= modifyThread sv
    void $ doFork (work sv) (svarMrun sv) (handleFoldException sv)
    return sv

    where

    {-# NOINLINE work #-}
    work sv = void $ runFold f $ fromProducer sv

data TapState sv st = TapInit | Tapping sv st | TapDone st

{-# INLINE_NORMAL tapAsync #-}
tapAsync :: MonadAsync m => Fold m a b -> Stream m a -> Stream m a
tapAsync f (Stream step1 state1) = Stream step TapInit
    where

    drainFold svr = do
            -- In general, a Stop event would come equipped with the result
            -- of the fold. It is not used here but it would be useful in
            -- applicative and distribute.
            done <- fromConsumer svr
            when (not done) $ do
                liftIO $ withDiagMVar svr "teeToSVar: waiting to drain"
                       $ takeMVar (outputDoorBellFromConsumer svr)
                drainFold svr

    stopFold svr = do
            liftIO $ sendStop svr Nothing
            -- drain/wait until a stop event arrives from the fold.
            drainFold svr

    {-# INLINE_LATE step #-}
    step gst TapInit = do
        sv <- newFoldSVar gst f
        return $ Skip (Tapping sv state1)

    step gst (Tapping sv st) = do
        r <- step1 gst st
        case r of
            Yield a s ->  do
                done <- pushToFold sv a
                if done
                then do
                    -- XXX we do not need to wait synchronously here
                    stopFold sv
                    return $ Yield a (TapDone s)
                else return $ Yield a (Tapping sv s)
            Skip s -> return $ Skip (Tapping sv s)
            Stop -> do
                stopFold sv
                return $ Stop

    step gst (TapDone st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (TapDone s)
            Skip s    -> Skip (TapDone s)
            Stop      -> Stop

-- XXX Exported from Array again as this fold is specific to Array
-- | Take last 'n' elements from the stream and discard the rest.
{-# INLINE lastN #-}
lastN :: (Storable a, MonadIO m) => Int -> Fold m a (Array a)
lastN n
    | n <= 0 = fmap (const mempty) FL.drain
    | otherwise = A.unsafeFreeze <$> Fold step initial done
  where
    step (Tuple3' rb rh i) a = do
        rh1 <- liftIO $ RB.unsafeInsert rb rh a
        return $ Tuple3' rb rh1 (i + 1)
    initial = fmap (\(a, b) -> Tuple3' a b (0 :: Int)) $ liftIO $ RB.new n
    done (Tuple3' rb rh i) = do
        arr <- liftIO $ MA.newArray n
        foldFunc i rh snoc' arr rb
    snoc' b a = liftIO $ MA.unsafeSnoc b a
    foldFunc i
        | i < n = RB.unsafeFoldRingM
        | otherwise = RB.unsafeFoldRingFullM

------------------------------------------------------------------------------
-- Time related
------------------------------------------------------------------------------

-- XXX using getTime in the loop can be pretty expensive especially for
-- computations where iterations are lightweight. We have the following
-- options:
--
-- 1) Run a timeout thread updating a flag asynchronously and check that
-- flag here, that way we can have a cheap termination check.
--
-- 2) Use COARSE clock to get time with lower resolution but more efficiently.
--
-- 3) Use rdtscp/rdtsc to get time directly from the processor, compute the
-- termination value of rdtsc in the beginning and then in each iteration just
-- get rdtsc and check if we should terminate.
--
data TakeByTime st s
    = TakeByTimeInit st
    | TakeByTimeCheck st s
    | TakeByTimeYield st s

{-# INLINE_NORMAL takeByTime #-}
takeByTime :: (MonadIO m, TimeUnit64 t) => t -> Stream m a -> Stream m a
takeByTime duration (Stream step1 state1) = Stream step (TakeByTimeInit state1)
    where

    lim = toRelTime64 duration

    {-# INLINE_LATE step #-}
    step _ (TakeByTimeInit _) | lim == 0 = return Stop
    step _ (TakeByTimeInit st) = do
        t0 <- liftIO $ getTime Monotonic
        return $ Skip (TakeByTimeYield st t0)
    step _ (TakeByTimeCheck st t0) = do
        t <- liftIO $ getTime Monotonic
        return $
            if diffAbsTime64 t t0 > lim
            then Stop
            else Skip (TakeByTimeYield st t0)
    step gst (TakeByTimeYield st t0) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Yield x (TakeByTimeCheck s t0)
             Skip s -> Skip (TakeByTimeCheck s t0)
             Stop -> Stop

data DropByTime st s x
    = DropByTimeInit st
    | DropByTimeGen st s
    | DropByTimeCheck st s x
    | DropByTimeYield st

{-# INLINE_NORMAL dropByTime #-}
dropByTime :: (MonadIO m, TimeUnit64 t) => t -> Stream m a -> Stream m a
dropByTime duration (Stream step1 state1) = Stream step (DropByTimeInit state1)
    where

    lim = toRelTime64 duration

    {-# INLINE_LATE step #-}
    step _ (DropByTimeInit st) = do
        t0 <- liftIO $ getTime Monotonic
        return $ Skip (DropByTimeGen st t0)
    step gst (DropByTimeGen st t0) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Skip (DropByTimeCheck s t0 x)
             Skip s -> Skip (DropByTimeGen s t0)
             Stop -> Stop
    step _ (DropByTimeCheck st t0 x) = do
        t <- liftIO $ getTime Monotonic
        if diffAbsTime64 t t0 <= lim
        then return $ Skip $ DropByTimeGen st t0
        else return $ Yield x $ DropByTimeYield st
    step gst (DropByTimeYield st) = do
        r <- step1 gst st
        return $ case r of
             Yield x s -> Yield x (DropByTimeYield s)
             Skip s -> Skip (DropByTimeYield s)
             Stop -> Stop

{-# INLINE updateTimeVar #-}
updateTimeVar :: Prim.IORef Int64 -> IO ()
updateTimeVar timeVar = do
    MicroSecond64 t <- fromAbsTime <$> getTime Monotonic
    Prim.modifyIORef' timeVar (const t)

{-# INLINE updateWithDelay #-}
updateWithDelay :: RealFrac a => a -> Prim.IORef Int64 -> IO ()
updateWithDelay precision timeVar = do
    threadDelay (delayTime precision)
    updateTimeVar timeVar

    where

    -- Keep the minimum at least a millisecond to avoid high CPU usage
    {-# INLINE delayTime #-}
    delayTime g
        | g' >= fromIntegral (maxBound :: Int) = maxBound
        | g' < 1000 = 1000
        | otherwise =  round g'

        where

        g' = g * 10 ^ (6 :: Int)

-- XXX we should move this to stream generation section of this file. Also, the
-- take/drop combinators above should be moved to filtering section.

{-# INLINE_NORMAL times #-}
times :: MonadAsync m => Double -> Stream m (AbsTime, RelTime64)
times g = Stream step Nothing

    where

    {-# INLINE_LATE step #-}
    step _ Nothing = do
        -- XXX note that this is safe only on a 64-bit machine. On a 32-bit
        -- machine a 64-bit 'Var' cannot be read consistently without a lock
        -- while another thread is writing to it.
        timeVar <- liftIO $ Prim.newIORef (0 :: Int64)
        liftIO $ updateTimeVar timeVar
        tid <- forkManaged $ liftIO $ forever (updateWithDelay g timeVar)
        a <- liftIO $ Prim.readIORef timeVar
        return $ Skip $ Just (timeVar, tid, a)

    step _ s@(Just (timeVar, _, t0)) = do
        a <- liftIO $ Prim.readIORef timeVar
        -- XXX we can perhaps use an AbsTime64 using a 64 bit Int for
        -- efficiency.  or maybe we can use a representation using Double for
        -- floating precision time
        return $ Yield (toAbsTime (MicroSecond64 t0),
            (toRelTime64 (MicroSecond64 (a - t0)))) s
