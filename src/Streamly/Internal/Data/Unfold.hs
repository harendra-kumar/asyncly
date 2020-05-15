{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streams forcing a closed control flow loop can be categorized under
-- two types, unfolds and folds, both of these are duals of each other.
--
-- Unfold streams are really generators of a sequence of elements, we can also
-- call them pull style streams. These are lazy producers of streams. On each
-- evaluation the producer generates the next element.  A consumer can
-- therefore pull elements from the stream whenever it wants to.  A stream
-- consumer can multiplex pull streams by pulling elements from the chosen
-- streams, therefore, pull streams allow merging or multiplexing.  On the
-- other hand, with this representation we cannot split or demultiplex a
-- stream.  So really these are stream sources that can be generated from a
-- seed and can be merged or zipped into a single stream.
--
-- The dual of Unfolds are Folds. Folds can also be called as push style
-- streams or reducers. These are strict consumers of streams. We keep pushing
-- elements to a fold and we can extract the result at any point. A driver can
-- choose which fold to push to and can also push the same element to multiple
-- folds. Therefore, folds allow splitting or demultiplexing a stream. On the
-- other hand, we cannot merge streams using this representation. So really
-- these are stream consumers that reduce the stream to a single value, these
-- consumers can be composed such that a stream can be split over multiple
-- consumers.
--
-- Performance:
--
-- Composing a tree or graph of computations with unfolds can be much more
-- efficient compared to composing with the Monad instance.  The reason is that
-- unfolds allow the compiler to statically know the state and optimize it
-- using stream fusion whereas it is not possible with the monad bind because
-- the state is determined dynamically.

-- Open control flow style streams can also have two representations. StreamK
-- is a producer style representation. We can also have a consumer style
-- representation. We can use that for composable folds in StreamK
-- representation.
--
module Streamly.Internal.Data.Unfold
    (
    -- * Unfold Type
      Unfold

    -- * Operations on Input
    , lmap
    , lmapM
    , supply
    , supplyFirst
    , supplySecond
    , discardFirst
    , discardSecond
    , swap
    -- coapply
    -- comonad

    -- * Operations on Output
    , fold
    -- pipe

    -- * Unfolds
    , fromStream
    , fromStream1
    , fromStream2
    , nilM
    , consM
    , effect
    , singletonM
    , singleton
    , identity
    , const
    , replicateM
    , repeatM
    , fromList
    , fromListM
    , enumerateFromStepIntegral
    , enumerateFromToIntegral
    , enumerateFromIntegral

    -- * Transformations
    , map
    , mapM
    , mapMWithInput

    -- * Filtering
    , takeWhileM
    , takeWhile
    , take
    , filter
    , filterM

    -- * Zipping
    , zipWithM
    , zipWith
    , teeZipWith

    -- * Nesting
    , concat
    , concatMapM
    , outerProduct

    -- * Exceptions
    , gbracket
    , gbracketIO
    , before
    , after
    , afterIO
    , onException
    , finally
    , finallyIO
    , bracket
    , bracketIO
    , handle
    )
where

import Control.Exception (Exception, mask_)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Data.Void (Void)
import GHC.Types (SPEC(..))
import Prelude
       hiding (concat, map, mapM, takeWhile, take, filter, const, zipWith)

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..), Step(..))
#if __GLASGOW_HASKELL__ < 800
import Streamly.Internal.Data.Stream.StreamD.Type (pattern Stream)
#endif
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.IORef
    (newFinalizedIORef, runIORefFinalizer, clearIORefFinalizer)
import Streamly.Internal.Data.SVar (defState, MonadAsync)

import qualified Prelude
import qualified Control.Monad.Catch as MC
import qualified Data.Tuple as Tuple
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

-------------------------------------------------------------------------------
-- Input operations
-------------------------------------------------------------------------------

-- | Map a function on the input argument of the 'Unfold'.
--
-- @
-- lmap f = concat (singleton f)
-- @
--
-- /Internal/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (uinject . f)

-- | Map an action on the input argument of the 'Unfold'.
--
-- @
-- lmapM f = concat (singletonM f)
-- @
--
-- /Internal/
{-# INLINE_NORMAL lmapM #-}
lmapM :: Monad m => (a -> m c) -> Unfold m c b -> Unfold m a b
lmapM f (Unfold ustep uinject) = Unfold ustep (\x -> f x >>= uinject)

-- XXX change the signature to the following?
-- supply :: a -> Unfold m a b -> Unfold m Void b
--
-- | Supply the seed to an unfold closing the input end of the unfold.
--
-- /Internal/
--
{-# INLINE_NORMAL supply #-}
supply :: Unfold m a b -> a -> Unfold m Void b
supply unf a = lmap (Prelude.const a) unf

-- XXX change the signature to the following?
-- supplyFirst :: a -> Unfold m (a, b) c -> Unfold m b c
--
-- | Supply the first component of the tuple to an unfold that accepts a tuple
-- as a seed resulting in a fold that accepts the second component of the tuple
-- as a seed.
--
-- /Internal/
--
{-# INLINE_NORMAL supplyFirst #-}
supplyFirst :: Unfold m (a, b) c -> a -> Unfold m b c
supplyFirst unf a = lmap (a, ) unf

-- XXX change the signature to the following?
-- supplySecond :: b -> Unfold m (a, b) c -> Unfold m a c
--
-- | Supply the second component of the tuple to an unfold that accepts a tuple
-- as a seed resulting in a fold that accepts the first component of the tuple
-- as a seed.
--
-- /Internal/
--
{-# INLINE_NORMAL supplySecond #-}
supplySecond :: Unfold m (a, b) c -> b -> Unfold m a c
supplySecond unf b = lmap (, b) unf

-- | Convert an 'Unfold' into an unfold accepting a tuple as an argument,
-- using the argument of the original fold as the second element of tuple and
-- discarding the first element of the tuple.
--
-- /Internal/
--
{-# INLINE_NORMAL discardFirst #-}
discardFirst :: Unfold m a b -> Unfold m (c, a) b
discardFirst = lmap snd

-- | Convert an 'Unfold' into an unfold accepting a tuple as an argument,
-- using the argument of the original fold as the first element of tuple and
-- discarding the second element of the tuple.
--
-- /Internal/
--
{-# INLINE_NORMAL discardSecond #-}
discardSecond :: Unfold m a b -> Unfold m (a, c) b
discardSecond = lmap fst

-- | Convert an 'Unfold' that accepts a tuple as an argument into an unfold
-- that accepts a tuple with elements swapped.
--
-- /Internal/
--
{-# INLINE_NORMAL swap #-}
swap :: Unfold m (a, c) b -> Unfold m (c, a) b
swap = lmap Tuple.swap

-------------------------------------------------------------------------------
-- Output operations
-------------------------------------------------------------------------------

-- | Compose an 'Unfold' and a 'Fold'. Given an @Unfold m a b@ and a
-- @Fold m b c@, returns a monadic action @a -> m c@ representing the
-- application of the fold on the unfolded stream.
--
-- /Internal/
--
{-# INLINE_NORMAL fold #-}
fold :: Monad m => Unfold m a b -> Fold m b c -> a -> m c
fold (Unfold ustep inject) (Fold fstep initial extract) a =
    initial >>= \x -> inject a >>= go SPEC x
  where
    -- XXX !acc?
    {-# INLINE_LATE go #-}
    go !_ acc st = acc `seq` do
        r <- ustep st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Skip s -> go SPEC acc s
            Stop   -> extract acc

{-# INLINE_NORMAL map #-}
map :: Monad m => (b -> c) -> Unfold m a b -> Unfold m a c
map f (Unfold ustep uinject) = Unfold step uinject
    where
    {-# INLINE_LATE step #-}
    step st = do
        r <- ustep st
        return $ case r of
            Yield x s -> Yield (f x) s
            Skip s    -> Skip s
            Stop      -> Stop

{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (b -> m c) -> Unfold m a b -> Unfold m a c
mapM f (Unfold ustep uinject) = Unfold step uinject
    where
    {-# INLINE_LATE step #-}
    step st = do
        r <- ustep st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return $ Stop

{-# INLINE_NORMAL mapMWithInput #-}
mapMWithInput :: Monad m => (a -> b -> m c) -> Unfold m a b -> Unfold m a c
mapMWithInput f (Unfold ustep uinject) = Unfold step inject
    where
    inject a = do
        r <- uinject a
        return (a, r)

    {-# INLINE_LATE step #-}
    step (inp, st) = do
        r <- ustep st
        case r of
            Yield x s -> f inp x >>= \a -> return $ Yield a (inp, s)
            Skip s    -> return $ Skip (inp, s)
            Stop      -> return $ Stop

-------------------------------------------------------------------------------
-- Convert streams into unfolds
-------------------------------------------------------------------------------

{-# INLINE_LATE streamStep #-}
streamStep :: Monad m => Stream m a -> m (Step (Stream m a) a)
streamStep (Stream step1 state) = do
    r <- step1 defState state
    return $ case r of
        Yield x s -> Yield x (Stream step1 s)
        Skip s    -> Skip (Stream step1 s)
        Stop      -> Stop

-- | Convert a stream into an 'Unfold'. Note that a stream converted to an
-- 'Unfold' may not be as efficient as an 'Unfold' in some situations.
--
-- /Internal/
fromStream :: (K.IsStream t, Monad m) => t m b -> Unfold m Void b
fromStream str = Unfold streamStep (\_ -> return $ D.toStreamD str)

-- | Convert a single argument stream generator function into an
-- 'Unfold'. Note that a stream converted to an 'Unfold' may not be as
-- efficient as an 'Unfold' in some situations.
--
-- /Internal/
fromStream1 :: (K.IsStream t, Monad m) => (a -> t m b) -> Unfold m a b
fromStream1 f = Unfold streamStep (return . D.toStreamD . f)

-- | Convert a two argument stream generator function into an 'Unfold'. Note
-- that a stream converted to an 'Unfold' may not be as efficient as an
-- 'Unfold' in some situations.
--
-- /Internal/
fromStream2 :: (K.IsStream t, Monad m)
    => (a -> b -> t m c) -> Unfold m (a, b) c
fromStream2 f = Unfold streamStep (\(a, b) -> return $ D.toStreamD $ f a b)

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Lift a monadic function into an unfold generating a nil stream with a side
-- effect.
--
{-# INLINE nilM #-}
nilM :: Monad m => (a -> m c) -> Unfold m a b
nilM f = Unfold step return
    where
    {-# INLINE_LATE step #-}
    step x = f x >> return Stop

-- | Prepend a monadic single element generator function to an 'Unfold'.
--
-- /Internal/
{-# INLINE_NORMAL consM #-}
consM :: Monad m => (a -> m b) -> Unfold m a b -> Unfold m a b
consM action unf = Unfold step inject

    where

    inject = return . Left

    {-# INLINE_LATE step #-}
    step (Left a) = do
        action a >>= \r -> return $ Yield r (Right (D.unfold unf a))
    step (Right (UnStream step1 st)) = do
        res <- step1 defState st
        case res of
            Yield x s -> return $ Yield x (Right (Stream step1 s))
            Skip s -> return $ Skip (Right (Stream step1 s))
            Stop -> return Stop

-- | Lift a monadic effect into an unfold generating a singleton stream.
--
{-# INLINE effect #-}
effect :: Monad m => m b -> Unfold m Void b
effect eff = Unfold step inject
    where
    inject _ = return True
    {-# INLINE_LATE step #-}
    step True = eff >>= \r -> return $ Yield r False
    step False = return Stop

-- XXX change it to yieldM or change yieldM in Prelude to singletonM
--
-- | Lift a monadic function into an unfold generating a singleton stream.
--
{-# INLINE singletonM #-}
singletonM :: Monad m => (a -> m b) -> Unfold m a b
singletonM f = Unfold step inject
    where
    inject x = return $ Just x
    {-# INLINE_LATE step #-}
    step (Just x) = f x >>= \r -> return $ Yield r Nothing
    step Nothing = return Stop

-- | Lift a pure function into an unfold generating a singleton stream.
--
{-# INLINE singleton #-}
singleton :: Monad m => (a -> b) -> Unfold m a b
singleton f = singletonM $ return . f

-- | Identity unfold. Generates a singleton stream with the seed as the only
-- element in the stream.
--
-- > identity = singletonM return
--
{-# INLINE identity #-}
identity :: Monad m => Unfold m a a
identity = singletonM return

const :: Monad m => m b -> Unfold m a b
const m = Unfold step inject
    where
    inject _ = return ()
    step () = m >>= \r -> return $ Yield r ()

-- | Generates a stream replicating the seed @n@ times.
--
{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Unfold m a a
replicateM n = Unfold step inject
    where
    inject x = return (x, n)
    {-# INLINE_LATE step #-}
    step (x, i) = return $
        if i <= 0
        then Stop
        else Yield x (x, (i - 1))

-- | Generates an infinite stream repeating the seed.
--
{-# INLINE repeatM #-}
repeatM :: Monad m => Unfold m a a
repeatM = Unfold step return
    where
    {-# INLINE_LATE step #-}
    step x = return $ Yield x x

-- | Convert a list of pure values to a 'Stream'
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Unfold m [a] a
fromList = Unfold step inject
  where
    inject x = return x
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step []     = return Stop

-- | Convert a list of monadic values to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: Monad m => Unfold m [m a] a
fromListM = Unfold step inject
  where
    inject x = return x
    {-# INLINE_LATE step #-}
    step (x:xs) = x >>= \r -> return $ Yield r xs
    step []     = return Stop

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL take #-}
take :: Monad m => Int -> Unfold m a b -> Unfold m a b
take n (Unfold step1 inject1) = Unfold step inject
  where
    inject x = do
        s <- inject1 x
        return (s, 0)
    {-# INLINE_LATE step #-}
    step (st, i) | i < n = do
        r <- step1 st
        return $ case r of
            Yield x s -> Yield x (s, i + 1)
            Skip s -> Skip (s, i)
            Stop   -> Stop
    step (_, _) = return Stop

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
takeWhileM f (Unfold step1 inject1) = Unfold step inject1
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
takeWhile f = takeWhileM (return . f)

{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
filterM f (Unfold step1 inject1) = Unfold step inject1
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Skip s
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE filter #-}
filter :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
filter f = filterM (return . f)

-------------------------------------------------------------------------------
-- Enumeration
-------------------------------------------------------------------------------

-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => Unfold m (a, a) a
enumerateFromStepIntegral = Unfold step inject
    where
    inject (from, stride) = from `seq` stride `seq` return (from, stride)
    {-# INLINE_LATE step #-}
    step !(x, stride) = return $ Yield x $! (x + stride, stride)

-- We are assuming that "to" is constrained by the type to be within
-- max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> Unfold m a a
enumerateFromToIntegral to =
    takeWhile (<= to) $ supplySecond enumerateFromStepIntegral 1

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => Unfold m a a
enumerateFromIntegral = enumerateFromToIntegral maxBound

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (a -> b -> m c) -> Unfold m x a -> Unfold m y b -> Unfold m (x, y) c
zipWithM f (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject (x, y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return (s1, s2, Nothing)

    {-# INLINE_LATE step #-}
    step (s1, s2, Nothing) = do
        r <- step1 s1
        return $
          case r of
            Yield x s -> Skip (s, s2, Just x)
            Skip s    -> Skip (s, s2, Nothing)
            Stop      -> Stop

    step (s1, s2, Just x) = do
        r <- step2 s2
        case r of
            Yield y s -> do
                z <- f x y
                return $ Yield z (s1, s, Nothing)
            Skip s -> return $ Skip (s1, s, Just x)
            Stop   -> return Stop

-- | Divide the input into two unfolds and then zip the outputs to a single
-- stream.
--
-- @
--   S.mapM_ print
-- $ S.concatUnfold (UF.zipWith (,) UF.identity (UF.singleton sqrt))
-- $ S.map (\x -> (x,x))
-- $ S.fromList [1..10]
-- @
--
-- /Internal/
--
{-# INLINE zipWith #-}
zipWith :: Monad m
    => (a -> b -> c) -> Unfold m x a -> Unfold m y b -> Unfold m (x, y) c
zipWith f = zipWithM (\a b -> return (f a b))

-- | Distribute the input to two unfolds and then zip the outputs to a single
-- stream.
--
-- @
-- S.mapM_ print $ S.concatUnfold (UF.teeZipWith (,) UF.identity (UF.singleton sqrt)) $ S.fromList [1..10]
-- @
--
-- /Internal/
--
{-# INLINE_NORMAL teeZipWith #-}
teeZipWith :: Monad m
    => (a -> b -> c) -> Unfold m x a -> Unfold m x b -> Unfold m x c
teeZipWith f unf1 unf2 = lmap (\x -> (x,x)) $ zipWith f unf1 unf2

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# ANN type ConcatState Fuse #-}
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Internal/
--
{-# INLINE_NORMAL concat #-}
concat :: Monad m => Unfold m a b -> Unfold m b c -> Unfold m a c
concat (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject
    where
    inject x = do
        s <- inject1 x
        return $ ConcatOuter s

    {-# INLINE_LATE step #-}
    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (ConcatInner s innerSt)
            Skip s    -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (ConcatInner ost s)
            Skip s    -> Skip (ConcatInner ost s)
            Stop      -> Skip (ConcatOuter ost)

data OuterProductState s1 s2 sy x y =
    OuterProductOuter s1 y | OuterProductInner s1 sy s2 x

-- | Create an outer product (vector product or cartesian product) of the
-- output streams of two unfolds.
--
{-# INLINE_NORMAL outerProduct #-}
outerProduct :: Monad m
    => Unfold m a b -> Unfold m c d -> Unfold m (a, c) (b, d)
outerProduct (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject
    where
    inject (x, y) = do
        s1 <- inject1 x
        return $ OuterProductOuter s1 y

    {-# INLINE_LATE step #-}
    step (OuterProductOuter st1 sy) = do
        r <- step1 st1
        case r of
            Yield x s -> do
                s2 <- inject2 sy
                return $ Skip (OuterProductInner s sy s2 x)
            Skip s    -> return $ Skip (OuterProductOuter s sy)
            Stop      -> return Stop

    step (OuterProductInner ost sy ist x) = do
        r <- step2 ist
        return $ case r of
            Yield y s -> Yield (x, y) (OuterProductInner ost sy s x)
            Skip s    -> Skip (OuterProductInner ost sy s x)
            Stop      -> Skip (OuterProductOuter ost sy)

-- XXX This can be used to implement a Monad instance for "Unfold m ()".

data ConcatMapState s1 s2 = ConcatMapOuter s1 | ConcatMapInner s1 s2

-- | Map an unfold generating action to each element of an unfold and
-- flattern the results into a single stream.
--
{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m
    => (b -> m (Unfold m () c)) -> Unfold m a b -> Unfold m a c
concatMapM f (Unfold step1 inject1) = Unfold step inject
    where
    inject x = do
        s <- inject1 x
        return $ ConcatMapOuter s

    {-# INLINE_LATE step #-}
    step (ConcatMapOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                Unfold step2 inject2 <- f x
                innerSt <- inject2 ()
                return $ Skip (ConcatMapInner s (Stream (\_ ss -> step2 ss)
                                                        innerSt))
            Skip s    -> return $ Skip (ConcatMapOuter s)
            Stop      -> return Stop

    step (ConcatMapInner ost (UnStream istep ist)) = do
        r <- istep defState ist
        return $ case r of
            Yield x s -> Yield x (ConcatMapInner ost (Stream istep s))
            Skip s    -> Skip (ConcatMapInner ost (Stream istep s))
            Stop      -> Skip (ConcatMapOuter ost)

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | The most general bracketing and exception combinator. All other
-- combinators can be expressed in terms of this combinator. This can also be
-- used for cases which are not covered by the standard combinators.
--
-- /Internal/
--
{-# INLINE_NORMAL gbracket #-}
gbracket
    :: Monad m
    => (a -> m c)                           -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop
    -> Unfold m (c, e) b                    -- ^ on exception
    -> Unfold m c b                         -- ^ unfold to run
    -> Unfold m a b
gbracket bef exc aft (Unfold estep einject) (Unfold step1 inject1) =
    Unfold step inject

    where

    inject x = do
        r <- bef x
        s <- inject1 r
        return $ Right (s, r)

    {-# INLINE_LATE step #-}
    step (Right (st, v)) = do
        res <- exc $ step1 st
        case res of
            Right r -> case r of
                Yield x s -> return $ Yield x (Right (s, v))
                Skip s    -> return $ Skip (Right (s, v))
                Stop      -> aft v >> return Stop
            Left e -> do
                r <- einject (v, e)
                return $ Skip (Left r)
    step (Left st) = do
        res <- estep st
        case res of
            Yield x s -> return $ Yield x (Left s)
            Skip s    -> return $ Skip (Left s)
            Stop      -> return Stop

-- | The most general bracketing and exception combinator. All other
-- combinators can be expressed in terms of this combinator. This can also be
-- used for cases which are not covered by the standard combinators.
--
-- /Internal/
--
{-# INLINE_NORMAL gbracketIO #-}
gbracketIO
    :: (MonadIO m, MonadBaseControl IO m)
    => (a -> m c)                           -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop, or GC
    -> Unfold m (c, e) b                    -- ^ on exception
    -> Unfold m c b                         -- ^ unfold to run
    -> Unfold m a b
gbracketIO bef exc aft (Unfold estep einject) (Unfold step1 inject1) =
    Unfold step inject

    where

    inject x = do
        -- Mask asynchronous exceptions to make the execution of 'bef' and
        -- the registration of 'aft' atomic. See comment in 'D.gbracketIO'.
        (r, ref) <- liftBaseOp_ mask_ $ do
            r <- bef x
            ref <- newFinalizedIORef (aft r)
            return (r, ref)
        s <- inject1 r
        return $ Right (s, r, ref)

    {-# INLINE_LATE step #-}
    step (Right (st, v, ref)) = do
        res <- exc $ step1 st
        case res of
            Right r -> case r of
                Yield x s -> return $ Yield x (Right (s, v, ref))
                Skip s    -> return $ Skip (Right (s, v, ref))
                Stop      -> do
                    runIORefFinalizer ref
                    return Stop
            Left e -> do
                clearIORefFinalizer ref
                r <- einject (v, e)
                return $ Skip (Left r)
    step (Left st) = do
        res <- estep st
        case res of
            Yield x s -> return $ Yield x (Left s)
            Skip s    -> return $ Skip (Left s)
            Stop      -> return Stop

-- The custom implementation of "before" is slightly faster (5-7%) than
-- "_before".  This is just to document and make sure that we can always use
-- gbracket to implement before. The same applies to other combinators as well.
--
{-# INLINE_NORMAL _before #-}
_before :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
_before action unf = gbracket (\x -> action x >> return x) (fmap Right)
                             (\_ -> return ()) undefined unf

-- | Run a side effect before the unfold yields its first element.
--
-- /Internal/
{-# INLINE_NORMAL before #-}
before :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
before action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        _ <- action x
        st <- inject1 x
        return st

    {-# INLINE_LATE step #-}
    step st = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

{-# INLINE_NORMAL _after #-}
_after :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
_after aft = gbracket return (fmap Right) aft undefined

-- | Run a side effect whenever the unfold stops normally.
--
-- Prefer afterIO over this as the @after@ action in this combinator is not
-- executed if the unfold is partially evaluated lazily and then garbage
-- collected.
--
-- /Internal/
{-# INLINE_NORMAL after #-}
after :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
after action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> action v >> return Stop

-- | Run a side effect whenever the unfold stops normally
-- or is garbage collected after a partial lazy evaluation.
--
-- /Internal/
{-# INLINE_NORMAL afterIO #-}
afterIO :: (MonadIO m, MonadBaseControl IO m)
    => (a -> m c) -> Unfold m a b -> Unfold m a b
afterIO action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        ref <- newFinalizedIORef (action x)
        return (s, ref)

    {-# INLINE_LATE step #-}
    step (st, ref) = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x (s, ref)
            Skip s    -> return $ Skip (s, ref)
            Stop      -> do
                runIORefFinalizer ref
                return Stop

{-# INLINE_NORMAL _onException #-}
_onException :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
_onException action unf =
    gbracket return MC.try
        (\_ -> return ())
        (nilM (\(a, (e :: MC.SomeException)) -> action a >> MC.throwM e)) unf

-- | Run a side effect whenever the unfold aborts due to an exception.
--
-- /Internal/
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
onException action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` action v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> return Stop

{-# INLINE_NORMAL _finally #-}
_finally :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
_finally action unf =
    gbracket return MC.try action
        (nilM (\(a, (e :: MC.SomeException)) -> action a >> MC.throwM e)) unf

-- | Run a side effect whenever the unfold stops normally or aborts due to an
-- exception.
--
-- Prefer finallyIO over this as the @after@ action in this combinator is not
-- executed if the unfold is partially evaluated lazily and then garbage
-- collected.
--
-- /Internal/
{-# INLINE_NORMAL finally #-}
finally :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
finally action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` action v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> action v >> return Stop

-- | Run a side effect whenever the unfold stops normally, aborts due to an
-- exception or if it is garbage collected after a partial lazy evaluation.
--
-- /Internal/
{-# INLINE_NORMAL finallyIO #-}
finallyIO :: (MonadAsync m, MonadCatch m)
    => (a -> m c) -> Unfold m a b -> Unfold m a b
finallyIO action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        ref <- newFinalizedIORef (action x)
        return (s, ref)

    {-# INLINE_LATE step #-}
    step (st, ref) = do
        res <- step1 st `MC.onException` runIORefFinalizer ref
        case res of
            Yield x s -> return $ Yield x (s, ref)
            Skip s    -> return $ Skip (s, ref)
            Stop      -> do
                runIORefFinalizer ref
                return Stop

{-# INLINE_NORMAL _bracket #-}
_bracket :: MonadCatch m
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
_bracket bef aft unf =
    gbracket bef MC.try aft (nilM (\(a, (e :: MC.SomeException)) -> aft a >>
    MC.throwM e)) unf

-- | @bracket before after between@ runs the @before@ action and then unfolds
-- its output using the @between@ unfold. When the @between@ unfold is done or
-- if an exception occurs then the @after@ action is run with the output of
-- @before@ as argument.
--
-- Prefer bracketIO over this as the @after@ action in this combinator is not
-- executed if the unfold is partially evaluated lazily and then garbage
-- collected.
--
-- /Internal/
{-# INLINE_NORMAL bracket #-}
bracket :: MonadCatch m
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
bracket bef aft (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        r <- bef x
        s <- inject1 r
        return (s, r)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` aft v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> aft v >> return Stop

-- | @bracket before after between@ runs the @before@ action and then unfolds
-- its output using the @between@ unfold. When the @between@ unfold is done or
-- if an exception occurs then the @after@ action is run with the output of
-- @before@ as argument. The after action is also executed if the unfold is
-- paritally evaluated and then garbage collected.
--
-- /Internal/
{-# INLINE_NORMAL bracketIO #-}
bracketIO :: (MonadAsync m, MonadCatch m)
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
bracketIO bef aft (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        -- Mask asynchronous exceptions to make the execution of 'bef' and
        -- the registration of 'aft' atomic. See comment in 'D.gbracketIO'.
        (r, ref) <- liftBaseOp_ mask_ $ do
            r <- bef x
            ref <- newFinalizedIORef (aft r)
            return (r, ref)
        s <- inject1 r
        return (s, ref)

    {-# INLINE_LATE step #-}
    step (st, ref) = do
        res <- step1 st `MC.onException` runIORefFinalizer ref
        case res of
            Yield x s -> return $ Yield x (s, ref)
            Skip s    -> return $ Skip (s, ref)
            Stop      -> do
                runIORefFinalizer ref
                return Stop

-- | When unfolding if an exception occurs, unfold the exception using the
-- exception unfold supplied as the first argument to 'handle'.
--
-- /Internal/
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => Unfold m e b -> Unfold m a b -> Unfold m a b
handle exc unf =
    gbracket return MC.try (\_ -> return ()) (discardFirst exc) unf
