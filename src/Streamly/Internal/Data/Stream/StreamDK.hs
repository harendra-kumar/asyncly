#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamDK
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Stream.StreamDK
    (
    -- * Stream Type

      Stream
    , Step (..)

    -- * Construction
    , nil
    , cons
    , consM
    , unfoldr
    , unfoldrM
    , replicateM

    -- * Folding
    , uncons
    , foldrS

    -- * Specific Folds
    , drain
    )
where

import Streamly.Internal.Data.Stream.StreamDK.Type (Stream(..), Step(..))

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

nil :: Monad m => Stream m a
nil = Stream $ return Stop

{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream m a -> Stream m a
cons x xs = Stream $ return $ Yield x xs

consM :: Monad m => m a -> Stream m a -> Stream m a
consM eff xs = Stream $ eff >>= \x -> return $ Yield x xs

unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream (step' state)
  where
    step' st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x (Stream (step' s))
            Nothing     -> Stop
{-
unfoldrM next s0 = buildM $ \yld stp ->
    let go s = do
            r <- next s
            case r of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0
-}

{-# INLINE unfoldr #-}
unfoldr :: Monad m => (b -> Maybe (a, b)) -> b -> Stream m a
unfoldr next s0 = build $ \yld stp ->
    let go s =
            case next s of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0

replicateM :: Monad m => Int -> a -> Stream m a
replicateM n x = Stream (step n)
    where
    step i = return $
        if i <= 0
        then Stop
        else Yield x (Stream (step (i - 1)))

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream step) = do
    r <- step
    return $ case r of
        Yield x xs -> Just (x, xs)
        Stop -> Nothing

-- | Lazy right associative fold to a stream.
{-# INLINE_NORMAL foldrS #-}
foldrS :: Monad m
       => (a -> Stream m b -> Stream m b)
       -> Stream m b
       -> Stream m a
       -> Stream m b
foldrS f streamb = go
    where
    go (Stream stepa) = Stream $ do
        r <- stepa
        case r of
            Yield x xs -> let Stream step = f x (go xs) in step
            Stop -> let Stream step = streamb in step

{-# INLINE_LATE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM fstep acc = go
    where
    go (Stream step) = do
        r <- step
        case r of
            Yield x xs -> fstep x (go xs)
            Stop -> acc

{-# INLINE_NORMAL build #-}
build :: Monad m
    => forall a. (forall b. (a -> b -> b) -> b -> b) -> Stream m a
build g = g cons nil

{-# RULES
"foldrM/build"  forall k z (g :: forall b. (a -> b -> b) -> b -> b).
                foldrM k z (build g) = g k z #-}

{-
-- To fuse foldrM with unfoldrM we need the type m1 to be polymorphic such that
-- it is either Monad m or Stream m.  So that we can use cons/nil as well as
-- monadic construction function as its arguments.
--
{-# INLINE_NORMAL buildM #-}
buildM :: Monad m
    => forall a. (forall b. (a -> m1 b -> m1 b) -> m1 b -> m1 b) -> Stream m a
buildM g = g cons nil
-}

-------------------------------------------------------------------------------
-- Specific folds
-------------------------------------------------------------------------------

{-# INLINE drain #-}
drain :: Monad m => Stream m a -> m ()
drain = foldrM (\_ xs -> xs) (return ())
{-
drain (Stream step) = do
    r <- step
    case r of
        Yield _ next -> drain next
        Stop      -> return ()
        -}
