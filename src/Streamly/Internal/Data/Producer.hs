-- |
-- Module      : Streamly.Internal.Data.Producer
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Producer' is an 'Unfold' with an 'extract' function added to extract
-- the state. It is more powerful but less general than an Unfold.
--
-- A 'Producer' represents steps of a loop generating a sequence of elements.
-- While unfolds are closed representation of imperative loops with some opaque
-- internal state, producers are open loops with the state being accessible to
-- the user.
--
-- Unlike an unfold, which runs a loop till completion, a producer can be
-- stopped in the middle, its state can be extracted, examined, changed, and
-- then it can be resumed later from the stopped state.
--
-- A producer can be used in places where a CPS stream would otherwise be
-- needed, because the state of the loop can be passed around. However, it can
-- be much more efficient than CPS because it allows stream fusion and
-- unecessary function calls can be avoided.

module Streamly.Internal.Data.Producer
    ( Producer (..)

    -- * Converting
    , simplify

    -- * Producers
    , nil
    , nilM
    , unfoldrM
    , fromStreamD
    , fromList

    -- * Combinators
    , NestedLoop (..)
    , concat
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..))
import Streamly.Internal.Data.SVar (defState)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Step as D

import Streamly.Internal.Data.Producer.Type
import Prelude hiding (concat)

-- XXX We should write unfolds as producers where possible and define
-- unfolds using "simplify".
--
-------------------------------------------------------------------------------
-- Converting to unfolds
-------------------------------------------------------------------------------

-- | Simplify a producer to an unfold.
--
-- /Pre-release/
{-# INLINE simplify #-}
simplify :: Monad m => Producer m a b -> Unfold m a b
simplify (Producer step inject _) = Unfold step1 inject

    where

    step1 st = do
        res <- step st
        return $ case res of
            Yield x s -> D.Yield x s
            Skip s -> D.Skip s
            Stop _ -> D.Stop

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Convert a StreamD stream into a producer.
--
-- /Pre-release/
{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: Monad m => Producer m (Stream m a) a
fromStreamD = Producer step return (return . Just)

    where

    {-# INLINE_LATE step #-}
    step (UnStream step1 state1) = do
        r <- step1 defState state1
        return $ case r of
            D.Yield x s -> Yield x (Stream step1 s)
            D.Skip s    -> Skip (Stream step1 s)
            D.Stop      -> Stop Nothing
