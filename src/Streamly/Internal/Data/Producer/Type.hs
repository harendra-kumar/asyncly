-- |
-- Module      : Streamly.Internal.Data.Producer.Type
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Internal.Data.Producer" for introduction.
--

module Streamly.Internal.Data.Producer.Type
    (
    -- * Type
      Step (..)
    , Producer (..)

    -- * Producers
    , nil
    , nilM
    , unfoldrM
    , fromList

    -- * Mapping
    , translate
    , lmap

    -- * Nesting
    , cross
    , NestedLoop (..)
    , concat
    )
where

#include "inline.hs"

import Fusion.Plugin.Types (Fuse(..))
import Prelude hiding (concat, map, const)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Producer as Producer
-- >>> import qualified Streamly.Prelude as Stream

------------------------------------------------------------------------------
-- Step
------------------------------------------------------------------------------

{-# ANN type Step Fuse #-}
data Step s a b = Yield b s | Skip s | Stop (Maybe a)

instance Functor (Step s a) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ (Skip s) = Skip s
    fmap _ (Stop s) = Stop s

------------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------------

-- We need the return type of extract to be "Maybe a" so that we can represent
-- a "used up" state for types that can not inherently represent that. For
-- example, we may not be able to implement "const" correctly without it.
--
-- Or we can use the input type to be Maybe instead. Check duality with the
-- type of "initial" in folds.
--
-- | A @Producer m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- /Pre-release/

data Producer m a b =
    -- | @Producer step inject extract@
    forall s. Producer (s -> m (Step s a b)) (a -> m s) (s -> m (Maybe a))

------------------------------------------------------------------------------
-- Producers
------------------------------------------------------------------------------

{-# INLINE nilM #-}
nilM :: Monad m => (a -> m c) -> Producer m a b
nilM f = Producer step return (return . Just)

    where

    {-# INLINE_LATE step #-}
    step x = f x >> return (Stop Nothing)

{-# INLINE nil #-}
nil :: Monad m => Producer m a b
nil = nilM (\_ -> return ())

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> Producer m a b
unfoldrM next = Producer step return (return . Just)

    where

    {-# INLINE_LATE step #-}
    step st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing -> Stop Nothing

-- | Convert a list of pure values to a 'Stream'
--
-- /Pre-release/
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Producer m [a] a
fromList = Producer step return (return . Just)

    where

    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step [] = return $ Stop Nothing

------------------------------------------------------------------------------
-- Mapping
------------------------------------------------------------------------------

-- | Interconvert the producer between two interconvertible input types.
--
-- /Pre-release/
{-# INLINE_NORMAL translate #-}
translate :: Monad m =>
    (a -> c) -> (c -> a) -> Producer m c b -> Producer m a b
translate f g (Producer step inject extract) =
    Producer step1 (inject . f)  (fmap (fmap g) . extract)

    where

    step1 st = do
        r <- step st
        return $ case r of
            Yield b s -> Yield b s
            Skip s -> Skip s
            Stop c -> Stop $ fmap g c

-- | Map the producer input to another value of the same type.
--
-- /Pre-release/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> a) -> Producer m a b -> Producer m a b
lmap f (Producer step inject extract) = Producer step (inject . f) extract

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

-- | Map a function on the output of the producer (the type @b@).
--
-- /Pre-release/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Producer m a b -> Producer m a c
map f (Producer ustep uinject uextract) = Producer step uinject uextract

    where

    {-# INLINE_LATE step #-}
    step st = fmap (fmap f) (ustep st)

-- | Maps a function on the output of the producer (the type @b@).
instance Functor m => Functor (Producer m a) where
    {-# INLINE fmap #-}
    fmap = map

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE const #-}
const :: Applicative m => m b -> Producer m a b
const m = Producer step inject extract

    where

    inject a = pure (Left a)

    step (Left a) = (`Yield` Right a) <$> m
    step (Right a) = pure $ Stop (Just a)

    extract (Left a) = pure $ Just a
    extract (Right a) = pure $ Just a

data Cross s1 b s2 = CrossOuter s1 | CrossInner b s2

-- Two producers consuming from the same shared state in an interleaving
-- fashion and producing tuples with output from each one. For example, we can
-- interleave two Source.parseMany.
--
-- We can also have two interleaved producers running serially one after
-- another and producing elements in the same output stream:
--
-- interleave :: Monad m => Producer m a b -> Producer m a b -> Producer m a b
--
-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two producers.
--
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Producer m a b -> Producer m a c -> Producer m a (b, c)
cross (Producer step1 inject1 extract1) (Producer step2 inject2 extract2) =
    Producer step inject extract

    where

    -- XXX What if we want to stop the whole loop rather than stopping the
    -- iteration? How do we distinguish that? Perhaps we need "inject" to say
    -- Stop for that.
    inject a = do
        s1 <- inject1 a
        return $ CrossOuter s1

    {-# INLINE_LATE step #-}
    step (CrossOuter s1) = do
        r <- step1 s1
        case r of
            Yield b s -> do
                res <- extract1 s
                case res of
                    -- XXX We should probably undo what step1 did since we are
                    -- discarding "b" here. The state type must support
                    -- something like "unread". Or should this be considered as
                    -- an error?
                    Nothing -> return $ Stop Nothing
                    Just a -> do
                        s2 <- inject2 a
                        return $ Skip (CrossInner b s2)
            Skip s -> return $ Skip (CrossOuter s)
            Stop a -> return $ Stop a

    step (CrossInner b s2) = do
        r <- step2 s2
        case r of
            Yield c s -> return $ Yield (b, c) (CrossInner b s)
            Skip s -> return $ Skip (CrossInner b s)
            Stop res -> do
                case res of
                    Nothing -> return $ Stop Nothing
                    Just a -> do
                        s1 <- inject1 a
                        return $ Skip (CrossOuter s1)

    extract (CrossOuter s1) = extract1 s1
    extract (CrossInner _ s2) = extract2 s2

-- | Example:
--
-- >>> Stream.toList $ Stream.unfold (Producer.simplify $ ((,) <$> Producer.fromList <*> Producer.fromList)) ([1,2,3,4])
-- [(1,2),(1,3),(1,4)]
--
instance Monad m => Applicative (Producer m a) where
    {-# INLINE pure #-}
    pure = const Prelude.. return

    {-# INLINE (<*>) #-}
    u1 <*> u2 = fmap (\(a, b) -> a b) (cross u1 u2)

    -- {-# INLINE (*>) #-}
    -- (*>) = apSequence

    -- {-# INLINE (<*) #-}
    -- (<*) = apDiscardSnd

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | State representing a nested loop.
{-# ANN type NestedLoop Fuse #-}
data NestedLoop s1 s2 = OuterLoop s1 | InnerLoop s1 s2

-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Pre-release/
--
{-# INLINE_NORMAL concat #-}
concat :: Monad m =>
    Producer m a b -> Producer m b c -> Producer m (NestedLoop a b) c
concat (Producer step1 inject1 extract1) (Producer step2 inject2 extract2) =
    Producer step inject extract

    where

    inject (OuterLoop x) = do
        s <- inject1 x
        return $ OuterLoop s
    inject (InnerLoop x y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return $ InnerLoop s1 s2

    {-# INLINE_LATE step #-}
    step (OuterLoop st) = do
        r <- step1 st
        case r of
            Yield b s -> do
                s2 <- inject2 b
                return $ Skip (InnerLoop s s2)
            Skip s -> return $ Skip (OuterLoop s)
            Stop res -> return $ Stop $ fmap OuterLoop res

    step (InnerLoop s1 s2) = do
        r <- step2 s2
        return $ case r of
            Yield c s -> Yield c (InnerLoop s1 s)
            Skip s -> Skip (InnerLoop s1 s)
            Stop res ->
                case res of
                    Nothing -> Skip (OuterLoop s1)
                    -- XXX When the state is not fully consumed should we stop
                    -- or discard and continue or error out?
                    Just _ -> Skip (OuterLoop s1)

    extract (OuterLoop s1) = fmap OuterLoop <$> extract1 s1
    extract (InnerLoop s1 s2) = do
        r1 <- extract1 s1
        case r1 of
            Nothing -> return Nothing
            Just a -> do
                r2 <- extract2 s2
                return $ Just $ case r2 of
                    Nothing -> OuterLoop a
                    Just b -> InnerLoop a b
