{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |
-- Module      : Streamly.Internal.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- Also see the "Streamly.Internal.Data.Sink" module that provides specialized left folds
-- that discard the outputs.
--
-- IMPORTANT: keep the signatures consistent with the folds in Streamly.Prelude

module Streamly.Internal.Data.Fold
    (
    -- * Fold Type
      Fold (..)

    , hoist
    , generally

    -- , tail
    -- , init

    -- * Fold Creation Utilities
    , mkPure
    , mkPureId
    , mkFold
    , mkFoldId

    -- ** Full Folds
    , drain
    , drainBy
    , drainBy2
    , last
    , length
    , sum
    , product
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    -- , the
    , mean
    , variance
    , stdDev
    , rollingHash
    , rollingHashWithSalt
    , rollingHashFirstN
    -- , rollingHashLastN

    -- ** Full Folds (Monoidal)
    , mconcat
    , mconcatTo
    , foldMap
    , foldMapM

    -- ** Full Folds (To Containers)

    , toList
    , toListRevF  -- experimental

    -- ** Partial Folds
    , drainN
    , drainWhile
    -- , lastN
    -- , (!!)
    -- , genericIndex
    , index
    , head
    -- , findM
    , find
    , lookup
    , findIndex
    , elemIndex
    , null
    , elem
    , notElem
    -- XXX these are slower than right folds even when full input is used
    , all
    , any
    , and
    , or

    -- * Transformations

    -- ** Covariant Operations
    , sequence
    , mapM

    -- ** Mapping
    , transform
    , lmap
    --, lsequence
    , lmapM
    -- ** Filtering
    , lfilter
    , lfilterM
    -- , ldeleteBy
    -- , luniq
    , lcatMaybes

    {-
    -- ** Mapping Filters
    , lmapMaybe
    , lmapMaybeM

    -- ** Scanning Filters
    , lfindIndices
    , lelemIndices

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , linsertBy
    , lintersperseM

    -- ** Reordering
    , lreverse
    -}

    -- * Parsing
    -- ** Trimming
    , ltake
    -- , lrunFor -- time
    , ltakeWhile
    {-
    , ltakeWhileM
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    , lsessionsOf
    , lchunksOf

    -- ** Breaking

    -- Binary
    , splitAt -- spanN
    -- , splitIn -- sessionN

    -- By elements
    , span  -- spanWhile
    , break -- breakBefore
    -- , breakAfter
    -- , breakOn
    -- , breakAround
    , spanBy
    , spanByRolling

    -- By sequences
    -- , breakOnSeq
    -- , breakOnStream -- on a stream

    -- * Distributing

    , tee
    , distribute
    , distribute_

    -- * Partitioning

    -- , partitionByM
    -- , partitionBy
    , partition

    -- * Demultiplexing

    , demux
    -- , demuxWith
    , demux_
    , demuxDefault_
    -- , demuxWith_
    , demuxWithDefault_

    -- * Classifying

    , classify
    -- , classifyWith

    -- * Unzipping
    , unzip
    -- These can be expressed using lmap/lmapM and unzip
    -- , unzipWith
    -- , unzipWithM

    -- * Nested Folds
    -- , concatMap
    , foldChunks
    , duplicate

    -- * Exceptions
    , handle
    , onException
    , bracket
    , before
    , after
    , finally

    -- * Running Folds
    , initialize
    , runStep

    -- * Folding to SVar
    , toParallelSVar
    , toParallelSVarLimited
    )
where

import Control.Exception (mask_)
import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (liftBaseOp_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Map.Strict (Map)

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM)

import qualified Control.Monad.Catch as MC
import qualified Data.Map.Strict as Map
import qualified Prelude

import Streamly.Internal.Data.IORef (newFinalizedIORef, runIORefFinalizer)
import Streamly.Internal.Data.Pipe.Types (Pipe (..), PipeState(..))
import Streamly.Internal.Data.Fold.Types
import Streamly.Internal.Data.Strict
import Streamly.Internal.Data.SVar

import qualified Streamly.Internal.Data.Pipe.Types as Pipe

------------------------------------------------------------------------------
-- Smart constructors
------------------------------------------------------------------------------

-- | Make a fold using a pure step function, a pure initial state and
-- a pure state extraction function.
--
-- /Internal/
--
{-# INLINE mkPure #-}
mkPure :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
mkPure step initial extract =
    Fold (\s a -> return $ step s a) (return initial) (return . extract)

-- | Make a fold using a pure step function and a pure initial state. The
-- final state extracted is identical to the intermediate state.
--
-- /Internal/
--
{-# INLINE mkPureId #-}
mkPureId :: Monad m => (b -> a -> b) -> b -> Fold m a b
mkPureId step initial = mkPure step initial id

-- | Make a fold with an effectful step function and initial state, and a state
-- extraction function.
--
-- > mkFold = Fold
--
--  We can just use 'Fold' but it is provided for completeness.
--
-- /Internal/
--
{-# INLINE mkFold #-}
mkFold :: (s -> a -> m s) -> m s -> (s -> m b) -> Fold m a b
mkFold = Fold

-- | Make a fold with an effectful step function and initial state.  The final
-- state extracted is identical to the intermediate state.
--
-- /Internal/
--
{-# INLINE mkFoldId #-}
mkFoldId :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
mkFoldId step initial = Fold step initial return

------------------------------------------------------------------------------
-- hoist
------------------------------------------------------------------------------

-- | Change the underlying monad of a fold
--
-- /Internal/
hoist :: (forall x. m x -> n x) -> Fold m a b -> Fold n a b
hoist f (Fold step initial extract) =
    Fold (\x a -> f $ step x a) (f initial) (f . extract)

-- | Adapt a pure fold to any monad
--
-- > generally = hoist (return . runIdentity)
--
-- /Internal/
generally :: Monad m => Fold Identity a b -> Fold m a b
generally = hoist (return . runIdentity)

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- | Flatten the monadic output of a fold to pure output.
--
-- @since 0.7.0
{-# INLINE sequence #-}
sequence :: Monad m => Fold m a (m b) -> Fold m a b
sequence (Fold step initial extract) = Fold step initial extract'
  where
    extract' x = do
        act <- extract x
        act >>= return

-- | Map a monadic function on the output of a fold.
--
-- @since 0.7.0
{-# INLINE mapM #-}
mapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
mapM f = sequence . fmap f

------------------------------------------------------------------------------
-- Transformations on fold inputs
------------------------------------------------------------------------------

-- rename to lpipe?
--
-- | Apply a transformation on a 'Fold' using a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE transform #-}
transform :: Monad m => Pipe m a b -> Fold m b c -> Fold m a c
transform (Pipe pstep1 pstep2 pinitial) (Fold fstep finitial fextract) =
    Fold step initial extract

    where

    initial = Tuple' <$> return pinitial <*> finitial
    step (Tuple' ps fs) x = do
        r <- pstep1 ps x
        go fs r

        where
        -- XXX use SPEC?
        go acc (Pipe.Yield b (Consume ps')) = do
            acc' <- fstep acc b
            return (Tuple' ps' acc')

        go acc (Pipe.Yield b (Produce ps')) = do
            acc' <- fstep acc b
            r <- pstep2 ps'
            go acc' r

        go acc (Pipe.Continue (Consume ps')) = return (Tuple' ps' acc)

        go acc (Pipe.Continue (Produce ps')) = do
            r <- pstep2 ps'
            go acc r

    extract (Tuple' _ fs) = fextract fs

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | @_Fold1 step@ returns a new 'Fold' using just a step function that has the
-- same type for the accumulator and the element. The result type is the
-- accumulator type wrapped in 'Maybe'. The initial accumulator is retrieved
-- from the 'Foldable', the result is 'None' for empty containers.
{-# INLINABLE _Fold1 #-}
_Fold1 :: Monad m => (a -> a -> a) -> Fold m a (Maybe a)
_Fold1 step = Fold step_ (return Nothing') (return . toMaybe)
  where
    step_ mx a = return $ Just' $
        case mx of
            Nothing' -> a
            Just' x -> step x a

------------------------------------------------------------------------------
-- Left folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Run Effects
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- > drain = drainBy (const (return ()))
--
-- @since 0.7.0
{-# INLINABLE drain #-}
drain :: Monad m => Fold m a ()
drain = Fold step begin done
    where
    begin = return ()
    step _ _ = return ()
    done = return

-- |
-- > drainBy f = lmapM f drain
-- > drainBy = FL.foldMapM (void . f)
--
-- Drain all input after passing it through a monadic function. This is the
-- dual of mapM_ on stream producers.
--
-- @since 0.7.0
{-# INLINABLE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Fold m a ()
drainBy f = Fold (const (void . f)) (return ()) return

{-# INLINABLE drainBy2 #-}
drainBy2 ::  Monad m => (a -> m b) -> Fold2 m c a ()
drainBy2 f = Fold2 (const (void . f)) (\_ -> return ()) return

-- | Extract the last element of the input stream, if any.
--
-- > last = fmap getLast $ FL.foldMap (Last . Just)
--
-- @since 0.7.0
{-# INLINABLE last #-}
last :: Monad m => Fold m a (Maybe a)
last = _Fold1 (flip const)

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Like 'length', except with a more general 'Num' return value
--
-- > genericLength = fmap getSum $ foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINABLE genericLength #-}
genericLength :: (Monad m, Num b) => Fold m a b
genericLength = Fold (\n _ -> return $ n + 1) (return 0) return

-- | Determine the length of the input stream.
--
-- > length = fmap getSum $ foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINABLE length #-}
length :: Monad m => Fold m a Int
length = genericLength

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- > sum = fmap getSum $ FL.foldMap Sum
--
-- @since 0.7.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum = Fold (\x a -> return $ x + a) (return 0) return

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty.
--
-- > product = fmap getProduct $ FL.foldMap Product
--
-- @since 0.7.0
{-# INLINABLE product #-}
product :: (Monad m, Num a) => Fold m a a
product = Fold (\x a -> return $ x * a) (return 1) return

------------------------------------------------------------------------------
-- To Summary (Maybe)
------------------------------------------------------------------------------

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- @since 0.7.0
{-# INLINABLE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
maximumBy cmp = _Fold1 max'
  where
    max' x y = case cmp x y of
        GT -> x
        _  -> y

-- |
-- @
-- maximum = 'maximumBy' compare
-- @
--
-- Determine the maximum element in a stream.
--
-- Compare with @FL.foldMap Max@.
--
-- @since 0.7.0
{-# INLINABLE maximum #-}
maximum :: (Monad m, Ord a) => Fold m a (Maybe a)
maximum = _Fold1 max

-- | Computes the minimum element with respect to the given comparison function
--
-- @since 0.7.0
{-# INLINABLE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
minimumBy cmp = _Fold1 min'
  where
    min' x y = case cmp x y of
        GT -> y
        _  -> x

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- @
-- minimum = 'minimumBy' compare
-- @
--
-- Compare with @FL.foldMap Min@.
--
-- @since 0.7.0
{-# INLINABLE minimum #-}
minimum :: (Monad m, Ord a) => Fold m a (Maybe a)
minimum = _Fold1 min

------------------------------------------------------------------------------
-- To Summary (Statistical)
------------------------------------------------------------------------------

-- | Compute a numerically stable arithmetic mean of all elements in the input
-- stream.
--
-- @since 0.7.0
{-# INLINABLE mean #-}
mean :: (Monad m, Fractional a) => Fold m a a
mean = Fold step (return begin) (return . done)
  where
    begin = Tuple' 0 0
    step (Tuple' x n) y = return $
        let n' = n + 1
        in Tuple' (x + (y - x) / n') n'
    done (Tuple' x _) = x

-- | Compute a numerically stable (population) variance over all elements in
-- the input stream.
--
-- @since 0.7.0
{-# INLINABLE variance #-}
variance :: (Monad m, Fractional a) => Fold m a a
variance = Fold step (return begin) (return . done)
  where
    begin = Tuple3' 0 0 0

    step (Tuple3' n mean_ m2) x = return $ Tuple3' n' mean' m2'
      where
        n'     = n + 1
        mean'  = (n * mean_ + x) / (n + 1)
        delta  = x - mean_
        m2'    = m2 + delta * delta * n / (n + 1)

    done (Tuple3' n _ m2) = m2 / n

-- | Compute a numerically stable (population) standard deviation over all
-- elements in the input stream.
--
-- @since 0.7.0
{-# INLINABLE stdDev #-}
stdDev :: (Monad m, Floating a) => Fold m a a
stdDev = sqrt variance

-- | Compute an 'Int' sized polynomial rolling hash
--
-- > H = salt * k ^ n + c1 * k ^ (n - 1) + c2 * k ^ (n - 2) + ... + cn * k ^ 0
--
-- Where @c1@, @c2@, @cn@ are the elements in the input stream and @k@ is a
-- constant.
--
-- This hash is often used in Rabin-Karp string search algorithm.
--
-- See https://en.wikipedia.org/wiki/Rolling_hash
--
-- @since 0.7.0
{-# INLINABLE rollingHashWithSalt #-}
rollingHashWithSalt :: (Monad m, Enum a) => Int64 -> Fold m a Int64
rollingHashWithSalt salt = Fold step initial extract
    where
    k = 2891336453 :: Int64
    initial = return salt
    step cksum a = return $ cksum * k + fromIntegral (fromEnum a)
    extract = return

-- | A default salt used in the implementation of 'rollingHash'.
{-# INLINE defaultSalt #-}
defaultSalt :: Int64
defaultSalt = -2578643520546668380

-- | Compute an 'Int' sized polynomial rolling hash of a stream.
--
-- > rollingHash = rollingHashWithSalt defaultSalt
--
-- @since 0.7.0
{-# INLINABLE rollingHash #-}
rollingHash :: (Monad m, Enum a) => Fold m a Int64
rollingHash = rollingHashWithSalt defaultSalt

-- | Compute an 'Int' sized polynomial rolling hash of the first n elements of
-- a stream.
--
-- > rollingHashFirstN = ltake n rollingHash
{-# INLINABLE rollingHashFirstN #-}
rollingHashFirstN :: (Monad m, Enum a) => Int -> Fold m a Int64
rollingHashFirstN n = ltake n rollingHash

------------------------------------------------------------------------------
-- Monoidal left folds
------------------------------------------------------------------------------

-- | 'mappend' the elements of an input stream to a provided starting value.
--
-- > S.fold (FL.mconcatTo 10) (S.map Sum $ S.enumerateFromTo 1 10)
--
-- This could be faster than (fmap (m0 <>) FL.mconcat) especially when we have
-- to perform the operation many times.
--
-- /Internal/
--
{-# INLINE mconcatTo #-}
mconcatTo :: (Monad m, Monoid a) => a -> Fold m a a
mconcatTo i = Fold (\x a -> return $ mappend x a) (return i) return

-- | Fold an input stream consisting of monoidal elements using 'mappend'
-- and 'mempty'.
--
-- > S.fold FL.mconcat (S.map Sum $ S.enumerateFromTo 1 10)
--
-- @since 0.7.0
{-# INLINE mconcat #-}
mconcat :: (Monad m, Monoid a) => Fold m a a
mconcat = mconcatTo mempty

-- |
-- > foldMap f = lmap f mconcat
--
-- Make a fold from a pure function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > S.fold (FL.foldMap Sum) $ S.enumerateFromTo 1 10
--
-- @since 0.7.0
{-# INLINABLE foldMap #-}
foldMap :: (Monad m, Monoid b) => (a -> b) -> Fold m a b
foldMap f = lmap f mconcat

-- |
-- > foldMapM f = lmapM f mconcat
--
-- Make a fold from a monadic function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > S.fold (FL.foldMapM (return . Sum)) $ S.enumerateFromTo 1 10
--
-- @since 0.7.0
{-# INLINABLE foldMapM #-}
foldMapM ::  (Monad m, Monoid b) => (a -> m b) -> Fold m a b
foldMapM act = Fold step begin done
    where
    done = return
    begin = return mempty
    step m a = do
        m' <- act a
        return $! mappend m m'

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Memory.Array" instead.
--
-- @since 0.7.0

-- id . (x1 :) . (x2 :) . (x3 :) . ... . (xn :) $ []
{-# INLINABLE toList #-}
toList :: Monad m => Fold m a [a]
toList = Fold (\f x -> return $ f . (x :))
              (return id)
              (return . ($ []))

------------------------------------------------------------------------------
-- Partial Folds
------------------------------------------------------------------------------

-- | A fold that drains the first n elements of its input, running the effects
-- and discarding the results.
{-# INLINABLE drainN #-}
drainN :: Monad m => Int -> Fold m a ()
drainN n = ltake n drain

-- | A fold that drains elements of its input as long as the predicate succeeds,
-- running the effects and discarding the results.
{-# INLINABLE drainWhile #-}
drainWhile :: Monad m => (a -> Bool) -> Fold m a ()
drainWhile p = ltakeWhile p drain

------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- @since 0.7.0
{-# INLINABLE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Fold m a (Maybe a)
genericIndex i = Fold step (return $ Left' 0) done
  where
    step x a = return $
        case x of
            Left'  j -> if i == j
                        then Right' a
                        else Left' (j + 1)
            _        -> x
    done x = return $
        case x of
            Left'  _ -> Nothing
            Right' a -> Just a

-- | Lookup the element at the given index.
--
-- @since 0.7.0
{-# INLINABLE index #-}
index :: Monad m => Int -> Fold m a (Maybe a)
index = genericIndex

-- | Extract the first element of the stream, if any.
--
-- > head = fmap getFirst $ FL.foldMap (First . Just)
--
-- @since 0.7.0
{-# INLINABLE head #-}
head :: Monad m => Fold m a (Maybe a)
head = _Fold1 const

-- | Returns the first element that satisfies the given predicate.
--
-- @
-- find p = fmap getFirst $
--     FL.foldMap (\x -> First (if p x then Just x else Nothing))
-- @
--
-- @since 0.7.0
{-# INLINABLE find #-}
find :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
find predicate = Fold step (return Nothing') (return . toMaybe)
  where
    step x a = return $
        case x of
            Nothing' -> if predicate a
                        then Just' a
                        else Nothing'
            _        -> x

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> find ((==) . fst)
--
-- @since 0.7.0
{-# INLINABLE lookup #-}
lookup :: (Eq a, Monad m) => a -> Fold m (a,b) (Maybe b)
lookup a0 = Fold step (return Nothing') (return . toMaybe)
  where
    step x (a,b) = return $
        case x of
            Nothing' -> if a == a0
                        then Just' b
                        else Nothing'
            _ -> x

-- | Convert strict 'Either'' to lazy 'Maybe'
{-# INLINABLE hush #-}
hush :: Either' a b -> Maybe b
hush (Left'  _) = Nothing
hush (Right' b) = Just b

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINABLE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndex predicate = Fold step (return $ Left' 0) (return . hush)
  where
    step x a = return $
        case x of
            Left' i ->
                if predicate a
                then Right' i
                else Left' (i + 1)
            _       -> x

-- | Returns the first index where a given value is found in the stream.
--
-- > elemIndex a = findIndex (== a)
--
-- @since 0.7.0
{-# INLINABLE elemIndex #-}
elemIndex :: (Eq a, Monad m) => a -> Fold m a (Maybe Int)
elemIndex a = findIndex (a ==)

------------------------------------------------------------------------------
-- To Boolean
------------------------------------------------------------------------------

-- | Return 'True' if the input stream is empty.
--
-- > null = fmap isJust head
--
-- @since 0.7.0
{-# INLINABLE null #-}
null :: Monad m => Fold m a Bool
null = Fold (\_ _ -> return False) (return True) return

-- |
-- > any p = lmap p or
-- > any p = fmap getAny . FL.foldMap (Any . p)
--
-- | Returns 'True' if any of the elements of a stream satisfies a predicate.
--
-- @since 0.7.0
{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Fold m a Bool
any predicate = Fold (\x a -> return $ x || predicate a) (return False) return

-- | Return 'True' if the given element is present in the stream.
--
-- > elem a = any (== a)
--
-- @since 0.7.0
{-# INLINABLE elem #-}
elem :: (Eq a, Monad m) => a -> Fold m a Bool
elem a = any (a ==)

-- |
-- > all p = lmap p and
-- > all p = fmap getAll . FL.foldMap (All . p)
--
-- | Returns 'True' if all elements of a stream satisfy a predicate.
--
-- @since 0.7.0
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Fold m a Bool
all predicate = Fold (\x a -> return $ x && predicate a) (return True) return

-- | Returns 'True' if the given element is not present in the stream.
--
-- > notElem a = all (/= a)
--
-- @since 0.7.0
{-# INLINABLE notElem #-}
notElem :: (Eq a, Monad m) => a -> Fold m a Bool
notElem a = all (a /=)

-- | Returns 'True' if all elements are 'True', 'False' otherwise
--
-- > and = all (== True)
-- > and = fmap getAll . FL.foldMap All
--
-- @since 0.7.0
{-# INLINABLE and #-}
and :: Monad m => Fold m Bool Bool
and = Fold (\x a -> return $ x && a) (return True) return

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- > or = any (== True)
-- > or = fmap getAny . FL.foldMap Any
--
-- @since 0.7.0
{-# INLINABLE or #-}
or :: Monad m => Fold m Bool Bool
or = Fold (\x a -> return $ x || a) (return False) return

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------
--
-- XXX These would just be applicative compositions of terminating folds.

-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are consumed by fold @f1@ and the rest of the stream
-- is consumed by fold @f2@.
--
-- > let splitAt_ n xs = S.fold (FL.splitAt n FL.toList FL.toList) $ S.fromList xs
--
-- >>> splitAt_ 6 "Hello World!"
-- > ("Hello ","World!")
--
-- >>> splitAt_ (-1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 0 [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 1 [1,2,3]
-- > ([1],[2,3])
--
-- >>> splitAt_ 3 [1,2,3]
-- > ([1,2,3],[])
--
-- >>> splitAt_ 4 [1,2,3]
-- > ([1,2,3],[])
--
-- /Internal/

-- This can be considered as a two-fold version of 'ltake' where we take both
-- the segments instead of discarding the leftover.
--
{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract
    where
      initial  = Tuple3' <$> return n <*> initialL <*> initialR

      step (Tuple3' i xL xR) input =
        if i > 0
        then stepL xL input >>= (\a -> return (Tuple3' (i - 1) a xR))
        else stepR xR input >>= (\b -> return (Tuple3' i xL b))

      extract (Tuple3' _ a b) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
-- /Internal/
--
spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where
      initial = Tuple3' <$> initialL <*> initialR <*> return (Tuple' Nothing True)

      step (Tuple3' a b (Tuple' (Just frst) isFirstG)) input =
        if cmp frst input && isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just frst) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      step (Tuple3' a b (Tuple' Nothing isFirstG)) input =
        if isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just input) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- > let span_ p xs = S.fold (S.span p FL.toList FL.toList) $ S.fromList xs
--
-- >>> span_ (< 1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> span_ (< 2) [1,2,3]
-- > ([1],[2,3])
--
-- >>> span_ (< 4) [1,2,3]
-- > ([1,2,3],[])
--
-- /Internal/

-- This can be considered as a two-fold version of 'ltakeWhile' where we take
-- both the segments instead of discarding the leftover.
{-# INLINE span #-}
span
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
span p (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = Tuple3' <$> initialL <*> initialR <*> return True

    step (Tuple3' a b isFirstG) input =
        if isFirstG && p input
        then stepL a input >>= (\a' -> return (Tuple3' a' b True))
        else stepR b input >>= (\a' -> return (Tuple3' a a' False))

    extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- |
-- > break p = span (not . p)
--
-- Break as soon as the predicate becomes 'True'. @break p f1 f2@ composes
-- folds @f1@ and @f2@ such that @f1@ stops consuming input as soon as the
-- predicate @p@ becomes 'True'. The rest of the input is consumed @f2@.
--
-- This is the binary version of 'splitBy'.
--
-- > let break_ p xs = S.fold (S.break p FL.toList FL.toList) $ S.fromList xs
--
-- >>> break_ (< 1) [3,2,1]
-- > ([3,2,1],[])
--
-- >>> break_ (< 2) [3,2,1]
-- > ([3,2],[1])
--
-- >>> break_ (< 4) [3,2,1]
-- > ([],[3,2,1])
--
-- /Internal/
{-# INLINE break #-}
break
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
break p = span (not . p)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
--
-- /Internal/
{-# INLINE spanByRolling #-}
spanByRolling
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanByRolling cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

  where
    initial = Tuple3' <$> initialL <*> initialR <*> return Nothing

    step (Tuple3' a b (Just frst)) input =
      if cmp input frst
      then stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))
      else stepR b input >>= (\b' -> return (Tuple3' a b' (Just input)))

    step (Tuple3' a b Nothing) input =
      stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))

    extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

{-
-- | Find the first occurrence of the specified sequence in the input stream
-- and break the input stream into two parts, the first part consisting of the
-- stream before the sequence and the second part consisting of the sequence
-- and the rest of the stream.
--
-- > let breakOn_ pat xs = S.fold (S.breakOn pat FL.toList FL.toList) $ S.fromList xs
--
-- >>> breakOn_ "dear" "Hello dear world!"
-- > ("Hello ","dear world!")
--
{-# INLINE breakOn #-}
breakOn :: Monad m => Array a -> Fold m a b -> Fold m a c -> Fold m a (b,c)
breakOn pat f m = undefined
-}

------------------------------------------------------------------------------
-- Distributing
------------------------------------------------------------------------------
--
-- | Distribute one copy of the stream to each fold and zip the results.
--
-- @
--                 |-------Fold m a b--------|
-- ---stream m a---|                         |---m (b,c)
--                 |-------Fold m a c--------|
-- @
-- >>> S.fold (FL.tee FL.sum FL.length) (S.enumerateFromTo 1.0 100.0)
-- (5050.0,100)
--
-- @since 0.7.0
{-# INLINE tee #-}
tee :: Monad m => Fold m a b -> Fold m a c -> Fold m a (b,c)
tee f1 f2 = (,) <$> f1 <*> f2

{-# INLINE foldNil #-}
foldNil :: Monad m => Fold m a [b]
foldNil = Fold step begin done  where
  begin = return []
  step _ _ = return []
  done = return

{-# INLINE foldCons #-}
foldCons :: Monad m => Fold m a b -> Fold m a [b] -> Fold m a [b]
foldCons (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = Tuple' <$> beginL <*> beginR
    step (Tuple' xL xR) a = Tuple' <$> stepL xL a <*> stepR xR a
    done (Tuple' xL xR) = (:) <$> (doneL xL) <*> (doneR xR)

-- XXX use "List" instead of "[]"?, use Array for output to scale it to a large
-- number of consumers? For polymorphic case a vector could be helpful. For
-- Storables we can use arrays. Will need separate APIs for those.
--
-- | Distribute one copy of the stream to each fold and collect the results in
-- a container.
--
-- @
--
--                 |-------Fold m a b--------|
-- ---stream m a---|                         |---m [b]
--                 |-------Fold m a b--------|
--                 |                         |
--                            ...
-- @
--
-- >>> S.fold (FL.distribute [FL.sum, FL.length]) (S.enumerateFromTo 1 5)
-- [15,5]
--
-- This is the consumer side dual of the producer side 'sequence' operation.
--
-- @since 0.7.0
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute [] = foldNil
distribute (x:xs) = foldCons x (distribute xs)

-- | Like 'distribute' but for folds that return (), this can be more efficient
-- than 'distribute' as it does not need to maintain state.
--
{-# INLINE distribute_ #-}
distribute_ :: Monad m => [Fold m a ()] -> Fold m a ()
distribute_ fs = Fold step initial extract
    where
    initial    = Prelude.mapM (\(Fold s i e) ->
        i >>= \r -> return (Fold s (return r) e)) fs
    step ss a  = do
        Prelude.mapM_ (\(Fold s i _) -> i >>= \r -> s r a >> return ()) ss
        return ss
    extract ss = do
        Prelude.mapM_ (\(Fold _ i e) -> i >>= \r -> e r) ss
        return ()

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------
--
-- | Partition the input over two folds using an 'Either' partitioning
-- predicate.
--
-- @
--
--                                     |-------Fold b x--------|
-- -----stream m a --> (Either b c)----|                       |----(x,y)
--                                     |-------Fold c y--------|
-- @
--
-- Send input to either fold randomly:
--
-- >>> import System.Random (randomIO)
-- >>> randomly a = randomIO >>= \x -> return $ if x then Left a else Right a
-- >>> S.fold (FL.partitionByM randomly FL.length FL.length) (S.enumerateFromTo 1 100)
-- (59,41)
--
-- Send input to the two folds in a proportion of 2:1:
--
-- @
-- import Data.IORef (newIORef, readIORef, writeIORef)
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m Left, replicate n Right]
--  return $ \\a -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ head r a
--
-- main = do
--  f <- proportionately 2 1
--  r <- S.fold (FL.partitionByM f FL.length FL.length) (S.enumerateFromTo (1 :: Int) 100)
--  print r
-- @
-- @
-- (67,33)
-- @
--
-- This is the consumer side dual of the producer side 'mergeBy' operation.
--
-- @since 0.7.0
{-# INLINE partitionByM #-}
partitionByM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =

    Fold step begin done

    where

    begin = Tuple' <$> beginL <*> beginR
    step (Tuple' xL xR) a = do
        r <- f a
        case r of
            Left b -> Tuple' <$> stepL xL b <*> return xR
            Right c -> Tuple' <$> return xL <*> stepR xR c
    done (Tuple' xL xR) = (,) <$> doneL xL <*> doneR xR

-- Note: we could use (a -> Bool) instead of (a -> Either b c), but the latter
-- makes the signature clearer as to which case belongs to which fold.
-- XXX need to check the performance in both cases.

-- | Same as 'partitionByM' but with a pure partition function.
--
-- Count even and odd numbers in a stream:
--
-- @
-- >>> let f = FL.partitionBy (\\n -> if even n then Left n else Right n)
--                       (fmap (("Even " ++) . show) FL.length)
--                       (fmap (("Odd "  ++) . show) FL.length)
--   in S.fold f (S.enumerateFromTo 1 100)
-- ("Even 50","Odd 50")
-- @
--
-- @since 0.7.0
{-# INLINE partitionBy #-}
partitionBy :: Monad m
    => (a -> Either b c) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionBy f = partitionByM (return . f)

-- | Compose two folds such that the combined fold accepts a stream of 'Either'
-- and routes the 'Left' values to the first fold and 'Right' values to the
-- second fold.
--
-- > partition = partitionBy id
--
-- @since 0.7.0
{-# INLINE partition #-}
partition :: Monad m
    => Fold m b x -> Fold m c y -> Fold m (Either b c) (x, y)
partition = partitionBy id

{-
-- | Send one item to each fold in a round-robin fashion. This is the consumer
-- side dual of producer side 'mergeN' operation.
--
-- partitionN :: Monad m => [Fold m a b] -> Fold m a [b]
-- partitionN fs = Fold step begin done
-}

-- TODO Demultiplex an input element into a number of typed variants. We want
-- to statically restrict the target values within a set of predefined types,
-- an enumeration of a GADT. We also want to make sure that the Map contains
-- only those types and the full set of those types.
--
-- TODO Instead of the input Map it should probably be a lookup-table using an
-- array and not in GC memory. The same applies to the output Map as well.
-- However, that would only be helpful if we have a very large data structure,
-- need to measure and see how it scales.
--
-- This is the consumer side dual of the producer side 'mux' operation (XXX to
-- be implemented).

-- | Split the input stream based on a key field and fold each split using a
-- specific fold collecting the results in a map from the keys to the results.
-- Useful for cases like protocol handlers to handle different type of packets
-- using different handlers.
--
-- @
--
--                             |-------Fold m a b
-- -----stream m a-----Map-----|
--                             |-------Fold m a b
--                             |
--                                       ...
-- @
--
-- @since 0.7.0
{-# INLINE demuxWith #-}
demuxWith :: (Monad m, Ord k)
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m a (Map k b)
demuxWith f kv = Fold step initial extract

    where

    initial = return kv
-- alterF is available only since containers version 0.5.8.2
#if MIN_VERSION_containers(0,5,8)
    step mp a = case f a of
      (k, a') -> Map.alterF twiddle k mp
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
        -- XXX we could use a monadic update function for a single lookup and
        -- update in the map.
        where
          twiddle Nothing = pure Nothing
          twiddle (Just (Fold step' acc extract')) = do
            !r <- acc >>= \x -> step' x a'
            pure . Just $ Fold step' (return r) extract'
#else
    step mp a =
        let (k, a') = f a
        in case Map.lookup k mp of
            Nothing -> return mp
            Just (Fold step' acc extract') -> do
                !r <- acc >>= \x -> step' x a'
                return $ Map.insert k (Fold step' (return r) extract') mp
#endif
    extract = Prelude.mapM (\(Fold _ acc e) -> acc >>= e)

-- | Fold a stream of key value pairs using a map of specific folds for each
-- key into a map from keys to the results of fold outputs of the corresponding
-- values.
--
-- @
-- > let table = Data.Map.fromList [(\"SUM", FL.sum), (\"PRODUCT", FL.product)]
--       input = S.fromList [(\"SUM",1),(\"PRODUCT",2),(\"SUM",3),(\"PRODUCT",4)]
--   in S.fold (FL.demux table) input
-- fromList [("PRODUCT",8),("SUM",4)]
-- @
--
-- @since 0.7.0
{-# INLINE demux #-}
demux :: (Monad m, Ord k)
    => Map k (Fold m a b) -> Fold m (k, a) (Map k b)
demux = demuxWith id

{-# INLINE demuxWithDefault_ #-}
demuxWithDefault_ :: (Monad m, Ord k)
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m (k, a') b -> Fold m a ()
demuxWithDefault_ f kv (Fold dstep dinitial dextract) =
    Fold step initial extract

    where

    initFold (Fold s i e) = i >>= \r -> return (Fold s (return r) e)
    initial = do
        mp <- Prelude.mapM initFold kv
        dacc <- dinitial
        return (Tuple' mp dacc)
    step (Tuple' mp dacc) a
      | (k, a') <- f a
      = case Map.lookup k mp of
            Nothing -> do
                acc <- dstep dacc (k, a')
                return (Tuple' mp acc)
            Just (Fold step' acc _) -> do
                _ <- acc >>= \x -> step' x a'
                return (Tuple' mp dacc)
    extract (Tuple' mp dacc) = do
        void $ dextract dacc
        Prelude.mapM_ (\(Fold _ acc e) -> acc >>= e) mp

-- | Split the input stream based on a key field and fold each split using a
-- specific fold without collecting the results. Useful for cases like protocol
-- handlers to handle different type of packets.
--
-- @
--
--                             |-------Fold m a ()
-- -----stream m a-----Map-----|
--                             |-------Fold m a ()
--                             |
--                                       ...
-- @
--
--
-- @since 0.7.0

-- demuxWith_ can be slightly faster than demuxWith because we do not need to
-- update the Map in this case. This may be significant only if the map is
-- large.
{-# INLINE demuxWith_ #-}
demuxWith_ :: (Monad m, Ord k)
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m a ()
demuxWith_ f kv = Fold step initial extract

    where

    initial = do
        Prelude.mapM (\(Fold s i e) ->
            i >>= \r -> return (Fold s (return r) e)) kv
    step mp a
        -- XXX should we raise an exception in Nothing case?
        -- Ideally we should enforce that it is a total map over k so that look
        -- up never fails
      | (k, a') <- f a
      = case Map.lookup k mp of
            Nothing -> return mp
            Just (Fold step' acc _) -> do
                _ <- acc >>= \x -> step' x a'
                return mp
    extract mp = Prelude.mapM_ (\(Fold _ acc e) -> acc >>= e) mp

-- | Given a stream of key value pairs and a map from keys to folds, fold the
-- values for each key using the corresponding folds, discarding the outputs.
--
-- @
-- > let prn = FL.drainBy print
-- > let table = Data.Map.fromList [(\"ONE", prn), (\"TWO", prn)]
--       input = S.fromList [(\"ONE",1),(\"TWO",2)]
--   in S.fold (FL.demux_ table) input
-- One 1
-- Two 2
-- @
--
-- @since 0.7.0
{-# INLINE demux_ #-}
demux_ :: (Monad m, Ord k) => Map k (Fold m a ()) -> Fold m (k, a) ()
demux_ = demuxWith_ id

{-# INLINE demuxDefault_ #-}
demuxDefault_ :: (Monad m, Ord k)
    => Map k (Fold m a ()) -> Fold m (k, a) () -> Fold m (k, a) ()
demuxDefault_ = demuxWithDefault_ id

-- TODO If the data is large we may need a map/hashmap in pinned memory instead
-- of a regular Map. That may require a serializable constraint though. We can
-- have another API for that.
--
-- | Split the input stream based on a key field and fold each split using the
-- given fold. Useful for map/reduce, bucketizing the input in different bins
-- or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in S.fold (FL.classify FL.toList) input
-- fromList [(\"ONE",[1.1,1.0]),(\"TWO",[2.2,2.0])]
-- @
--
-- @since 0.7.0
{-# INLINE classifyWith #-}
classifyWith :: (Monad m, Ord k) => (a -> k) -> Fold m a b -> Fold m a (Map k b)
classifyWith f (Fold step initial extract) = Fold step' initial' extract'

    where

    initial' = return Map.empty
    step' kv a =
        let k = f a
        in case Map.lookup k kv of
            Nothing -> do
                x <- initial
                r <- step x a
                return $ Map.insert k r kv
            Just x -> do
                r <- step x a
                return $ Map.insert k r kv
    extract' = Prelude.mapM extract

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- @
-- > let input = S.fromList [(\"ONE",1),(\"ONE",1.1),(\"TWO",2), (\"TWO",2.2)]
--   in S.fold (FL.classify FL.toList) input
-- fromList [(\"ONE",[1.1,1.0]),(\"TWO",[2.2,2.0])]
-- @
--
-- @since 0.7.0

-- Same as:
--
-- > classify fld = classifyWith fst (lmap snd fld)
--
{-# INLINE classify #-}
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify fld = classifyWith fst (lmap snd fld)

------------------------------------------------------------------------------
-- Unzipping
------------------------------------------------------------------------------
--
-- | Like 'unzipWith' but with a monadic splitter function.
--
-- @since 0.7.0
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    step (Tuple' xL xR) a = do
        (b,c) <- f a
        Tuple' <$> stepL xL b <*> stepR xR c
    begin = Tuple' <$> beginL <*> beginR
    done (Tuple' xL xR) = (,) <$> doneL xL <*> doneR xR

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- @since 0.7.0
{-# INLINE unzipWith #-}
unzipWith :: Monad m
    => (a -> (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWith f = unzipWithM (return . f)

-- | Send the elements of tuples in a stream of tuples through two different
-- folds.
--
-- @
--
--                           |-------Fold m a x--------|
-- ---------stream of (a,b)--|                         |----m (x,y)
--                           |-------Fold m b y--------|
--
-- @
--
-- This is the consumer side dual of the producer side 'zip' operation.
--
-- @since 0.7.0
{-# INLINE unzip #-}
unzip :: Monad m => Fold m a x -> Fold m b y -> Fold m (a,b) (x,y)
unzip = unzipWith id

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

{-
-- All the stream flattening transformations can also be applied to a fold
-- input stream.

-- | This can be used to apply all the stream generation operations on folds.
lconcatMap ::(IsStream t, Monad m) => (a -> t m b)
    -> Fold m b c
    -> Fold m a c
lconcatMap s f1 f2 = undefined
-}

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream. groupBy et al can be written as terminating
-- folds and then we can apply foldChunks to use those repeatedly on a stream.

-- | Apply a terminating fold repeatedly to the input of another fold.
--
-- Compare with: Streamly.Prelude.concatMap, Streamly.Prelude.foldChunks
--
-- /Unimplemented/
--
{-# INLINABLE foldChunks #-}
foldChunks ::
    -- Monad m =>
    Fold m a b -> Fold m b c -> Fold m a c
foldChunks = undefined

{-
-- XXX this would be an application of foldChunks using a terminating fold.
--
-- | Group the input stream into groups of elements between @low@ and @high@.
-- Collection starts in chunks of @low@ and then keeps doubling until we reach
-- @high@. Each chunk is folded using the provided fold function.
--
-- This could be useful, for example, when we are folding a stream of unknown
-- size to a stream of arrays and we want to minimize the number of
-- allocations.
--
-- @
--
-- XXX we should be able to implement it with parsers/terminating folds.
--
{-# INLINE lchunksInRange #-}
lchunksInRange :: Monad m
    => Int -> Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksInRange low high (Fold step1 initial1 extract1)
                        (Fold step2 initial2 extract2) = undefined
-}

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Exception handling states of a fold
data HandleExc s f1 f2 = InitDone !s | InitFailed !f1 | StepFailed !f2

-- | @handle initHandler stepHandler fold@ produces a new fold from a given
-- fold.  The new fold executes the original @fold@, if an exception occurs
-- when initializing the fold then @initHandler@ is executed and fold resulting
-- from that starts execution.  If an exception occurs while executing the
-- @step@ function of a fold then the @stephandler@ is executed and we start
-- executing the fold resulting from that.
--
-- The exception is caught and handled, not rethrown. If the exception handler
-- itself throws an exception that exception is thrown.
--
-- /Internal/
--
{-# INLINE handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> m (Fold m a b))
    -> (e -> Fold m a b -> m (Fold m a b))
    -> Fold m a b
    -> Fold m a b
handle initH stepH (Fold step1 initial1 extract1) = Fold step initial extract

    where

    initial = fmap InitDone initial1 `MC.catch` (fmap InitFailed . initH)

    step (InitDone s) a =
        let f = Fold step1 (return s) extract1
         in fmap InitDone (step1 s a)
                `MC.catch` (\e -> fmap StepFailed (stepH e f))
    step (InitFailed (Fold step2 initial2 extract2)) a = do
        s <- initial2
        s1 <- step2 s a
        return $ InitFailed $ Fold step2 (return s1) extract2
    step (StepFailed (Fold step2 initial2 extract2)) a = do
        s <- initial2
        s1 <- step2 s a
        return $ StepFailed $ Fold step2 (return s1) extract2

    extract (InitDone s) = extract1 s
    extract (InitFailed (Fold _ initial2 extract2)) = initial2 >>= extract2
    extract (StepFailed (Fold _ initial2 extract2)) = initial2 >>= extract2

-- | @onException action fold@ runs @action@ whenever the fold throws an
-- exception.  The action is executed on any exception whether it is in
-- initial, step or extract action of the fold.
--
-- The exception is not caught, simply rethrown. If the @action@ itself
-- throws an exception that exception is thrown instead of the original
-- exception.
--
-- /Internal/
--
{-# INLINE onException #-}
onException :: MonadCatch m => m x -> Fold m a b -> Fold m a b
onException action (Fold step1 initial1 extract1) = Fold step initial extract

    where

    initial = initial1 `MC.onException` action
    step s a = step1 s a `MC.onException` action
    extract s = extract1 s `MC.onException` action

-- XXX we cannot use a bracketed fold for scan, because extract would release
-- the resource. This can be fixed when we have terminating folds, we can
-- release the resource on Stop instead of on extract.
--
-- | @bracket before after between@ runs @before@ and invokes @between@ using
-- its output, then runs the fold generated by @between@.  If the fold ends
-- normally, due to an exception or if it is garbage collected prematurely then
-- @after@ is run with the output of @before@ as argument.
--
-- If @before@ or @after@ throw an exception that exception is thrown.
--
-- /Internal/
--
{-# INLINE bracket #-}
bracket :: (MonadAsync m, MonadCatch m)
    => m x -> (x -> m c) -> (x -> m (Fold m a b)) -> Fold m a b
bracket bef aft bet = Fold step initial extract

    where

    initial = do
        (r, ref) <- liftBaseOp_ mask_ $ do
            r <- bef
            ref <- newFinalizedIORef (aft r)
            return (r, ref)
        fld <- bet r
        return $ Tuple' ref fld

    step (Tuple' ref (Fold step1 initial1 extract1)) a = do
        s <- initial1
        s1 <- step1 s a `MC.onException` runIORefFinalizer ref
        return $ Tuple' ref $ Fold step1 (return s1) extract1

    extract (Tuple' ref (Fold _ initial1 extract1)) = do
        runIORefFinalizer ref
        initial1 >>= extract1

-- | Run a side effect whenever the fold stops normally, aborts due to an
-- exception or is garbage collected.
--
-- /Internal/
--
{-# INLINE finally #-}
finally :: (MonadAsync m, MonadCatch m) => m b -> Fold m a b -> Fold m a b
finally aft (Fold step1 initial1 extract1) = Fold step initial extract

    where

    initial = do
        ref <- newFinalizedIORef aft
        Tuple' ref <$> initial1

    step (Tuple' ref s) a = do
        s1 <- step1 s a `MC.onException` runIORefFinalizer ref
        return $ Tuple' ref s1

    extract (Tuple' ref s) = do
        runIORefFinalizer ref
        extract1 s

-- | Run a side effect before the fold consumes its first element.
--
-- /Internal/
--
{-# INLINE before #-}
before :: Monad m => m x -> Fold m a b -> Fold m a b
before effect (Fold step1 initial1 extract1) = Fold step1 initial extract1

    where

    initial = effect *> initial1

-- | Run a side effect after the fold stops normally. Please use 'finally' if
-- you need a guarantee that the action runs even if the fold is garbage
-- collected.
--
-- /Internal/
--
{-# INLINE after #-}
after :: Monad m => m x -> Fold m a b -> Fold m a b
after effect = mapM (effect $>)

------------------------------------------------------------------------------
-- Fold to a Parallel SVar
------------------------------------------------------------------------------

{-# INLINE toParallelSVar #-}
toParallelSVar :: MonadIO m => SVar t m a -> Maybe WorkerInfo -> Fold m a ()
toParallelSVar svar winfo = Fold step initial extract
    where

    initial = return ()

    step () x = liftIO $ do
        -- XXX we can have a separate fold for unlimited buffer case to avoid a
        -- branch in the step here.
        decrementBufferLimit svar
        void $ send svar (ChildYield x)

    extract () = liftIO $ do
        sendStop svar winfo

{-# INLINE toParallelSVarLimited #-}
toParallelSVarLimited :: MonadIO m
    => SVar t m a -> Maybe WorkerInfo -> Fold m a ()
toParallelSVarLimited svar winfo = Fold step initial extract
    where

    initial = return True

    step True x = liftIO $ do
        yieldLimitOk <- decrementYieldLimit svar
        if yieldLimitOk
        then do
            decrementBufferLimit svar
            void $ send svar (ChildYield x)
            return True
        else do
            cleanupSVarFromWorker svar
            sendStop svar winfo
            return False
    step False _ = return False

    extract True = liftIO $ sendStop svar winfo
    extract False = return ()
