-- |
-- Module      : Streamly.Internal.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Data.Fold" for an overview.
--
-- IMPORTANT: keep the signatures consistent with the folds in Streamly.Prelude

module Streamly.Internal.Data.Fold
    (
    -- * Fold Type
      Step (..)
    , Fold (..)

    -- * Creating
    , mkAccum
    , mkAccum_
    , mkAccumM
    , mkAccumM_
    , mkFold
    , mkFold_
    , mkFoldM
    , mkFoldM_

    -- * Generators
    , yield
    , yieldM

    -- * Accumulators
    -- ** Semigroups and Monoids
    , sconcat
    , mconcat
    , foldMap
    , foldMapM

    -- ** Reducers
    , drain
    , drainBy
    , drainBy2
    , last
    , length
    , mean
    , variance
    , stdDev
    , rollingHash
    , rollingHashWithSalt
    , rollingHashFirstN
    -- , rollingHashLastN

    -- ** Saturating Reducers
    -- | 'product' terminates if it becomes 0. Other folds can theoretically
    -- saturate on bounded types, and therefore terminate, however, they will
    -- run forever on unbounded types like Integer/Double.
    , sum
    , product
    , maximumBy
    , maximum
    , minimumBy
    , minimum

    -- ** Collectors
    , toList
    , toListRevF  -- experimental
    -- $toListRevF

    -- * Terminating Folds
    , drainN
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
    , all
    , any
    , and
    , or
    -- , the

    -- * Adapting
    , hoist
    , generally

    -- * Running Incrementally
    , initialize
    , runStep

    -- * Output Transformations
    , rsequence
    , sequence
    , rmapM
    , mapM

    -- * Input Transformations

    -- ** Mapping
    , transform
    , map
    --, lsequence
    , lmapM
    -- ** Filtering
    , filter
    , filterM
    -- , ldeleteBy
    -- , luniq
    , lcatMaybes
    , mapMaybe
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

    -- ** Trimming
    , takeLE
    , takeByTime
    -- By elements
    , sliceSepBy
    -- , breakOn
    , sliceSepByMax
    , sliceEndWith
    -- , breakAfter
    {-
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    -- * Splitting

    -- Binary
    -- , tail
    -- , init
    , splitAt -- spanN
    -- , splitIn -- sessionN

    -- XXX To be moved to parsers
    , span  -- spanWhile
    , break -- breakBefore
    -- , breakAround
    , spanBy
    , spanByRolling

    -- By sequences
    -- , breakOnSeq
    -- , breakOnStream -- on a stream

    -- * Distributing

    , tee
    , teeWith
    , teeWithFst
    , teeWithMin
    , distribute
    -- , distributeFst
    -- , distributeMin

    -- * Partitioning

    , partitionByM
    , partitionByFstM
    , partitionByMinM
    , partitionBy
    , partition

    -- * Demultiplexing

    , demux        -- XXX rename this to demux_
    , demuxWith
    , demuxDefault -- XXX rename this to demux
    , demuxDefaultWith
    -- , demuxWithSel
    -- , demuxWithMin

    -- * Classifying

    , classify
    , classifyWith
    -- , classifyWithSel
    -- , classifyWithMin

    -- * Unzipping
    , unzip
    , unzipWith
    , unzipWithM
    , unzipWithFstM
    , unzipWithMinM

    -- * Nesting
    , many
    , intervalsOf
    , chunksOf
    , chunksBetween

    , concatSequence
    , concatMap
    , duplicate
    )
where

import Control.Monad (void, join)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, fromJust)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup((<>)))
#endif
import Streamly.Internal.Data.Either.Strict
    (Either'(..), fromLeft', fromRight', isLeft', isRight')
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Pipe.Types (Pipe (..), PipeState(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.Pipe.Types as Pipe
import qualified Prelude

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith
       , foldl, map, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, (!!)
       , scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip
       , span, splitAt, break, mapM)
import Streamly.Internal.Data.Fold.Types

------------------------------------------------------------------------------
-- hoist
------------------------------------------------------------------------------

-- | Change the underlying monad of a fold
--
-- /Internal/
hoist :: (forall x. m x -> n x) -> Fold m a b -> Fold n a b
hoist f (Fold step initial extract cleanup) =
    Fold (\x a -> f $ step x a) (f initial) (f . extract) (f . cleanup)

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
-- @since 0.8.0
{-# INLINE rsequence #-}
rsequence :: Monad m => Fold m a (m b) -> Fold m a b
rsequence (Fold step initial extract clean) = Fold step' initial1 extract' clean

    where

    eval res =
        case res of
            Partial x -> return $ Partial x
            Done b -> Done <$> b

    initial1 = initial >>= eval

    step' s a = step s a >>= eval

    extract' = join . extract

-- | Flatten the monadic output of a fold to pure output.
--
-- @since 0.7.0
{-# DEPRECATED sequence "Use rsequence instead" #-}
{-# INLINE sequence #-}
sequence :: Monad m => Fold m a (m b) -> Fold m a b
sequence = rsequence

-- | Map a monadic function on the output of a fold.
--
-- @since 0.8.0
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
rmapM f = sequence . fmap f

-- | Map a monadic function on the output of a fold.
--
-- @since 0.7.0
{-# DEPRECATED mapM "Use rmapM instead" #-}
{-# INLINE mapM #-}
mapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
mapM = rmapM

-- | @mapMaybe f fold@ maps a 'Maybe' returning function @f@ on the input of
-- the fold, filters out 'Nothing' elements, and return the values extracted
-- from 'Just'.
--
-- >>> f x = if even x then Just x else Nothing
-- >>> fld = Fold.mapMaybe f Fold.toList
-- >>> Stream.fold fld (Stream.enumerateFromTo 1 10)
-- [2,4,6,8]
--
-- /Internal/
{-# INLINE mapMaybe #-}
mapMaybe :: (Monad m) => (a -> Maybe b) -> Fold m b r -> Fold m a r
mapMaybe f = map f . filter isJust . map fromJust

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
transform (Pipe pstep1 pstep2 pinitial) (Fold fstep finitial fextract fclean) =
    Fold step initial extract clean

    where

    initial = first (Tuple' pinitial) <$> finitial

    step (Tuple' ps fs) x = do
        r <- pstep1 ps x
        go fs r

        where

        -- XXX use SPEC?
        go acc (Pipe.Yield b (Consume ps')) = do
            acc' <- fstep acc b
            return
                $ case acc' of
                      Partial s -> Partial $ Tuple' ps' s
                      Done b2 -> Done b2
        go acc (Pipe.Yield b (Produce ps')) = do
            acc' <- fstep acc b
            r <- pstep2 ps'
            case acc' of
                Partial s -> go s r
                Done b2 -> return $ Done b2
        go acc (Pipe.Continue (Consume ps')) =
            return $ Partial $ Tuple' ps' acc
        go acc (Pipe.Continue (Produce ps')) = do
            r <- pstep2 ps'
            go acc r

    extract (Tuple' _ fs) = fextract fs

    clean (Tuple' _ fs) = fclean fs

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | @_Fold1 step@ returns a new 'Fold' using just a step function that has the
-- same type for the accumulator and the element. The result type is the
-- accumulator type wrapped in 'Maybe'. The initial accumulator is retrieved
-- from the 'Foldable', the result is 'None' for empty containers.
{-# INLINABLE _Fold1 #-}
_Fold1 :: Monad m => (a -> a -> a) -> Fold m a (Maybe a)
_Fold1 step = mkAccum step_ Nothing' toMaybe

    where

    step_ Nothing' a = Just' a
    step_ (Just' x) a = Just' $ step x a

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
drain = mkAccum_ (\_ _ -> ()) ()

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
drainBy f = lmapM f drain

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
last = _Fold1 (\_ x -> x)

------------------------------------------------------------------------------
-- To Summary
------------------------------------------------------------------------------

-- | Like 'length', except with a more general 'Num' return value
--
-- > genericLength = fmap getSum $ foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINE genericLength #-}
genericLength :: (Monad m, Num b) => Fold m a b
genericLength = mkAccum_ (\n _ -> n + 1) 0

-- | Determine the length of the input stream.
--
-- > length = fmap getSum $ foldMap (Sum . const  1)
--
-- @since 0.7.0
{-# INLINE length #-}
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
sum =  mkAccum_ (+) 0

-- | Determine the product of all elements of a stream of numbers. Returns
-- multiplicative identity (@1@) when the stream is empty. The fold terminates
-- when it encounters (@0@) in its input.
--
-- > product = fmap getProduct $ FL.foldMap Product
--
-- @since 0.7.0
-- /Since 0.8.0 (Added 'Eq' constraint)/
{-# INLINE product #-}
product :: (Monad m, Num a, Eq a) => Fold m a a
product =  mkFold_ step (Partial 1)

    where

    step x a =
        if a == 0
        then Done 0
        else Partial $ x * a

------------------------------------------------------------------------------
-- To Summary (Maybe)
------------------------------------------------------------------------------

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- @since 0.7.0
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
maximumBy cmp = _Fold1 max'

    where

    max' x y =
        case cmp x y of
            GT -> x
            _ -> y

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
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Fold m a (Maybe a)
maximum = _Fold1 max

-- | Computes the minimum element with respect to the given comparison function
--
-- @since 0.7.0
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Fold m a (Maybe a)
minimumBy cmp = _Fold1 min'

    where

    min' x y =
        case cmp x y of
            GT -> y
            _ -> x

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
{-# INLINE minimum #-}
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
mean = mkAccum step begin done

    where

    begin = Tuple' 0 0

    step (Tuple' x n) y =
        let n1 = n + 1
         in Tuple' (x + (y - x) / n1) n1

    done (Tuple' x _) = x

-- | Compute a numerically stable (population) variance over all elements in
-- the input stream.
--
-- @since 0.7.0
{-# INLINABLE variance #-}
variance :: (Monad m, Fractional a) => Fold m a a
variance = mkAccum step begin done

    where

    begin = Tuple3' 0 0 0

    step (Tuple3' n mean_ m2) x = Tuple3' n' mean' m2'

        where

        n' = n + 1
        mean' = (n * mean_ + x) / (n + 1)
        delta = x - mean_
        m2' = m2 + delta * delta * n / (n + 1)

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
rollingHashWithSalt = mkAccum_ step

    where

    k = 2891336453 :: Int64

    step cksum a = cksum * k + fromIntegral (fromEnum a)

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
-- > rollingHashFirstN = takeLE n rollingHash
{-# INLINABLE rollingHashFirstN #-}
rollingHashFirstN :: (Monad m, Enum a) => Int -> Fold m a Int64
rollingHashFirstN n = takeLE n rollingHash

------------------------------------------------------------------------------
-- Monoidal left folds
------------------------------------------------------------------------------

-- | Append the elements of an input stream to a provided starting value.
--
-- > S.fold (FL.sconcat 10) (S.map Sum $ S.enumerateFromTo 1 10)
--
-- /Internal/
--
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => a -> Fold m a a
sconcat = mkAccum_ (<>)

-- | Fold an input stream consisting of monoidal elements using 'mappend'
-- and 'mempty'.
--
-- > S.fold FL.mconcat (S.map Sum $ S.enumerateFromTo 1 10)
--
-- @since 0.7.0
{-# INLINE mconcat #-}
mconcat ::
    ( Monad m
#if !MIN_VERSION_base(4,11,0)
    , Semigroup a
#endif
    , Monoid a) => Fold m a a
mconcat = sconcat mempty

-- |
-- > foldMap f = map f mconcat
--
-- Make a fold from a pure function that folds the output of the function
-- using 'mappend' and 'mempty'.
--
-- > S.fold (FL.foldMap Sum) $ S.enumerateFromTo 1 10
--
-- @since 0.7.0
{-# INLINABLE foldMap #-}
foldMap :: (Monad m, Monoid b
#if !MIN_VERSION_base(4,11,0)
    , Semigroup b
#endif
    ) => (a -> b) -> Fold m a b
foldMap f = map f mconcat

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
foldMapM act = mkAccumM_ step (pure mempty)

    where

    step m a = do
        m' <- act a
        return $! mappend m m'

------------------------------------------------------------------------------
-- To Containers
------------------------------------------------------------------------------

-- $toListRevF
-- This is more efficient than 'Streamly.Internal.Data.Fold.toList'. toList is
-- exactly the same as reversing the list after 'toListRevF'.

-- | Buffers the input stream to a list in the reverse order of the input.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0

--  xn : ... : x2 : x1 : []
{-# INLINABLE toListRevF #-}
toListRevF :: Monad m => Fold m a [a]
toListRevF = mkAccum_ (flip (:)) []

------------------------------------------------------------------------------
-- Partial Folds
------------------------------------------------------------------------------

-- | A fold that drains the first n elements of its input, running the effects
-- and discarding the results.
{-# INLINABLE drainN #-}
drainN :: Monad m => Int -> Fold m a ()
drainN n = takeLE n drain

------------------------------------------------------------------------------
-- To Elements
------------------------------------------------------------------------------

-- | Like 'index', except with a more general 'Integral' argument
--
-- @since 0.7.0
{-# INLINABLE genericIndex #-}
genericIndex :: (Integral i, Monad m) => i -> Fold m a (Maybe a)
genericIndex i = mkFold step (Partial 0) (const Nothing)

    where

    step j a =
        if i == j
        then Done $ Just a
        else Partial (j + 1)

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
head = mkFold_ (const (Done . Just)) (Partial Nothing)

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
find predicate = mkFold step (Partial ()) (const Nothing)

    where

    step () a =
        if predicate a
        then Done (Just a)
        else Partial ()

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> find ((==) . fst)
--
-- @since 0.7.0
{-# INLINABLE lookup #-}
lookup :: (Eq a, Monad m) => a -> Fold m (a,b) (Maybe b)
lookup a0 = mkFold step (Partial ()) (const Nothing)

    where

    step () (a, b) =
        if a == a0
        then Done $ Just b
        else Partial ()

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.7.0
{-# INLINABLE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> Fold m a (Maybe Int)
findIndex predicate = mkFold step (Partial 0) (const Nothing)

    where

    step i a =
        if predicate a
        then Done $ Just i
        else Partial (i + 1)

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
null = mkFold (\() _ -> Done False) (Partial ()) (const True)

--
-- > any p = map p or
-- > any p = fmap getAny . FL.foldMap (Any . p)
--
-- | Returns 'True' if any of the elements of a stream satisfies a predicate.
--
-- >>> Stream.fold (Fold.any (== 0)) $ Stream.fromList [1,0,1]
-- > True
--
-- @since 0.7.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Fold m a Bool
any predicate = mkFold_ step initial

    where

    initial = Partial False

    step _ a =
        if predicate a
        then Done True
        else Partial False

-- | Return 'True' if the given element is present in the stream.
--
-- > elem a = any (== a)
--
-- @since 0.7.0
{-# INLINABLE elem #-}
elem :: (Eq a, Monad m) => a -> Fold m a Bool
elem a = any (a ==)

--
-- > all p = map p and
-- > all p = fmap getAll . FL.foldMap (All . p)
--
-- | Returns 'True' if all elements of a stream satisfy a predicate.
--
-- >>> Stream.fold (Fold.all (== 0)) $ Stream.fromList [1,0,1]
-- > False
--
-- @since 0.7.0
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Fold m a Bool
all predicate = mkFold_ step initial

    where

    initial = Partial True

    step _ a =
        if predicate a
        then Partial True
        else Done False

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
{-# INLINE and #-}
and :: Monad m => Fold m Bool Bool
and = all (== True)

-- | Returns 'True' if any element is 'True', 'False' otherwise
--
-- > or = any (== True)
-- > or = fmap getAny . FL.foldMap Any
--
-- @since 0.7.0
{-# INLINE or #-}
or :: Monad m => Fold m Bool Bool
or = any (== True)

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

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
-- > splitAt n f1 f2 = splitWith (,) (takeLE n f1) f2
--
-- /Internal/

{-# INLINE splitAt #-}
splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
splitAt n fld = splitWith (,) (takeLE n fld)

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- Note: Keep this consistent with S.splitOn. In fact we should eliminate
-- S.splitOn in favor of the fold.
--
-- | Consume the input until it encounters an infixed separator element (i.e.
-- when the supplied predicate succeeds), dropping the separator.
--
-- Repeated applications of 'sliceSepBy' splits the stream on separator
-- elements determined by the supplied predicate, separator is considered as
-- infixed between two segments, if one side of the separator is missing then
-- it is parsed as an empty stream.  The supplied 'Fold' is applied on the
-- split segments. With '-' representing non-separator elements and '.' as
-- separator, repeated 'sliceSepBy' splits the stream as follows:
--
-- @
-- "--.--" => "--" "--"
-- "--."   => "--" ""
-- ".--"   => ""   "--"
-- @
--
-- Repeated applications of @Fold.sliceSepBy (== x)@ on the input stream gives us
-- an inverse of @Stream.intercalate (Stream.yield x)@
--
-- > Stream.splitOn pred f = Stream.foldMany (Fold.sliceSepBy pred f)
--
-- Let's use the following definition for illustration:
--
-- > splitOn p = Stream.foldMany (Fold.sliceSepBy pred Fold.toList)
-- > splitOn' p = Stream.toList . splitOn p . Stream.fromList
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
-- Stops - when the predicate succeeds.
--
-- /Internal/
{-# INLINE sliceSepBy #-}
sliceSepBy :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
sliceSepBy predicate (Fold fstep finitial fextract fcleanup) =
    Fold step initial fextract fcleanup

    where

    initial = finitial

    step s a =
        if not (predicate a)
        then fstep s a
        else Done <$> finalExtract fextract fcleanup s

-- | Like 'sliceSepBy' but terminates a parse even before the separator
-- is encountered if its size exceeds the specified maximum limit.
--
-- > take n = PR.sliceSepByMax (const True) n
-- > sliceSepBy p = PR.sliceSepByMax p maxBound
--
-- Let's use the following definitions for illustration:
--
-- > splitOn p n = PR.many FL.toList $ PR.sliceSepByMax p n (FL.toList)
-- > splitOn' p n = S.parse (splitOn p n) . S.fromList
--
-- >>> splitOn' (== '.') 0 ""
-- [""]
--
-- >>> splitOn' (== '.') 0 "a"
-- infinite list of empty strings
--
-- >>> splitOn' (== '.') 3 "hello.world"
-- ["hel","lo","wor","ld"]
--
-- If the separator is found and the limit is reached at the same time then it
-- behaves just like 'sliceSepBy' i.e. the separator is dropped.
--
-- >>> splitOn' (== '.') 0 "."
-- ["",""]
--
-- >>> splitOn' (== '.') 0 ".."
-- ["","",""]
--
-- Stops - when the predicate succeeds or the limit is reached.
--
-- /Internal/
{-# INLINABLE sliceSepByMax #-}
sliceSepByMax :: Monad m
    => (a -> Bool) -> Int -> Fold m a b -> Fold m a b
sliceSepByMax p n = sliceSepBy p . takeLE n

-- | Collect stream elements until an element succeeds the predicate. Also take
-- the element on which the predicate succeeded. The succeeding element is
-- treated as a suffix separator which is kept in the output segement.
--
-- * Stops - when the predicate succeeds.
--
-- > Stream.splitWithSuffix pred f = Stream.foldMany (Fold.sliceEndWith pred f)
--
-- /Internal/
--
{-# INLINE sliceEndWith #-}
sliceEndWith :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
sliceEndWith predicate (Fold fstep finitial fextract fcleanup) =
    Fold step initial fextract fcleanup

    where

    initial = finitial

    step s a = do
        res <- fstep s a
        if not (predicate a)
        then return res
        else do
            case res of
                Partial s1 -> Done <$> finalExtract fextract fcleanup s1
                Done b -> return $ Done b

data SpanByState a bl fl fr
    = SpanByLeft0 !fl
    | SpanByLeft !a !fl
    | SpanByRight !bl !fr

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
-- /Internal/
--
{-# INLINE spanBy #-}
spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
spanBy cmp (Fold stepL initialL extractL _)
           (Fold stepR initialR extractR _) =
    Fold step initial extract clean

    where

    -- XXX This combinator will be removed in the future.
    clean = undefined

    initial = do
        resL <- initialL
        case resL of
            Partial fl -> return $ Partial $ SpanByLeft0 fl
            Done bl -> do
                resR <- initialR
                return
                    $ case resR of
                          Partial fr -> Partial $ SpanByRight bl fr
                          Done br -> Done (bl, br)

    step (SpanByLeft0 fl) input = do
        sfl <- stepL fl input
        case sfl of
            Partial fl1 -> return $ Partial $ SpanByLeft input fl1
            Done bl -> do
                resR <- initialR
                return
                    $ case resR of
                          Partial fr -> Partial $ SpanByRight bl fr
                          Done br -> Done (bl, br)
    step (SpanByLeft frst fl) input =
        if cmp frst input
        then do
            sfl <- stepL fl input
            case sfl of
                Partial fl1 -> return $ Partial $ SpanByLeft frst fl1
                Done bl -> do
                    resR <- initialR
                    return
                        $ case resR of
                              Partial fr -> Partial $ SpanByRight bl fr
                              Done br -> Done (bl, br)
        else do
            bl <- extractL fl
            resR <- initialR
            case resR of
                Partial fr -> do
                    sfr <- stepR fr input
                    return
                        $ case sfr of
                              Partial fr1 -> Partial $ SpanByRight bl fr1
                              Done br -> Done (bl, br)
                Done br -> return $ Done (bl, br)
    step (SpanByRight bl fr) input = do
        sfr <- stepR fr input
        return
            $ case sfr of
                  Partial fr1 -> Partial $ SpanByRight bl fr1
                  Done br -> Done (bl, br)

    extract (SpanByLeft0 fl) = do
        bl <- extractL fl
        resR <- initialR
        case resR of
            Partial fr -> (bl, ) <$> extractR fr
            Done br -> return (bl, br)
    extract (SpanByLeft _ fl) = do
        bl <- extractL fl
        resR <- initialR
        case resR of
            Partial fr -> (bl, ) <$> extractR fr
            Done br -> return (bl, br)
    extract (SpanByRight bl fr) = (bl, ) <$> extractR fr

data SpanState bl fl fr
    = SpanLeft !fl
    | SpanRight !bl !fr

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- > let span_ p xs = S.fold (FL.span p FL.toList FL.toList) $ S.fromList xs
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
span p (Fold stepL initialL extractL _) (Fold stepR initialR extractR _) =
    Fold step initial extract clean

    where

    -- XXX This combinator will be removed in the future.
    clean = undefined

    initial = do
        resL <- initialL
        case resL of
            Partial fl -> return $ Partial $ SpanLeft fl
            Done bl -> do
                resR <- initialR
                return
                    $ case resR of
                          Partial fr -> Partial $ SpanRight bl fr
                          Done br -> Done (bl, br)

    step (SpanLeft fl) input =
        if p input
        then do
            sfl <- stepL fl input
            case sfl of
                Partial fl1 -> return $ Partial $ SpanLeft fl1
                Done bl -> do
                    resR <- initialR
                    return
                        $ case resR of
                              Partial fr -> Partial $ SpanRight bl fr
                              Done br -> Done (bl, br)
        else do
            bl <- extractL fl
            resR <- initialR
            case resR of
                Partial fr -> do
                    sfr <- stepR fr input
                    return
                        $ case sfr of
                              Partial fr1 -> Partial $ SpanRight bl fr1
                              Done br -> Done (bl, br)
                Done br -> return $ Done (bl, br)
    step (SpanRight bl fr) input = do
        sfr <- stepR fr input
        return
            $ case sfr of
                  Partial fr1 -> Partial $ SpanRight bl fr1
                  Done br -> Done (bl, br)

    extract (SpanLeft fl) = do
        bl <- extractL fl
        resR <- initialR
        case resR of
            Partial fr -> (bl, ) <$> extractR fr
            Done br -> return (bl, br)
    extract (SpanRight bl fr) = (bl, ) <$> extractR fr

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
spanByRolling cmp (Fold stepL initialL extractL _) (Fold stepR initialR extractR _) =
    Fold step initial extract clean

    where

    -- XXX This combinator will be removed in the future.
    clean = undefined

    initial = do
        resL <- initialL
        case resL of
            Partial fl -> return $ Partial $ SpanByLeft0 fl
            Done bl -> do
                resR <- initialR
                return
                    $ case resR of
                          Partial fr -> Partial $ SpanByRight bl fr
                          Done br -> Done (bl, br)

    step (SpanByLeft0 fl) input = do
        sfl <- stepL fl input
        case sfl of
            Partial fl1 -> return $ Partial $ SpanByLeft input fl1
            Done bl -> do
                resR <- initialR
                return
                    $ case resR of
                          Partial fr -> Partial $ SpanByRight bl fr
                          Done br -> Done (bl, br)
    step (SpanByLeft input0 fl) input =
        if cmp input0 input
        then do
            sfl <- stepL fl input
            case sfl of
                Partial fl1 -> return $ Partial $ SpanByLeft input fl1
                Done bl -> do
                    resR <- initialR
                    return
                        $ case resR of
                              Partial fr -> Partial $ SpanByRight bl fr
                              Done br -> Done (bl, br)
        else do
            bl <- extractL fl
            resR <- initialR
            case resR of
                Partial fr -> do
                    sfr <- stepR fr input
                    return
                        $ case sfr of
                              Partial fr1 -> Partial $ SpanByRight bl fr1
                              Done br -> Done (bl, br)
                Done br -> return $ Done (bl, br)
    step (SpanByRight bl fr) input = do
        sfr <- stepR fr input
        return
            $ case sfr of
                  Partial fr1 -> Partial $ SpanByRight bl fr1
                  Done br -> Done (bl, br)

    extract (SpanByLeft0 fl) = do
        bl <- extractL fl
        resR <- initialR
        case resR of
            Partial fr -> (bl, ) <$> extractR fr
            Done br -> return (bl, br)
    extract (SpanByLeft _ fl) = do
        bl <- extractL fl
        resR <- initialR
        case resR of
            Partial fr -> (bl, ) <$> extractR fr
            Done br -> return (bl, br)
    extract (SpanByRight bl fr) = (bl, ) <$> extractR fr

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
tee = teeWith (,)

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
-- Stops when all the folds stop.
--
-- @since 0.7.0
{-# INLINE distribute #-}
distribute :: Monad m => [Fold m a b] -> Fold m a [b]
distribute = foldr (teeWith (:)) (yield [])

------------------------------------------------------------------------------
-- Partitioning
------------------------------------------------------------------------------

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
-- When one fold is done, any input meant for it is ignored until the other
-- fold is also done.
--
-- Stops when both the folds stop.
--
-- /See also: 'partitionByFstM' and 'partitionByMinM'./
--
-- @since 0.7.0
{-# INLINE partitionByM #-}
partitionByM :: Monad m
    => (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByM f (Fold stepL beginL doneL cleanL)
               (Fold stepR beginR doneR cleanR) =
    Fold step begin done clean

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return
            $ case resL of
                  Partial sL ->
                      Partial
                          $ case resR of
                                Partial sR -> RunBoth sL sR
                                Done bR -> RunLeft sL bR
                  Done bL ->
                      case resR of
                          Partial sR -> Partial $ RunRight bL sR
                          Done bR -> Done (bL, bR)

    step (RunBoth sL sR) a = do
        r <- f a
        case r of
            Left b -> do
                res <- stepL sL b
                return
                  $ Partial
                  $ case res of
                        Partial sres -> RunBoth sres sR
                        Done bres -> RunRight bres sR
            Right c -> do
                res <- stepR sR c
                return
                  $ Partial
                  $ case res of
                        Partial sres -> RunBoth sL sres
                        Done bres -> RunLeft sL bres
    step (RunLeft sL bR) a = do
        r <- f a
        case r of
            Left b -> do
                res <- stepL sL b
                return
                  $ case res of
                        Partial sres -> Partial $ RunLeft sres bR
                        Done bres -> Done (bres, bR)
            Right _ -> return $ Partial $ RunLeft sL bR
    step (RunRight bL sR) a = do
        r <- f a
        case r of
            Left _ -> return $ Partial $ RunRight bL sR
            Right c -> do
                res <- stepR sR c
                return
                  $ case res of
                        Partial sres -> Partial $ RunRight bL sres
                        Done bres -> Done (bL, bres)

    done (RunBoth sL sR) = (,) <$> doneL sL <*> doneR sR
    done (RunLeft sL bR) = (,bR) <$> doneL sL
    done (RunRight bL sR) = (bL,) <$> doneR sR

    clean (RunBoth sL sR) = cleanL sL >> cleanR sR
    clean (RunLeft sL _) = cleanL sL
    clean (RunRight _ sR) = cleanR sR

-- | Similar to 'partitionByM' but terminates when the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE partitionByFstM #-}
partitionByFstM :: -- Monad m =>
       (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByFstM = undefined

-- | Similar to 'partitionByM' but terminates when any fold terminates.
--
-- /Unimplemented/
--
{-# INLINE partitionByMinM #-}
partitionByMinM :: -- Monad m =>
       (a -> m (Either b c)) -> Fold m b x -> Fold m c y -> Fold m a (x, y)
partitionByMinM = undefined

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
-- Any input that does not map to a fold in the input Map is silently ignored.
--
-- > demuxWith f kv = fmap fst $ demuxDefaultWith f kv drain
--
-- /Internal/
--
{-# INLINE demuxWith #-}
demuxWith :: (Monad m, Ord k)
    => (a -> (k, a')) -> Map k (Fold m a' b) -> Fold m a (Map k b)
demuxWith f kv = fmap fst $ demuxDefaultWith f kv drain

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

data DemuxState s b doneMap runMap =
      DemuxMapAndDefault !s !doneMap !runMap
    | DemuxOnlyMap b !doneMap !runMap
    | DemuxOnlyDefault s !doneMap

-- | Like 'demuxWith' but uses a default catchall fold to handle inputs which
-- do not have a specific fold in the map to handle them.
--
-- If any fold in the map stops, inputs meant for that fold are sent to the
-- catchall fold. If the catchall fold stops then inputs that do not match any
-- fold are ignored.
--
-- Stops when all the folds, including the catchall fold, stop.
--
-- /Internal/
--
{-# INLINE demuxDefaultWith #-}
demuxDefaultWith :: (Monad m, Ord k)
    => (a -> (k, a'))
    -> Map k (Fold m a' b)
    -> Fold m (k, a') c
    -> Fold m a (Map k b, c)
demuxDefaultWith f kv (Fold dstep dinitial dextract dclean) =
    Fold step initial extract clean

    where

    initial = do
        let runInit (Fold step1 initial1 done1 clean1) = do
                r <- initial1
                return
                    $ case r of
                          Partial _ ->
                              Right' (Fold step1 (return r) done1 clean1)
                          Done b -> Left' b

        -- initialize folds in the kv map and separate the ones that are done
        -- from running ones
        kv1 <- Prelude.mapM runInit kv
        let runMap = Map.map fromRight' $ Map.filter isRight' kv1
            doneMap = Map.map fromLeft' $ Map.filter isLeft' kv1

        -- Run the default fold, and decide the next state based on its result
        dres <- dinitial
        return
            $ case dres of
                  Partial s ->
                      Partial
                          $ if Map.size runMap > 0
                            then DemuxMapAndDefault s doneMap runMap
                            else DemuxOnlyDefault s doneMap
                  Done b ->
                      if Map.size runMap > 0
                      then Partial $ DemuxOnlyMap b doneMap runMap
                      else Done (doneMap, b)

    {-# INLINE runFold #-}
    runFold fPartial fDone doneMap runMap
                (Fold step1 initial1 done1 clean1) k a1 = do
        resi <- initial1
        case resi of
            Partial st -> do
                res <- step1 st a1
                return $ case res of
                    Partial s ->
                        let fld = Fold step1 (return $ Partial s) done1 clean1
                            runMap1 = Map.insert k fld runMap
                         in Partial $ fPartial doneMap runMap1
                    Done b -> do
                        let runMap1 = Map.delete k runMap
                            doneMap1 = Map.insert k b doneMap
                        if Map.size runMap1 == 0
                        then fDone doneMap1
                        else Partial $ fPartial doneMap1 runMap1
            Done _ -> error "Bug: demuxDefaultWith: Done fold"

    step (DemuxMapAndDefault dacc doneMap runMap) a = do
        let (k, a1) = f a
        case Map.lookup k runMap of
            Nothing -> do
                res <- dstep dacc (k, a1)
                return
                    $ Partial
                    $ case res of
                          Partial s -> DemuxMapAndDefault s doneMap runMap
                          Done b -> DemuxOnlyMap b doneMap runMap
            Just fld ->
                runFold
                    (DemuxMapAndDefault dacc)
                    (Partial . DemuxOnlyDefault dacc)
                    doneMap runMap fld k a1

    step (DemuxOnlyMap dval doneMap runMap) a = do
        let (k, a1) = f a
        case Map.lookup k runMap of
            Nothing -> return $ Partial $ DemuxOnlyMap dval doneMap runMap
            Just fld ->
                runFold
                    (DemuxOnlyMap dval)
                    (Done . (, dval))
                    doneMap runMap fld k a1
    step (DemuxOnlyDefault dacc doneMap) a = do
        let (k, a1) = f a
        res <- dstep dacc (k, a1)
        return
            $ case res of
                  Partial s -> Partial $ DemuxOnlyDefault s doneMap
                  Done b -> Done (doneMap, b)

    runExtract (Fold _ initial1 done1 _) = do
        res <- initial1
        case res of
            Partial s -> done1 s
            Done b -> return b

    extract (DemuxMapAndDefault dacc doneMap runMap) = do
        b <- dextract dacc
        runMap1 <- Prelude.mapM runExtract runMap
        return (doneMap `Map.union` runMap1, b)
    extract (DemuxOnlyMap dval doneMap runMap) = do
        runMap1 <- Prelude.mapM runExtract runMap
        return (doneMap `Map.union` runMap1, dval)
    extract (DemuxOnlyDefault dacc doneMap) = do
        b <- dextract dacc
        return (doneMap, b)

    runClean (Fold _ initial1 _ clean1) = do
        res <- initial1
        case res of
            Partial s -> clean1 s
            Done _ -> return ()

    clean (DemuxMapAndDefault dacc _ runMap) =
        dclean dacc >> Prelude.mapM_ runClean runMap
    clean (DemuxOnlyMap _ _ runMap) = Prelude.mapM_ runClean runMap
    clean (DemuxOnlyDefault dacc _) = dclean dacc

{-# INLINE demuxDefault #-}
demuxDefault :: (Monad m, Ord k)
    => Map k (Fold m a b) -> Fold m (k, a) b -> Fold m (k, a) (Map k b, b)
demuxDefault = demuxDefaultWith id

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
-- If the classifier fold stops for a particular key any further inputs in that
-- bucket are ignored.
--
-- /Stops: never/
--
-- /Internal/
--
{-# INLINE classifyWith #-}
classifyWith :: (Monad m, Ord k) => (a -> k) -> Fold m a b -> Fold m a (Map k b)
classifyWith f (Fold step1 initial1 extract1 clean1) =
    Fold step initial extract clean

    where

    initial = return $ Partial Map.empty

    step kv a =
        case Map.lookup k kv of
            Nothing -> do
                x <- initial1
                case x of
                      Partial s -> do
                        r <- step1 s a
                        return
                            $ Partial
                            $ flip (Map.insert k) kv
                            $ case r of
                                  Partial s1 -> Left' s1
                                  Done b -> Right' b
                      Done b -> return $ Partial $ Map.insert k (Right' b) kv
            Just x -> do
                case x of
                    Left' s -> do
                        r <- step1 s a
                        return
                            $ Partial
                            $ flip (Map.insert k) kv
                            $ case r of
                                  Partial s1 -> Left' s1
                                  Done b -> Right' b
                    Right' _ -> return $ Partial kv

        where

        k = f a

    extract =
        Prelude.mapM
            (\case
                 Left' s -> extract1 s
                 Right' b -> return b)

    clean =
        Prelude.mapM_
            (\case
                 Left' s -> clean1 s
                 Right' _ -> return ())

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
-- > classify fld = classifyWith fst (map snd fld)
--
{-# INLINE classify #-}
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify fld = classifyWith fst (map snd fld)

------------------------------------------------------------------------------
-- Unzipping
------------------------------------------------------------------------------

-- | Like 'unzipWith' but with a monadic splitter function.
--
-- -- @unzipWithM k f1 f2 = lmapM k (unzip f1 f2)@
--
-- @since 0.7.0
{-# INLINE unzipWithM #-}
unzipWithM :: Monad m
    => (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithM f (Fold stepL beginL doneL cleanL) (Fold stepR beginR doneR cleanR) =
    Fold step begin done clean

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return
            $ case resL of
                  Partial sL ->
                      Partial
                          $ case resR of
                                Partial sR -> RunBoth sL sR
                                Done bR -> RunLeft sL bR
                  Done bL ->
                      case resR of
                          Partial sR -> Partial $ RunRight bL sR
                          Done bR -> Done (bL, bR)

    step (RunBoth sL sR) a = do
        (b, c) <- f a
        resL <- stepL sL b
        resR <- stepR sR c
        case resL of
            Partial sresL ->
                return
                    $ Partial
                    $ case resR of
                          Partial sresR -> RunBoth sresL sresR
                          Done bresR -> RunLeft sresL bresR
            Done bresL ->
                return
                    $ case resR of
                          Partial sresR -> Partial $ RunRight bresL sresR
                          Done bresR -> Done (bresL, bresR)
    step (RunLeft sL bR) a = do
        (b, _) <- f a
        resL <- stepL sL b
        return
            $ case resL of
                  Partial sresL -> Partial $ RunLeft sresL bR
                  Done bresL -> Done (bresL, bR)
    step (RunRight bL sR) a = do
        (_, c) <- f a
        resR <- stepR sR c
        return
            $ case resR of
                  Partial sresR -> Partial $ RunRight bL sresR
                  Done bresR -> Done (bL, bresR)

    done (RunBoth sL sR) = (,) <$> doneL sL <*> doneR sR
    done (RunLeft sL bR) = (,bR) <$> doneL sL
    done (RunRight bL sR) = (bL,) <$> doneR sR

    clean (RunBoth sL sR) = cleanL sL >> cleanR sR
    clean (RunLeft sL _) = cleanL sL
    clean (RunRight _ sR) = cleanR sR

-- | Similar to 'unzipWithM' but terminates when the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE unzipWithFstM #-}
unzipWithFstM :: -- Monad m =>
     (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithFstM = undefined

-- | Similar to 'unzipWithM' but terminates when any fold terminates.
--
-- /Unimplemented/
--
{-# INLINE unzipWithMinM #-}
unzipWithMinM :: -- Monad m =>
     (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
unzipWithMinM = undefined

-- | Split elements in the input stream into two parts using a pure splitter
-- function, direct each part to a different fold and zip the results.
--
-- @unzipWith f fld1 fld2 = map f (unzip fld1 fld2)@
--
-- This fold terminates when both the input folds terminate.
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

-- | @concatSequence f t@ applies folds from stream @t@ sequentially and
-- collects the results using the fold @f@.
--
-- /Unimplemented/
--
{-# INLINE concatSequence #-}
concatSequence ::
    -- IsStream t =>
    Fold m b c -> t (Fold m a b) -> Fold m a c
concatSequence _f _p = undefined

-- | Group the input stream into groups of elements between @low@ and @high@.
-- Collection starts in chunks of @low@ and then keeps doubling until we reach
-- @high@. Each chunk is folded using the provided fold function.
--
-- This could be useful, for example, when we are folding a stream of unknown
-- size to a stream of arrays and we want to minimize the number of
-- allocations.
--
-- NOTE: this would be an application of "many" using a terminating fold.
--
-- /Unimplemented/
--
{-# INLINE chunksBetween #-}
chunksBetween :: -- Monad m =>
       Int -> Int -> Fold m a b -> Fold m b c -> Fold m a c
chunksBetween _low _high _f1 _f2 = undefined
