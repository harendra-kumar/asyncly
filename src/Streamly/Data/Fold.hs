-- |
-- Module      : Streamly.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Fold' is an abstraction that can act as a consumer for a stream of values.
--
-- A fold driver e.g. 'Streamly.Prelude.fold' pushes values from a stream to the
-- 'Fold' one at a time, reducing the stream to a single value.
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Prelude as Stream
--
-- For example, a 'sum' Fold represents adding the input to the accumulated sum.
--
-- >>> Stream.fold Fold.sum $ Stream.fromList [1..100]
-- 5050
--
-- Conceptually, a 'Fold' is a data type that can mimic a strict left fold
-- ('Data.List.foldl') as well as lazy right fold ('Prelude.foldr').  The above
-- example is similar to a left fold using @(+)@ as the step and @0@ as the
-- initial value of the accumulator:
--
-- >>> Data.List.foldl' (+) 0 [1..100]
-- 5050
--
-- 'Fold's are non-greedy consumers and have the have the capability to
-- terminate early. Being non-greedy, they only consume the part of the input
-- stream that is required.
--
-- For e.g. the 'head' fold consumes only 1 element in the input stream and
-- terminates.
--
-- >>> Stream.fold Fold.head $ Stream.fromList [1..]
-- Just 1
--
-- 'Fold's can be combined together using combinators. For example, to create a
-- fold that sums first two elements in a stream:
--
-- >>> sumTwo = Fold.take 2 Fold.sum
-- >>> Stream.fold sumTwo $ Stream.fromList [1..100]
-- 3
--
-- In the above example, since the fold needs only 2 elements from the input
-- stream, it only consumes two.
--
-- Folds can be combined to run in parallel on the same input. For example, to
-- compute the average of numbers in a stream without going through the stream
-- twice:
--
-- >>> avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> Stream.fold avg $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- Folds can be combined so as to partition the input stream over multiple
-- folds. For example, to count even and odd numbers in a stream:
--
-- >>> split n = if even n then Left n else Right n
-- >>> stream = Stream.map split $ Stream.fromList [1..100]
-- >>> countEven = fmap (("Even " ++) . show) Fold.length
-- >>> countOdd = fmap (("Odd "  ++) . show) Fold.length
-- >>> f = Fold.partition countEven countOdd
-- >>> Stream.fold f stream
-- ("Even 50","Odd 50")
--
-- Folds can be combined to parse the stream serially such that the first fold
-- consumes the input until it terminates and the second fold consumes the rest
-- of the input until it terminates:
--
-- >>> f = Fold.serialWith (,) (Fold.take 8 Fold.toList) (Fold.takeEndBy (== '\n') Fold.toList)
-- >>> Stream.fold f $ Stream.fromList "header: hello\n"
-- ("header: ","hello\n")
--
-- Please note that if the first fold is a non-terminating fold like 'sum', it
-- will consume the entire input stream.
--
-- A 'Fold' can be applied repeatedly on a stream to transform it to a stream
-- of fold results. To split a stream on newlines:
--
-- >>> f = Fold.takeEndBy (== '\n') Fold.toList
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList "Hello there!\nHow are you\n"
-- ["Hello there!\n","How are you\n"]
--
-- Similarly, we can split the input of a fold too:
--
-- >>> Stream.fold (Fold.many f Fold.toList) $ Stream.fromList "Hello there!\nHow are you\n"
-- ["Hello there!\n","How are you\n"]
--
-- Please see "Streamly.Internal.Data.Fold" for additional @Pre-release@
-- functions.
--
-- = Folds vs. Streams
--
-- We can often use streams or folds to achieve the same goal. However, streams
-- allow efficient composition of producers (e.g. 'Streamly.Prelude.serial' or
-- 'Streamly.Prelude.mergeBy') whereas folds allow efficient composition of
-- consumers (e.g.  'serialWith', 'partition' or 'teeWith').
--
-- Streams are producers, transformations on streams happen on the output side:
--
-- >>> f = Stream.sum . Stream.map (+1) . Stream.filter odd
-- >>> f $ Stream.fromList [1..100]
-- 2550
--
-- Folds are stream consumers with an input stream and an output value, stream
-- transformations on folds happen on the input side:
--
-- >>> f = Fold.filter odd $ Fold.lmap (+1) $ Fold.sum
-- >>> Stream.fold f $ Stream.fromList [1..100]
-- 2550
--
-- Notice the composition by @.@ vs @$@ and the order of operations in the
-- above examples, the difference is due to output vs input side
-- transformations.

module Streamly.Data.Fold
    (
    -- * Fold Type

      Fold -- (..)

    -- * Constructors
    , mkFoldl
    , mkFoldlM
    , mkFoldr

    -- * Folds
    -- ** Accumulators
    -- | Folds that never terminate, these folds are much like strict left
    -- folds. 'mconcat' is the fundamental accumulator.  All other accumulators
    -- can be expressed in terms of 'mconcat' using a suitable Monoid.  Instead
    -- of writing folds we could write Monoids and turn them into folds.

    -- Monoids
    , sconcat
    , mconcat
    , foldMap
    , foldMapM

    -- Reducers
    , drain
    , drainBy
    , last
    , length
    , sum
    , product
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , mean
    , variance
    , stdDev


    -- Collectors
    , toList
    , toListRev

    -- ** Terminating Folds
    -- | These are much like lazy right folds.

    , index
    , head
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

    -- * Combinators
    -- | Combinators are modifiers of folds.  In the type @Fold m a b@, @a@ is
    -- the input type and @b@ is the output type.  Transformations can be
    -- applied either on the input side or on the output side.  Therefore,
    -- combinators are of one of the following general shapes:
    --
    -- * @... -> Fold m a b -> Fold m c b@ (input transformation)
    -- * @... -> Fold m a b -> Fold m a c@ (output transformation)
    --
    -- Output transformations are also known as covariant transformations, and
    -- input transformations are also known as contravariant transformations.
    -- The input side transformations are more interesting for folds.  Most of
    -- the following sections describe the input transformation operations on a
    -- fold.  The names and signatures of the operations are consistent with
    -- corresponding operations in "Streamly.Prelude". When an operation makes
    -- sense on both input and output side we use the prefix @l@ (for left) for
    -- input side operations and the prefix @r@ (for right) for output side
    -- operations.

    -- ** Mapping on output
    -- | The 'Functor' instance of a fold maps on the output of the fold:
    --
    -- >>> Stream.fold (fmap show Fold.sum) (Stream.enumerateFromTo 1 100)
    -- "5050"
    --
    , rmapM

    -- ** Mapping on Input
    , lmap
    , lmapM

    -- ** Filtering
    , filter
    , filterM

    -- -- ** Mapping Filters
    , catMaybes
    , mapMaybe

    -- ** Trimming
    , take
    -- , takeInterval
    , takeEndBy_
    , takeEndBy

    -- ** Serial Append
    , serialWith

    -- ** Parallel Distribution
    -- | For applicative composition using distribution see
    -- "Streamly.Internal.Data.Fold.Tee".

    , teeWith
    , tee
    , distribute

    -- ** Partitioning
    -- | Direct items in the input stream to different folds using a binary
    -- fold selector.

    , partition

    -- ** Unzipping
    , unzip

    -- ** Splitting
    , many
    , chunksOf
    -- , intervalsOf

    -- ** Nesting
    , concatMap

    -- * Deprecated
    , sequence
    , mapM
    )
where

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM)

import Streamly.Internal.Data.Fold
