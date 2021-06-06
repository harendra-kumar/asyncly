-- |
-- Module      : Streamly.Internal.Data.Array.Stream.Fold.Foreign
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fold a stream of foreign arrays.  @Fold m a b@ in this module works
-- on a stream of "Array a" and produces an output of type @b@.
--
-- Though @Fold m a b@ in this module works on a stream of @Array a@ it is
-- different from @Data.Fold m (Array a) b@.  While the latter works on arrays
-- as a whole treating them as atomic elements, the folds in this module can
-- work on the stream of arrays as if it is an element stream with all the
-- arrays coalesced together. This module allows adapting the element stream
-- folds in Data.Fold to correctly work on an array stream as if it is an
-- element stream. For example:
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
-- >>> import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as ArrayFold
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (arraysOf)
-- >>> import qualified Streamly.Prelude as Stream
--
-- >>> ArrayStream.foldArr (ArrayFold.fromFold (Fold.take 7 Fold.toList)) $ Stream.arraysOf 5 $ Stream.fromList "hello world"
-- "hello w"
--
module Streamly.Internal.Data.Array.Stream.Fold.Foreign
    (
      Fold (..)

    -- * Construction
    , fromFold
    , fromParser
    , fromArrayFold

    -- * Mapping
    , rmapM

    -- * Applicative
    , fromPure
    , fromEffect
    , serialWith

    -- * Monad
    , concatMap

    -- * Transformation
    , take
    )
where

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Foreign.Type (Array(..))
import Streamly.Internal.Data.Parser.ParserD (Initial(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Parser.ParserD.Type as ParserD

import Prelude hiding (concatMap, take)

-- | Array stream fold.
--
-- An array stream fold is basically an array stream "Parser" that does not
-- fail.  In case of array stream folds the count in 'Partial', 'Continue' and
-- 'Done' is a count of elements that includes the leftover element count in
-- the array that is currently being processed by the parser. If none of the
-- elements is consumed by the parser the count is at least the whole array
-- length. If the whole array is consumed by the parser then the count will be
-- 0.
--
-- /Pre-release/
--
newtype Fold m a b = Fold (ParserD.Parser m (Array a) b)

-------------------------------------------------------------------------------
-- Constructing array stream folds from element folds and parsers
-------------------------------------------------------------------------------

-- | Convert an element 'Fold' into an array stream fold.
--
-- /Pre-release/
{-# INLINE fromFold #-}
fromFold :: forall m a b. (MonadIO m, Storable a) =>
    Fold.Fold m a b -> Fold m a b
fromFold (Fold.Fold fstep finitial fextract) =
    Fold (ParserD.Parser step initial fextract)

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  Fold.Partial s1 -> IPartial s1
                  Fold.Done b -> IDone b

    step s (Array fp@(ForeignPtr start _) end) = do
        goArray SPEC (Ptr start) s

        where

        goArray !_ !cur !fs | cur >= end = do
            assert (cur == end) (return ())
            liftIO $ touchForeignPtr fp
            return $ Partial 0 fs
        goArray !_ !cur !fs = do
            x <- liftIO $ peek cur
            res <- fstep fs x
            let elemSize = sizeOf (undefined :: a)
                next = cur `plusPtr` elemSize
            case res of
                Fold.Done b ->
                    return $ Done ((end `minusPtr` next) `div` elemSize) b
                Fold.Partial fs1 ->
                    goArray SPEC next fs1

-- | Convert an element 'Parser' into an array stream fold. If the parser fails
-- the fold would throw an exception.
--
-- /Pre-release/
{-# INLINE fromParser #-}
fromParser :: forall m a b. (MonadIO m, Storable a) =>
    ParserD.Parser m a b -> Fold m a b
fromParser (ParserD.Parser step1 initial1 extract1) =
    Fold (ParserD.Parser step initial1 extract1)

    where

    step s (Array fp@(ForeignPtr start _) end) = do
        if Ptr start >= end
        then return $ Continue 0 s
        else goArray SPEC (Ptr start) s

        where

        {-# INLINE partial #-}
        partial arrRem cur next elemSize st n fs1 = do
            let next1 = next `plusPtr` negate (n * elemSize)
            if next1 >= Ptr start && cur < end
            then goArray SPEC next1 fs1
            else return $ st (arrRem + n) fs1

        goArray !_ !cur !fs = do
            x <- liftIO $ peek cur
            liftIO $ touchForeignPtr fp
            res <- step1 fs x
            let elemSize = sizeOf (undefined :: a)
                next = cur `plusPtr` elemSize
                arrRem = (end `minusPtr` next) `div` elemSize
            case res of
                ParserD.Done n b -> do
                    return $ Done (arrRem + n) b
                ParserD.Partial n fs1 ->
                    partial arrRem cur next elemSize Partial n fs1
                ParserD.Continue n fs1 -> do
                    partial arrRem cur next elemSize Continue n fs1
                Error err -> return $ Error err

-- | Adapt an array stream fold.
--
-- /Pre-release/
{-# INLINE fromArrayFold #-}
fromArrayFold :: forall m a b. (MonadIO m) =>
    Fold.Fold m (Array a) b -> Fold m a b
fromArrayFold f = Fold $ ParserD.fromFold f

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- | Maps a function over the result of fold.
--
-- /Pre-release/
instance Functor m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold p) = Fold $ fmap f p

-- | Map a monadic function on the output of a fold.
--
-- /Pre-release/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
rmapM f (Fold p) = Fold $ ParserD.rmapM f p

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- | A fold that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: Monad m => b -> Fold m a b
fromPure = Fold . ParserD.fromPure

-- | A fold that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Fold m a b
fromEffect = Fold . ParserD.fromEffect

-- | Applies two folds sequentially on the input stream and combines their
-- results using the supplied function.
--
-- /Pre-release/
{-# INLINE serial_ #-}
serial_ :: MonadThrow m => Fold m x a -> Fold m x b -> Fold m x b
serial_ (Fold p1) (Fold p2) = Fold $ ParserD.noErrorUnsafeSplit_ p1 p2

-- | Applies two folds sequentially on the input stream and combines their
-- results using the supplied function.
--
-- /Pre-release/
{-# INLINE serialWith #-}
serialWith :: MonadThrow m
    => (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
serialWith f (Fold p1) (Fold p2) =
    Fold $ ParserD.noErrorUnsafeSplitWith f p1 p2

-- | 'Applicative' form of 'serialWith'.
-- > (<*>) = serialWith id
instance MonadThrow m => Applicative (Fold m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = serialWith id

    {-# INLINE (*>) #-}
    (*>) = serial_

#if MIN_VERSION_base(4,10,0)
    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)
#endif

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- | Applies a fold on the input stream, generates the next fold from the
-- output of the previously applied fold and then applies that fold.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: MonadThrow m =>
    (b -> Fold m a c) -> Fold m a b -> Fold m a c
concatMap func (Fold p) =
    Fold $ ParserD.noErrorUnsafeConcatMap (\x -> let Fold y = func x in y) p

-- | Monad instance applies folds sequentially. Next fold can depend on the
-- output of the previous fold. See 'concatMap'.
--
-- > (>>=) = flip concatMap
instance MonadThrow m => Monad (Fold m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)

-------------------------------------------------------------------------------
-- Array to Array folds
-------------------------------------------------------------------------------

{-# INLINE take #-}
take :: forall m a b. (Monad m, Storable a) => Int -> Fold m a b -> Fold m a b
take n (Fold (ParserD.Parser step1 initial1 extract1)) =
    Fold $ ParserD.Parser step initial extract

    where

    initial = do
        res <- initial1
        case res of
            IPartial s ->
                if n > 0
                then return $ IPartial $ Tuple' n s
                else IDone <$> extract1 s
            IDone b -> return $ IDone b
            IError err -> return $ IError err

    {-# INLINE partial #-}
    partial i1 st j s =
        let i2 = i1 + j
         in if i2 > 0
            then return $ st j (Tuple' i2 s)
            else Done 0 <$> extract1 s -- i2 == i1 == j == 0

    step (Tuple' i r) arr = do
        let len = Array.length arr
            i1 = i - len
        if i1 >= 0
        then do
            res <- step1 r arr
            case res of
                Partial j s -> partial i1 Partial j s
                Continue j s -> partial i1 Continue j s
                Done j b -> return $ Done j b
                Error err -> return $ Error err
        else do
            let !(Array (ForeignPtr start contents) _) = arr
                sz = sizeOf (undefined :: a)
                end = Ptr start `plusPtr` (i * sz)
                arr1 = Array (ForeignPtr start contents) end
                remaining = negate i1
            res <- step1 r arr1
            case res of
                Partial 0 s -> Done remaining <$> extract1 s
                Partial j s -> return $ Partial (remaining + j) (Tuple' j s)
                Continue 0 s -> Done remaining <$> extract1 s
                Continue j s -> return $ Continue (remaining + j) (Tuple' j s)
                Done j b -> return $ Done (remaining + j) b
                Error err -> return $ Error err

    extract (Tuple' _ r) = extract1 r
