-- |
-- Module      : Streamly.Internal.Data.Stream.Enumeration
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The functions defined in this module should be rarely needed for direct use,
-- try to use the operations from the 'Enumerable' type class
-- instances instead.
--
-- This module provides an 'Enumerable' type class to enumerate 'Enum' types
-- into a stream. The operations in this type class correspond to similar
-- perations in the 'Enum' type class, the only difference is that they produce
-- a stream instead of a list. These operations cannot be defined generically
-- based on the 'Enum' type class. We provide instances for commonly used
-- types. If instances for other types are needed convenience functions defined
-- in this module can be used to define them. Alternatively, these functions
-- can be used directly.

module Streamly.Internal.Data.Stream.Enumeration
    (
      Enumerable (..)

    -- ** Enumerating 'Bounded' 'Enum' Types
    , enumerate
    , enumerateTo
    , enumerateFromBounded

    -- ** Enumerating 'Enum' Types not larger than 'Int'
    , enumerateFromToSmall
    , enumerateFromThenToSmall
    , enumerateFromThenSmallBounded

    -- ** Enumerating 'Bounded' 'Integral' Types
    , enumerateFromIntegral
    , enumerateFromThenIntegral

    -- ** Enumerating 'Integral' Types
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    -- ** Enumerating unbounded 'Integral' Types
    , enumerateFromStepIntegral

    -- ** Enumerating 'Fractional' Types
    , enumerateFromFractional
    , enumerateFromToFractional
    , enumerateFromThenFractional
    , enumerateFromThenToFractional
    )
where

import Data.Fixed
import Data.Int
import Data.Ratio
import Data.Word
import Numeric.Natural
import Data.Functor.Identity (Identity(..))

import Streamly.Internal.Data.Stream.StreamD (fromStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream(..))

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial

-------------------------------------------------------------------------------
-- Enumeration of Integral types
-------------------------------------------------------------------------------
--
-- | @enumerateFromStepIntegral from step@ generates an infinite stream whose
-- first element is @from@ and the successive elements are in increments of
-- @step@.
--
-- CAUTION: This function is not safe for finite integral types. It does not
-- check for overflow, underflow or bounds.
--
-- @
-- > S.toList $ S.take 4 $ S.enumerateFromStepIntegral 0 2
-- [0,2,4,6]
-- > S.toList $ S.take 3 $ S.enumerateFromStepIntegral 0 (-2)
-- [0,-2,-4]
-- @
--
-- @since 0.6.0
{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral
    :: (IsStream t, Monad m, Integral a)
    => a -> a -> t m a
enumerateFromStepIntegral from stride =
    fromStreamD $ D.enumerateFromStepIntegral from stride

-- | Enumerate an 'Integral' type. @enumerateFromIntegral from@ generates a
-- stream whose first element is @from@ and the successive elements are in
-- increments of @1@. The stream is bounded by the size of the 'Integral' type.
--
-- @
-- > S.toList $ S.take 4 $ S.enumerateFromIntegral (0 :: Int)
-- [0,1,2,3]
-- @
--
-- @since 0.6.0
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral
    :: (IsStream t, Monad m, Integral a, Bounded a)
    => a -> t m a
enumerateFromIntegral from = fromStreamD $ D.enumerateFromIntegral from

-- | Enumerate an 'Integral' type in steps. @enumerateFromThenIntegral from
-- then@ generates a stream whose first element is @from@, the second element
-- is @then@ and the successive elements are in increments of @then - from@.
-- The stream is bounded by the size of the 'Integral' type.
--
-- @
-- > S.toList $ S.take 4 $ S.enumerateFromThenIntegral (0 :: Int) 2
-- [0,2,4,6]
-- > S.toList $ S.take 4 $ S.enumerateFromThenIntegral (0 :: Int) (-2)
-- [0,-2,-4,-6]
-- @
--
-- @since 0.6.0
{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (IsStream t, Monad m, Integral a, Bounded a)
    => a -> a -> t m a
enumerateFromThenIntegral from next =
    fromStreamD $ D.enumerateFromThenIntegral from next

-- | Enumerate an 'Integral' type up to a given limit.
-- @enumerateFromToIntegral from to@ generates a finite stream whose first
-- element is @from@ and successive elements are in increments of @1@ up to
-- @to@.
--
-- @
-- > S.toList $ S.enumerateFromToIntegral 0 4
-- [0,1,2,3,4]
-- @
--
-- @since 0.6.0
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (IsStream t, Monad m, Integral a) => a -> a -> t m a
enumerateFromToIntegral from to =
    fromStreamD $ D.enumerateFromToIntegral from to

-- | Enumerate an 'Integral' type in steps up to a given limit.
-- @enumerateFromThenToIntegral from then to@ generates a finite stream whose
-- first element is @from@, the second element is @then@ and the successive
-- elements are in increments of @then - from@ up to @to@.
--
-- @
-- > S.toList $ S.enumerateFromThenToIntegral 0 2 6
-- [0,2,4,6]
-- > S.toList $ S.enumerateFromThenToIntegral 0 (-2) (-6)
-- [0,-2,-4,-6]
-- @
--
-- @since 0.6.0
{-# INLINE enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (IsStream t, Monad m, Integral a)
    => a -> a -> a -> t m a
enumerateFromThenToIntegral from next to =
    fromStreamD $ D.enumerateFromThenToIntegral from next to

-------------------------------------------------------------------------------
-- Enumeration of Fractional types
-------------------------------------------------------------------------------
--
-- Even though the underlying implementation of enumerateFromFractional and
-- enumerateFromThenFractional works for any 'Num' we have restricted these to
-- 'Fractional' because these do not perform any bounds check, in contrast to
-- integral versions and are therefore not equivalent substitutes for those.
--
-- | Numerically stable enumeration from a 'Fractional' number in steps of size
-- @1@. @enumerateFromFractional from@ generates a stream whose first element
-- is @from@ and the successive elements are in increments of @1@.  No overflow
-- or underflow checks are performed.
--
-- This is the equivalent to 'enumFrom' for 'Fractional' types. For example:
--
-- @
-- > S.toList $ S.take 4 $ S.enumerateFromFractional 1.1
-- [1.1,2.1,3.1,4.1]
-- @
--
--
-- @since 0.6.0
{-# INLINE enumerateFromFractional #-}
enumerateFromFractional :: (IsStream t, Monad m, Fractional a) => a -> t m a
enumerateFromFractional from = fromStreamD $ D.numFrom from

-- | Numerically stable enumeration from a 'Fractional' number in steps.
-- @enumerateFromThenFractional from then@ generates a stream whose first
-- element is @from@, the second element is @then@ and the successive elements
-- are in increments of @then - from@.  No overflow or underflow checks are
-- performed.
--
-- This is the equivalent of 'enumFromThen' for 'Fractional' types. For
-- example:
--
-- @
-- > S.toList $ S.take 4 $ S.enumerateFromThenFractional 1.1 2.1
-- [1.1,2.1,3.1,4.1]
-- > S.toList $ S.take 4 $ S.enumerateFromThenFractional 1.1 (-2.1)
-- [1.1,-2.1,-5.300000000000001,-8.500000000000002]
-- @
--
-- @since 0.6.0
{-# INLINE enumerateFromThenFractional #-}
enumerateFromThenFractional
    :: (IsStream t, Monad m, Fractional a)
    => a -> a -> t m a
enumerateFromThenFractional from next = fromStreamD $ D.numFromThen from next

-- | Numerically stable enumeration from a 'Fractional' number to a given
-- limit.  @enumerateFromToFractional from to@ generates a finite stream whose
-- first element is @from@ and successive elements are in increments of @1@ up
-- to @to@.
--
-- This is the equivalent of 'enumFromTo' for 'Fractional' types. For
-- example:
--
-- @
-- > S.toList $ S.enumerateFromToFractional 1.1 4
-- [1.1,2.1,3.1,4.1]
-- > S.toList $ S.enumerateFromToFractional 1.1 4.6
-- [1.1,2.1,3.1,4.1,5.1]
-- @
--
-- Notice that the last element is equal to the specified @to@ value after
-- rounding to the nearest integer.
--
-- @since 0.6.0
{-# INLINE enumerateFromToFractional #-}
enumerateFromToFractional
    :: (IsStream t, Monad m, Fractional a, Ord a)
    => a -> a -> t m a
enumerateFromToFractional from to =
    fromStreamD $ D.enumerateFromToFractional from to

-- | Numerically stable enumeration from a 'Fractional' number in steps up to a
-- given limit.  @enumerateFromThenToFractional from then to@ generates a
-- finite stream whose first element is @from@, the second element is @then@
-- and the successive elements are in increments of @then - from@ up to @to@.
--
-- This is the equivalent of 'enumFromThenTo' for 'Fractional' types. For
-- example:
--
-- @
-- > S.toList $ S.enumerateFromThenToFractional 0.1 2 6
-- [0.1,2.0,3.9,5.799999999999999]
-- > S.toList $ S.enumerateFromThenToFractional 0.1 (-2) (-6)
-- [0.1,-2.0,-4.1000000000000005,-6.200000000000001]
-- @
--
--
-- @since 0.6.0
{-# INLINE enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (IsStream t, Monad m, Fractional a, Ord a)
    => a -> a -> a -> t m a
enumerateFromThenToFractional from next to =
    fromStreamD $ D.enumerateFromThenToFractional from next to

-------------------------------------------------------------------------------
-- Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------
--
-- | 'enumerateFromTo' for 'Enum' types not larger than 'Int'.
--
-- @since 0.6.0
{-# INLINE enumerateFromToSmall #-}
enumerateFromToSmall :: (IsStream t, Monad m, Enum a) => a -> a -> t m a
enumerateFromToSmall from to = Serial.map toEnum $
    enumerateFromToIntegral (fromEnum from) (fromEnum to)

-- | 'enumerateFromThenTo' for 'Enum' types not larger than 'Int'.
--
-- @since 0.6.0
{-# INLINE enumerateFromThenToSmall #-}
enumerateFromThenToSmall :: (IsStream t, Monad m, Enum a)
    => a -> a -> a -> t m a
enumerateFromThenToSmall from next to = Serial.map toEnum $
    enumerateFromThenToIntegral (fromEnum from) (fromEnum next) (fromEnum to)

-- | 'enumerateFromThen' for 'Enum' types not larger than 'Int'.
--
-- Note: We convert the 'Enum' to 'Int' and enumerate the 'Int'. If a
-- type is bounded but does not have a 'Bounded' instance then we can go on
-- enumerating it beyond the legal values of the type, resulting in the failure
-- of 'toEnum' when converting back to 'Enum'. Therefore we require a 'Bounded'
-- instance for this function to be safely used.
--
-- @since 0.6.0
{-# INLINE enumerateFromThenSmallBounded #-}
enumerateFromThenSmallBounded :: (IsStream t, Monad m, Enumerable a, Bounded a)
    => a -> a -> t m a
enumerateFromThenSmallBounded from next =
    if fromEnum next >= fromEnum from
    then enumerateFromThenTo from next maxBound
    else enumerateFromThenTo from next minBound

-------------------------------------------------------------------------------
-- Enumerable type class
-------------------------------------------------------------------------------
--
-- NOTE: We would like to rewrite calls to fromList [1..] etc. to stream
-- enumerations like this:
--
-- {-# RULES "fromList enumFrom" [1]
--     forall (a :: Int). D.fromList (enumFrom a) = D.enumerateFromIntegral a #-}
--
-- But this does not work because enumFrom is a class method and GHC rewrites
-- it quickly, so we do not get a chance to have our rule fired.

-- | Types that can be enumerated as a stream. The operations in this type
-- class are equivalent to those in the 'Enum' type class, except that these
-- generate a stream instead of a list. Use the functions in
-- "Streamly.Internal.Data.Stream.Enumeration" module to define new instances.
--
-- @since 0.6.0
class Enum a => Enumerable a where
    -- | @enumerateFrom from@ generates a stream starting with the element
    -- @from@, enumerating up to 'maxBound' when the type is 'Bounded' or
    -- generating an infinite stream when the type is not 'Bounded'.
    --
    -- @
    -- > S.toList $ S.take 4 $ S.enumerateFrom (0 :: Int)
    -- [0,1,2,3]
    -- @
    --
    -- For 'Fractional' types, enumeration is numerically stable. However, no
    -- overflow or underflow checks are performed.
    --
    -- @
    -- > S.toList $ S.take 4 $ S.enumerateFrom 1.1
    -- [1.1,2.1,3.1,4.1]
    -- @
    --
    -- @since 0.6.0
    enumerateFrom :: (IsStream t, Monad m) => a -> t m a

    -- | Generate a finite stream starting with the element @from@, enumerating
    -- the type up to the value @to@. If @to@ is smaller than @from@ then an
    -- empty stream is returned.
    --
    -- @
    -- > S.toList $ S.enumerateFromTo 0 4
    -- [0,1,2,3,4]
    -- @
    --
    -- For 'Fractional' types, the last element is equal to the specified @to@
    -- value after rounding to the nearest integral value.
    --
    -- @
    -- > S.toList $ S.enumerateFromTo 1.1 4
    -- [1.1,2.1,3.1,4.1]
    -- > S.toList $ S.enumerateFromTo 1.1 4.6
    -- [1.1,2.1,3.1,4.1,5.1]
    -- @
    --
    -- @since 0.6.0
    enumerateFromTo :: (IsStream t, Monad m) => a -> a -> t m a

    -- | @enumerateFromThen from then@ generates a stream whose first element
    -- is @from@, the second element is @then@ and the successive elements are
    -- in increments of @then - from@.  Enumeration can occur downwards or
    -- upwards depending on whether @then@ comes before or after @from@. For
    -- 'Bounded' types the stream ends when 'maxBound' is reached, for
    -- unbounded types it keeps enumerating infinitely.
    --
    -- @
    -- > S.toList $ S.take 4 $ S.enumerateFromThen 0 2
    -- [0,2,4,6]
    -- > S.toList $ S.take 4 $ S.enumerateFromThen 0 (-2)
    -- [0,-2,-4,-6]
    -- @
    --
    -- @since 0.6.0
    enumerateFromThen :: (IsStream t, Monad m) => a -> a -> t m a

    -- | @enumerateFromThenTo from then to@ generates a finite stream whose
    -- first element is @from@, the second element is @then@ and the successive
    -- elements are in increments of @then - from@ up to @to@. Enumeration can
    -- occur downwards or upwards depending on whether @then@ comes before or
    -- after @from@.
    --
    -- @
    -- > S.toList $ S.enumerateFromThenTo 0 2 6
    -- [0,2,4,6]
    -- > S.toList $ S.enumerateFromThenTo 0 (-2) (-6)
    -- [0,-2,-4,-6]
    -- @
    --
    -- @since 0.6.0
    enumerateFromThenTo :: (IsStream t, Monad m) => a -> a -> a -> t m a

-- MAYBE: Sometimes it is more convenient to know the count rather then the
-- ending or starting element. For those cases we can define the folllowing
-- APIs. All of these will work only for bounded types if we represent the
-- count by Int.
--
-- enumerateN
-- enumerateFromN
-- enumerateToN
-- enumerateFromStep
-- enumerateFromStepN

-------------------------------------------------------------------------------
-- Convenient functions for bounded types
-------------------------------------------------------------------------------
--
-- |
-- > enumerate = enumerateFrom minBound
--
-- Enumerate a 'Bounded' type from its 'minBound' to 'maxBound'
--
-- @since 0.6.0
{-# INLINE enumerate #-}
enumerate :: (IsStream t, Monad m, Bounded a, Enumerable a) => t m a
enumerate = enumerateFrom minBound

-- |
-- > enumerateTo = enumerateFromTo minBound
--
-- Enumerate a 'Bounded' type from its 'minBound' to specified value.
--
-- @since 0.6.0
{-# INLINE enumerateTo #-}
enumerateTo :: (IsStream t, Monad m, Bounded a, Enumerable a) => a -> t m a
enumerateTo = enumerateFromTo minBound

-- |
-- > enumerateFromBounded = enumerateFromTo from maxBound
--
-- 'enumerateFrom' for 'Bounded' 'Enum' types.
--
-- @since 0.6.0
{-# INLINE enumerateFromBounded #-}
enumerateFromBounded :: (IsStream t, Monad m, Enumerable a, Bounded a)
    => a -> t m a
enumerateFromBounded from = enumerateFromTo from maxBound

-------------------------------------------------------------------------------
-- Enumerable Instances
-------------------------------------------------------------------------------
--
-- For Enum types smaller than or equal to Int size.
#define ENUMERABLE_BOUNDED_SMALL(SMALL_TYPE)           \
instance Enumerable SMALL_TYPE where {                 \
    {-# INLINE enumerateFrom #-};                      \
    enumerateFrom = enumerateFromBounded;              \
    {-# INLINE enumerateFromThen #-};                  \
    enumerateFromThen = enumerateFromThenSmallBounded; \
    {-# INLINE enumerateFromTo #-};                    \
    enumerateFromTo = enumerateFromToSmall;            \
    {-# INLINE enumerateFromThenTo #-};                \
    enumerateFromThenTo = enumerateFromThenToSmall }


ENUMERABLE_BOUNDED_SMALL(())
ENUMERABLE_BOUNDED_SMALL(Bool)
ENUMERABLE_BOUNDED_SMALL(Ordering)
ENUMERABLE_BOUNDED_SMALL(Char)

-- For bounded Integral Enum types, may be larger than Int.
#define ENUMERABLE_BOUNDED_INTEGRAL(INTEGRAL_TYPE)  \
instance Enumerable INTEGRAL_TYPE where {           \
    {-# INLINE enumerateFrom #-};                   \
    enumerateFrom = enumerateFromIntegral;          \
    {-# INLINE enumerateFromThen #-};               \
    enumerateFromThen = enumerateFromThenIntegral;  \
    {-# INLINE enumerateFromTo #-};                 \
    enumerateFromTo = enumerateFromToIntegral;      \
    {-# INLINE enumerateFromThenTo #-};             \
    enumerateFromThenTo = enumerateFromThenToIntegral }

ENUMERABLE_BOUNDED_INTEGRAL(Int)
ENUMERABLE_BOUNDED_INTEGRAL(Int8)
ENUMERABLE_BOUNDED_INTEGRAL(Int16)
ENUMERABLE_BOUNDED_INTEGRAL(Int32)
ENUMERABLE_BOUNDED_INTEGRAL(Int64)
ENUMERABLE_BOUNDED_INTEGRAL(Word)
ENUMERABLE_BOUNDED_INTEGRAL(Word8)
ENUMERABLE_BOUNDED_INTEGRAL(Word16)
ENUMERABLE_BOUNDED_INTEGRAL(Word32)
ENUMERABLE_BOUNDED_INTEGRAL(Word64)

-- For unbounded Integral Enum types.
#define ENUMERABLE_UNBOUNDED_INTEGRAL(INTEGRAL_TYPE)              \
instance Enumerable INTEGRAL_TYPE where {                         \
    {-# INLINE enumerateFrom #-};                                 \
    enumerateFrom from = enumerateFromStepIntegral from 1;        \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen from next =                                 \
        enumerateFromStepIntegral from (next - from);             \
    {-# INLINE enumerateFromTo #-};                               \
    enumerateFromTo = enumerateFromToIntegral;                    \
    {-# INLINE enumerateFromThenTo #-};                           \
    enumerateFromThenTo = enumerateFromThenToIntegral }

ENUMERABLE_UNBOUNDED_INTEGRAL(Integer)
ENUMERABLE_UNBOUNDED_INTEGRAL(Natural)

#define ENUMERABLE_FRACTIONAL(FRACTIONAL_TYPE,CONSTRAINT)         \
instance (CONSTRAINT) => Enumerable (FRACTIONAL_TYPE) where {     \
    {-# INLINE enumerateFrom #-};                                 \
    enumerateFrom = enumerateFromFractional;                      \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen = enumerateFromThenFractional;              \
    {-# INLINE enumerateFromTo #-};                               \
    enumerateFromTo = enumerateFromToFractional;                  \
    {-# INLINE enumerateFromThenTo #-};                           \
    enumerateFromThenTo = enumerateFromThenToFractional }

ENUMERABLE_FRACTIONAL(Float,)
ENUMERABLE_FRACTIONAL(Double,)
ENUMERABLE_FRACTIONAL(Fixed a,HasResolution a)
ENUMERABLE_FRACTIONAL(Ratio a,Integral a)

instance Enumerable a => Enumerable (Identity a) where
    {-# INLINE enumerateFrom #-}
    enumerateFrom (Identity from) = Serial.map Identity $
        enumerateFrom from
    {-# INLINE enumerateFromThen #-}
    enumerateFromThen (Identity from) (Identity next) = Serial.map Identity $
        enumerateFromThen from next
    {-# INLINE enumerateFromTo #-}
    enumerateFromTo (Identity from) (Identity to) = Serial.map Identity $
        enumerateFromTo from to
    {-# INLINE enumerateFromThenTo #-}
    enumerateFromThenTo (Identity from) (Identity next) (Identity to) =
        Serial.map Identity $ enumerateFromThenTo from next to

-- TODO
{-
instance Enumerable a => Enumerable (Last a)
instance Enumerable a => Enumerable (First a)
instance Enumerable a => Enumerable (Max a)
instance Enumerable a => Enumerable (Min a)
instance Enumerable a => Enumerable (Const a b)
instance Enumerable (f a) => Enumerable (Alt f a)
instance Enumerable (f a) => Enumerable (Ap f a)
-}
