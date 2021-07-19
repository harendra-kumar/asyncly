#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Type
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See notes in "Streamly.Internal.Data.Array.Foreign.Mut.Type"
--
module Streamly.Internal.Data.Array.Foreign.Type
    (
    -- $arrayNotes
      Array (..)

    -- * Freezing and Thawing
    , unsafeFreeze
    , unsafeFreezeWithShrink
    , unsafeThaw

    -- * Construction
    , spliceTwo

    , fromPtr
    , fromAddr#
    , fromCString#
    , fromList
    , fromListN
    , fromStreamDN
    , fromStreamD

    -- * Split
    , breakOn

    -- * Elimination
    , unsafeIndexIO
    , unsafeIndex
    , byteLength
    , length

    , foldl'
    , foldr
    , splitAt

    , readRev
    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toStream
    , toStreamRev
    , toList

    -- * Folds
    , toArrayMinChunk
    , writeN
    , writeNUnsafe
    , MA.ArrayUnsafe (..)
    , writeNAligned
    , writeNAlignedUnmanaged
    , write
    , writeAligned

    -- * Streams of arrays
    , arraysOf
    , bufferChunks
    , flattenArrays
    , flattenArraysRev

    -- * Utilities
    , MA.defaultChunkSize
    , MA.mkChunkSize
    , MA.mkChunkSizeKB
    , MA.unsafeInlineIO

    , MA.memcpy
    , MA.memcmp
    , MA.bytesToElemCount
    )
where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (Addr#, nullAddr#)
import GHC.Exts (IsList, IsString(..))
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
#ifdef DEVBUILD
import GHC.ForeignPtr (touchForeignPtr, unsafeForeignPtrToPtr)
#endif
import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Text.Read (readPrec, readListPrec, readListPrecDefault)

import Prelude hiding (length, foldr, read, unlines, splitAt)

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Unfold.Type as Unfold
import qualified GHC.Exts as Exts

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif

--
-- $setup
-- >>> :m
-- >>> :set -XMagicHash
-- >>> import Prelude hiding (length, foldr, read, unlines, splitAt)
-- >>> import Streamly.Internal.Data.Array.Foreign as Array

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- $arrayNotes
--
-- We can use a 'Storable' constraint in the Array type and the constraint can
-- be automatically provided to a function that pattern matches on the Array
-- type. However, it has huge performance cost, so we do not use it.
-- Investigate a GHC improvement possiblity.
--
-- XXX Rename the fields to better names.
--
data Array a =
#ifdef DEVBUILD
    Storable a =>
#endif
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused addres
    }

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

-------------------------------------------------------------------------------
-- Freezing and Thawing
-------------------------------------------------------------------------------

-- | Returns an immutable array using the same underlying pointers of the
-- mutable array. If the underlying array is mutated, the immutable promise is
-- lost. Please make sure that the mutable array is never used after freezing it
-- using /unsafeFreeze/.
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MA.Array a -> Array a
unsafeFreeze (MA.Array as ae _) = Array as ae

-- | Similar to 'unsafeFreeze' but uses 'MA.shrinkToFit' on the mutable array
-- first.
{-# INLINE unsafeFreezeWithShrink #-}
unsafeFreezeWithShrink :: Storable a => MA.Array a -> Array a
unsafeFreezeWithShrink arr = unsafePerformIO $ do
  MA.Array as ae _ <- MA.shrinkToFit arr
  return $ Array as ae

-- | Returns a mutable array using the same underlying pointers of the immutable
-- array. If the resulting array is mutated, the older immutable array is
-- mutated as well. Please make sure that the immutable array is never used
-- after thawing it using /unsafeThaw/.
{-# INLINE unsafeThaw #-}
unsafeThaw :: Array a -> MA.Array a
unsafeThaw (Array as ae) = MA.Array as ae ae

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- Splice two immutable arrays creating a new array.
{-# INLINE spliceTwo #-}
spliceTwo :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
spliceTwo arr1 arr2 =
    unsafeFreeze <$> MA.spliceTwo (unsafeThaw arr1) (unsafeThaw arr2)

-- | Create an 'Array' of the given number of elements of type @a@ from a read
-- only pointer @Ptr a@.  The pointer is not freed when the array is garbage
-- collected. This API is unsafe for the following reasons:
--
-- 1. The pointer must point to static pinned memory or foreign memory that
-- does not require freeing..
-- 2. The pointer must be legally accessible upto the given length.
-- 3. To guarantee that the array is immutable, the contents of the address
-- must be guaranteed to not change.
--
-- /Unsafe/
--
-- /Pre-release/
--
{-# INLINE fromPtr #-}
fromPtr ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Int -> Ptr a -> Array a
fromPtr n ptr = MA.unsafeInlineIO $ do
    fptr <- newForeignPtr_ ptr
    let end = ptr `plusPtr` n
    return $ Array
        { aStart = fptr
        , aEnd   = end
        }

-- XXX when converting an array of Word8 from a literal string we can simply
-- refer to the literal string. Is it possible to write rules such that
-- fromList Word8 can be rewritten so that GHC does not first convert the
-- literal to [Char] and then we convert it back to an Array Word8?
--
-- TBD: We can also add template haskell quasiquotes to specify arrays of other
-- literal types. TH will encode them into a string literal and we read that as
-- an array of the required type. With template Haskell we can provide a safe
-- version of fromString#.
--
-- | Create an @Array Word8@ of the given length from a static, read only
-- machine address 'Addr#'. See 'fromPtr' for safety caveats.
--
-- A common use case for this API is to create an array from a static unboxed
-- string literal. GHC string literals are of type 'Addr#', and must contain
-- characters that can be encoded in a byte i.e. characters or literal bytes in
-- the range from 0-255.
--
-- >>> import Data.Word (Word8)
-- >>> Array.fromAddr# 5 "hello world!"# :: Array Word8
-- [104,101,108,108,111]
--
-- >>> Array.fromAddr# 3 "\255\NUL\255"# :: Array Word8
-- [255,0,255]
--
-- /See also: 'fromString#'/
--
-- /Unsafe/
--
-- /Time complexity: O(1)/
--
-- /Pre-release/
--
{-# INLINE fromAddr# #-}
fromAddr# ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Int -> Addr# -> Array a
fromAddr# n addr# = fromPtr n (castPtr $ Ptr addr#)

-- | Generate a byte array from an 'Addr#' that contains a sequence of NUL
-- (@0@) terminated bytes. The array would not include the NUL byte. The
-- address must be in static read-only memory and must be legally accessible up
-- to and including the first NUL byte.
--
-- An unboxed string literal (e.g. @"hello"#@) is a common example of an
-- 'Addr#' in static read only memory. It represents the UTF8 encoded sequence
-- of bytes terminated by a NUL byte (a 'CString') corresponding to the
-- given unicode string.
--
-- >>> Array.fromCString# "hello world!"#
-- [104,101,108,108,111,32,119,111,114,108,100,33]
--
-- >>> Array.fromCString# "\255\NUL\255"#
-- [255]
--
-- /See also: 'fromAddr#'/
--
-- /Unsafe/
--
-- /Time complexity: O(n) (computes the length of the string)/
--
-- /Pre-release/
--
{-# INLINE fromCString# #-}
fromCString# :: Addr# -> Array Word8
fromCString# addr# = do
    let cstr = Ptr addr#
        len = MA.unsafeInlineIO $ c_strlen cstr
    fromPtr (fromIntegral len) (castPtr cstr)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINABLE fromListN #-}
fromListN :: Storable a => Int -> [a] -> Array a
fromListN n xs = unsafeFreeze $ MA.fromListN n xs

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafeFreeze $ MA.fromList xs

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = unsafeFreeze <$> MA.fromStreamDN limit str

{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: forall m a. (MonadIO m, Storable a)
    => D.Stream m a -> m (Array a)
fromStreamD str = unsafeFreeze <$> MA.fromStreamD str

-------------------------------------------------------------------------------
-- Streams of arrays
-------------------------------------------------------------------------------

{-# INLINE bufferChunks #-}
bufferChunks :: (MonadIO m, Storable a) =>
    D.Stream m a -> m (K.Stream m (Array a))
bufferChunks m = D.foldr K.cons K.nil $ arraysOf MA.defaultChunkSize m

-- | @arraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
{-# INLINE_NORMAL arraysOf #-}
arraysOf :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
arraysOf n str = D.map unsafeFreeze $ MA.arraysOf n str

-- | Use the "read" unfold instead.
--
-- @flattenArrays = unfoldMany read@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArrays = MA.flattenArrays . D.map unsafeThaw

-- | Use the "readRev" unfold instead.
--
-- @flattenArrays = unfoldMany readRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArraysRev = MA.flattenArraysRev . D.map unsafeThaw

-- Drops the separator byte
{-# INLINE breakOn #-}
breakOn :: MonadIO m
    => Word8 -> Array Word8 -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr = do
  (a, b) <- MA.breakOn sep (unsafeThaw arr)
  return (unsafeFreeze a, unsafeFreeze <$> b)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Return element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeIndexIO #-}
unsafeIndexIO :: forall a. Storable a => Array a -> Int -> IO a
unsafeIndexIO arr = MA.unsafeIndexIO (unsafeThaw arr)

-- | Return element at the specified index without checking the bounds.
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Storable a => Array a -> Int -> a
unsafeIndex arr i = let !r = MA.unsafeInlineIO $ unsafeIndexIO arr i in r

-- | /O(1)/ Get the byte length of the array.
--
-- @since 0.7.0
{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength = MA.byteLength . unsafeThaw

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr =  MA.length (unsafeThaw arr)

-- | Unfold an array into a stream in reverse order.
--
-- @since 0.8.0
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
readRev = Unfold.lmap unsafeThaw MA.readRev

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD arr = MA.toStreamD (unsafeThaw arr)

{-# INLINE toStreamK #-}
toStreamK :: forall t m a. (K.IsStream t, Storable a) => Array a -> t m a
toStreamK arr = MA.toStreamK (unsafeThaw arr)

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamDRev arr = MA.toStreamDRev (unsafeThaw arr)

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall t m a. (K.IsStream t, Storable a) => Array a -> t m a
toStreamKRev arr = MA.toStreamKRev (unsafeThaw arr)

-- | Convert an 'Array' into a stream.
--
-- /Pre-release/
{-# INLINE_EARLY toStream #-}
toStream :: (Monad m, K.IsStream t, Storable a) => Array a -> t m a
toStream = D.fromStreamD . toStreamD
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.read fallback to StreamK" [1]
--     forall a. S.readK (read a) = K.fromArray a #-}

-- | Convert an 'Array' into a stream in reverse order.
--
-- /Pre-release/
{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Monad m, K.IsStream t, Storable a) => Array a -> t m a
toStreamRev = D.fromStreamD . toStreamDRev

-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = MA.foldl' f z (unsafeThaw arr)

{-# INLINE_NORMAL foldr #-}
foldr :: Storable a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = MA.foldr f z (unsafeThaw arr)

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
-- @since 0.7.0
splitAt :: forall a. Storable a => Int -> Array a -> (Array a, Array a)
splitAt i arr = (unsafeFreeze a, unsafeFreeze b)
  where
    (a, b) = MA.splitAt i (unsafeThaw arr)

-- | Convert an 'Array' into a list.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE toList #-}
toList :: Storable a => Array a -> [a]
toList = MA.toList . unsafeThaw

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
writeN = fmap unsafeFreeze . MA.writeN

-- | @writeNAligned alignment n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAligned alignSize = fmap unsafeFreeze . MA.writeNAligned alignSize

-- | @writeNAlignedUnmanaged n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size and using unmanaged memory.
-- This could be useful to allocate memory that we need to allocate only once
-- in the lifetime of the program.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAlignedUnmanaged #-}
writeNAlignedUnmanaged :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAlignedUnmanaged alignSize =
    fmap unsafeFreeze . MA.writeNAlignedUnmanaged alignSize

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- @since 0.7.0
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = unsafeFreeze <$> MA.writeNUnsafe n

-- XXX The realloc based implementation needs to make one extra copy if we use
-- shrinkToFit.  On the other hand, the stream of arrays implementation may
-- buffer the array chunk pointers in memory but it does not have to shrink as
-- we know the exact size in the end. However, memory copying does not seems to
-- be as expensive as the allocations. Therefore, we need to reduce the number
-- of allocations instead. Also, the size of allocations matters, right sizing
-- an allocation even at the cost of copying sems to help.  Should be measured
-- on a big stream with heavy calls to toArray to see the effect.
--
-- XXX check if GHC's memory allocator is efficient enough. We can try the C
-- malloc to compare against.

{-# INLINE_NORMAL toArrayMinChunk #-}
toArrayMinChunk :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
-- toArrayMinChunk n = FL.rmapM spliceArrays $ toArraysOf n
toArrayMinChunk alignSize elemCount =
    unsafeFreeze <$> MA.toArrayMinChunk alignSize elemCount

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = fmap unsafeFreeze MA.write

-- | Like 'write' but the array memory is aligned according to the specified
-- alignment size. This could be useful when we have specific alignment, for
-- example, cache aligned arrays for lookup table etc.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE writeAligned #-}
writeAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeAligned = fmap unsafeFreeze . MA.writeAligned

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (Show a, Storable a) => Show (Array a) where
    {-# INLINE showsPrec #-}
    showsPrec _ = shows . toList

instance (Storable a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = fromList <$> readPrec
    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (Array a) where
    {-# INLINE fromString #-}
    fromString = fromList

-- GHC versions 8.0 and below cannot derive IsList
instance Storable a => IsList (Array a) where
    type (Item (Array a)) = a
    {-# INLINE fromList #-}
    fromList = fromList
    {-# INLINE fromListN #-}
    fromListN = fromListN
    {-# INLINE toList #-}
    toList = toList

-- XXX we are assuming that Storable equality means element equality. This may
-- or may not be correct? arrcmp is 40% faster compared to stream equality.
instance (Storable a, Eq a) => Eq (Array a) where
    {-# INLINE (==) #-}
    a == b = unsafeThaw a == unsafeThaw b
    -- arr1 == arr2 = runIdentity $ D.eqBy (==) (toStreamD arr1) (toStreamD arr2)

instance (Storable a, NFData a) => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()

instance (Storable a, Ord a) => Ord (Array a) where
    {-# INLINE compare #-}
    compare a b = compare (unsafeThaw a) (unsafeThaw b)

    -- Default definitions defined in base do not have an INLINE on them, so we
    -- replicate them here with an INLINE.
    {-# INLINE (<) #-}
    x <  y = case compare x y of { LT -> True;  _ -> False }

    {-# INLINE (<=) #-}
    x <= y = case compare x y of { GT -> False; _ -> True }

    {-# INLINE (>) #-}
    x >  y = case compare x y of { GT -> True;  _ -> False }

    {-# INLINE (>=) #-}
    x >= y = case compare x y of { LT -> False; _ -> True }

    -- These two default methods use '<=' rather than 'compare'
    -- because the latter is often more expensive
    {-# INLINE max #-}
    max x y = if x <= y then y else x

    {-# INLINE min #-}
    min x y = if x <= y then x else y

#ifdef DEVBUILD
-- Definitions using the Storable constraint from the Array type. These are to
-- make the Foldable instance possible though it is much slower (7x slower).
--
{-# INLINE_NORMAL toStreamD_ #-}
toStreamD_ :: forall m a. MonadIO m => Int -> Array a -> D.Stream m a
toStreamD_ size Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        x <- liftIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` size)

{-# INLINE_NORMAL _foldr #-}
_foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
_foldr f z arr@Array {..} =
    let !n = sizeOf (undefined :: a)
    in unsafePerformIO $ D.foldr f z $ toStreamD_ n arr

-- | Note that the 'Foldable' instance is 7x slower than the direct
-- operations.
instance Foldable Array where
  foldr = _foldr
#endif

-------------------------------------------------------------------------------
-- Semigroup and Monoid
-------------------------------------------------------------------------------

instance Storable a => Semigroup (Array a) where
    arr1 <> arr2 = unsafePerformIO $ spliceTwo arr1 arr2

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

nil ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Array a
nil = Array nullForeignPtr (Ptr nullAddr#)

instance Storable a => Monoid (Array a) where
    mempty = nil
    mappend = (<>)
