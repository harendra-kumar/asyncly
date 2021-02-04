{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Prim.Pinned.Mut.Types
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Prim.Pinned.Mut.Types
    (
      Array (..)

    -- * Construction
    , newArray
    , newAlignedArray
    , unsafeWriteIndex

    , spliceTwo
    , unsafeCopy

    , fromListM
    , fromListNM
    , fromStreamDN
    , fromStreamD

    -- * Streams of arrays
    , fromStreamDArraysOf

    , packArraysChunksOf
    , lpackArraysChunksOf

#if !defined(mingw32_HOST_OS)
--    , groupIOVecsOf
#endif

    -- * Elimination
    , unsafeReadIndex
    , length
    , byteLength

    , writeN
    , ArrayUnsafe(..)
    , writeNUnsafe
    , writeNAligned
    , write

    -- * Utilities
    , resizeArray
    , shrinkArray

    , touchArray
    , withArrayAsPtr
    )
where

import GHC.IO (IO(..))

#include "Streamly/Internal/Data/Array/Prim/MutTypesInclude.hs"

-------------------------------------------------------------------------------
-- Allocation (Pinned)
-------------------------------------------------------------------------------

-- XXX we can use a single newArray routine which accepts an allocation
-- function which could be newByteArray#, newPinnedByteArray# or
-- newAlignedPinnedByteArray#. That function can go in the common include file.
--
-- | Allocate an array that is pinned and can hold 'count' items.  The memory of
-- the array is uninitialized.
--
-- Note that this is internal routine, the reference to this array cannot be
-- given out until the array has been written to and frozen.
{-# INLINE newArray #-}
newArray ::
       forall m a. (MonadIO m, Prim a)
    => Int
    -> m (Array a)
newArray (I# n#) =
    liftIO $ do
        let bytes = n# *# sizeOf# (undefined :: a)
        primitive $ \s# ->
            case newPinnedByteArray# bytes s# of
                (# s1#, arr# #) -> (# s1#, Array arr# #)

-- Change order of args?
-- | Allocate a new array aligned to the specified alignment and using pinned
-- memory.
{-# INLINE newAlignedArray #-}
newAlignedArray ::
       forall m a. (MonadIO m, Prim a)
    => Int -- size
    -> Int -- Alignment
    -> m (Array a)
newAlignedArray (I# n#) (I# a#) =
    liftIO $ do
        let bytes = n# *# sizeOf# (undefined :: a)
        primitive $ \s# ->
            case newAlignedPinnedByteArray# bytes a# s# of
                (# s1#, arr# #) -> (# s1#, Array arr# #)

-- | Resize (pinned) mutable byte array to new specified size (in elem
-- count). The returned array is either the original array resized in-place or,
-- if not possible, a newly allocated (pinned) array (with the original content
-- copied over).
{-# INLINE resizeArray #-}
resizeArray ::
       (MonadIO m, Prim a)
    => Array a
    -> Int -- ^ new size
    -> m (Array a)
resizeArray arr i = do
    len <- length arr
    if len == i
    then return arr
    else if i < len
         then shrinkArray arr i >> return arr
         else do
             nArr <- newArray i
             unsafeCopy nArr 0 arr 0 len
             return nArr

-------------------------------------------------------------------------------
-- Aligned Construction
-------------------------------------------------------------------------------

-- XXX we can also factor out common code in writeN and writeNAligned in the
-- same way as suggested above.
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned ::
       (MonadIO m, Prim a)
    => Int
    -> Int
    -> Fold m a (Array a)
writeNAligned align limit = FL.mkFoldM step initial extract

    where

    initial = do
        marr <- newAlignedArray limit align
        return $ FL.Partial $ Tuple' marr 0

    extract (Tuple' marr len) = shrinkArray marr len >> return marr

    step s@(Tuple' marr i) x
        | i == limit = FL.Done <$> extract s
        | otherwise = do
            unsafeWriteIndex marr i x
            return $ FL.Partial $ Tuple' marr (i + 1)

-------------------------------------------------------------------------------
-- Mutation with pointers
-------------------------------------------------------------------------------

-- XXX This section can probably go in a common include file for pinned arrays.

-- Change name later.
{-# INLINE toPtr #-}
toPtr :: Array a -> Ptr a
toPtr (Array arr#) = Ptr (byteArrayContents# (unsafeCoerce# arr#))

{-# INLINE touchArray #-}
touchArray :: Array a -> IO ()
touchArray arr = IO $ \s -> case touch# arr s of s' -> (# s', () #)

{-# INLINE withArrayAsPtr #-}
withArrayAsPtr :: Array a -> (Ptr a -> IO b) -> IO b
withArrayAsPtr arr f = do
    r <- f (toPtr arr)
    touchArray arr
    return r
