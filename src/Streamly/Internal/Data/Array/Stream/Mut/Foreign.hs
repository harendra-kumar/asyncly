-- |
-- Module      : Streamly.Internal.Data.Array.Stream.Mut.Foreign
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of mutable arrays.
--
module Streamly.Internal.Data.Array.Stream.Mut.Foreign
    (
    -- * Generation
      arraysOf

    -- * Compaction
    , packArraysChunksOf
    , SpliceState (..)
    , lpackArraysChunksOf
#if !defined(mingw32_HOST_OS)
    , groupIOVecsOf
#endif
    , compact
    , compactLE
    , compactEQ
    , compactGE
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Bifunctor (first)
import Foreign.Storable (Storable(..))
#if !defined(mingw32_HOST_OS)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (castPtr)
import Streamly.Internal.FileSystem.FDIO (IOVec(..))
import Streamly.Internal.Data.Array.Foreign.Mut.Types (length)
import Streamly.Internal.Data.SVar (adaptState)
#endif
import Streamly.Internal.Data.Array.Foreign.Mut.Types (Array(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Types as MArray
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (length)

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- > arraysOf n = Stream.foldMany (MArray.writeN n)
--
-- /Internal/
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n = D.fromStreamD . MArray.arraysOf n . D.toStreamD

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

data SpliceState s arr
    = SpliceInitial s
    | SpliceBuffering s arr
    | SpliceYielding arr (SpliceState s arr)
    | SpliceFinish

-- | This mutates the first array (if it has space) to append values from the
-- second one. This would work for immutable arrays as well because an
-- immutable array never has space so a new array is allocated instead of
-- mutating it.
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- @since 0.7.0
{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> D.Stream m (Array a) -> D.Stream m (Array a)
packArraysChunksOf n (D.Stream step state) =
    D.Stream step' (SpliceInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (SpliceInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Types.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
        r <- step gst st
        case r of
            D.Yield arr s -> return $
                let len = MArray.byteLength arr
                 in if len >= n
                    then D.Skip (SpliceYielding arr (SpliceInitial s))
                    else D.Skip (SpliceBuffering s arr)
            D.Skip s -> return $ D.Skip (SpliceInitial s)
            D.Stop -> return D.Stop

    step' gst (SpliceBuffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                let len = MArray.byteLength buf + MArray.byteLength arr
                if len > n
                then return $
                    D.Skip (SpliceYielding buf (SpliceBuffering s arr))
                else do
                    buf' <- if MArray.byteCapacity buf < n
                            then liftIO $ MArray.realloc n buf
                            else return buf
                    buf'' <- MArray.spliceWith buf' arr
                    return $ D.Skip (SpliceBuffering s buf'')
            D.Skip s -> return $ D.Skip (SpliceBuffering s buf)
            D.Stop -> return $ D.Skip (SpliceYielding buf SpliceFinish)

    step' _ SpliceFinish = return D.Stop

    step' _ (SpliceYielding arr next) = return $ D.Yield arr next

{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf n (Fold step1 initial1 extract1 clean1) =
    Fold step initial extract clean

    where

    initial = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Types.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"

        r <- initial1
        return $ first (Tuple' Nothing) r

    extract (Tuple' Nothing r1) = extract1 r1
    extract (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        case r of
            FL.Partial rr -> extract1 rr
            FL.Done _ -> return ()

    clean (Tuple' Nothing r1) = clean1 r1
    clean (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        case r of
            FL.Partial rr -> clean1 rr
            FL.Done _ -> return ()

    step (Tuple' Nothing r1) arr =
            let len = MArray.byteLength arr
             in if len >= n
                then do
                    r <- step1 r1 arr
                    case r of
                        FL.Done _ -> return $ FL.Done ()
                        FL.Partial s -> do
                            clean1 s
                            res <- initial1
                            return $ first (Tuple' Nothing) res
                else return $ FL.Partial $ Tuple' (Just arr) r1

    step (Tuple' (Just buf) r1) arr = do
            let len = MArray.byteLength buf + MArray.byteLength arr
            buf' <- if MArray.byteCapacity buf < len
                    then liftIO $ MArray.realloc (max n len) buf
                    else return buf
            buf'' <- MArray.spliceWith buf' arr

            -- XXX this is common in both the equations of step
            if len >= n
            then do
                r <- step1 r1 buf''
                case r of
                    FL.Done _ -> return $ FL.Done ()
                    FL.Partial s -> do
                        clean1 s
                        res <- initial1
                        return $ first (Tuple' Nothing) res
            else return $ FL.Partial $ Tuple' (Just buf'') r1

-- XXX replace by compactLE
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (MonadIO m, Storable a)
    => Int -> SerialT m (Array a) -> SerialT m (Array a)
compact n xs = D.fromStreamD $ packArraysChunksOf n (D.toStreamD xs)

-- XXX Replace the above functions by a compactLEFold
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- /Unimplemented/
{-# INLINE_NORMAL compactLEFold #-}
compactLEFold :: -- (MonadIO m, Storable a) =>
    Int -> Fold m (Array a) (Array a)
compactLEFold = undefined

compactLE :: (MonadIO m {-, Storable a-}) =>
    Int -> SerialT m (Array a) -> SerialT m (Array a)
compactLE n xs = D.fromStreamD $ D.foldMany (compactLEFold n) (D.toStreamD xs)

-- | Like 'compact' but generates arrays of exactly equal to the size specified
-- except for the last array in the stream which could be shorter.
--
-- /Unimplemented/
{-# INLINE compactEQ #-}
compactEQ :: -- (MonadIO m, Storable a) =>
    Int -> SerialT m (Array a) -> SerialT m (Array a)
compactEQ _n _xs = undefined
    -- D.fromStreamD $ D.foldMany (compactEQFold n) (D.toStreamD xs)

-- | Like 'compact' but generates arrays of size greater than or equal to the
-- specified except for the last array in the stream which could be shorter.
--
-- /Unimplemented/
{-# INLINE compactGE #-}
compactGE :: -- (MonadIO m, Storable a) =>
    Int -> SerialT m (Array a) -> SerialT m (Array a)
compactGE _n _xs = undefined
    -- D.fromStreamD $ D.foldMany (compactGEFold n) (D.toStreamD xs)

-------------------------------------------------------------------------------
-- IOVec
-------------------------------------------------------------------------------

#if !defined(mingw32_HOST_OS)
data GatherState s arr
    = GatherInitial s
    | GatherBuffering s arr Int
    | GatherYielding arr (GatherState s arr)
    | GatherFinish

-- | @groupIOVecsOf maxBytes maxEntries@ groups arrays in the incoming stream
-- to create a stream of 'IOVec' arrays with a maximum of @maxBytes@ bytes in
-- each array and a maximum of @maxEntries@ entries in each array.
--
-- @since 0.7.0
{-# INLINE_NORMAL groupIOVecsOf #-}
groupIOVecsOf :: MonadIO m
    => Int -> Int -> D.Stream m (Array a) -> D.Stream m (Array IOVec)
groupIOVecsOf n maxIOVLen (D.Stream step state) =
    D.Stream step' (GatherInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (GatherInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Types.groupIOVecsOf: the size of "
                 ++ "groups [" ++ show n ++ "] must be a natural number"
        when (maxIOVLen <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Types.groupIOVecsOf: the number of "
                 ++ "IOVec entries [" ++ show n ++ "] must be a natural number"
        r <- step (adaptState gst) st
        case r of
            D.Yield arr s -> do
                let p = unsafeForeignPtrToPtr (aStart arr)
                    len = MArray.byteLength arr
                iov <- liftIO $ MArray.newArray maxIOVLen
                iov' <- liftIO $ MArray.unsafeSnoc iov (IOVec (castPtr p)
                                                (fromIntegral len))
                if len >= n
                then return $ D.Skip (GatherYielding iov' (GatherInitial s))
                else return $ D.Skip (GatherBuffering s iov' len)
            D.Skip s -> return $ D.Skip (GatherInitial s)
            D.Stop -> return D.Stop

    step' gst (GatherBuffering st iov len) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield arr s -> do
                let p = unsafeForeignPtrToPtr (aStart arr)
                    alen = MArray.byteLength arr
                    len' = len + alen
                if len' > n || length iov >= maxIOVLen
                then do
                    iov' <- liftIO $ MArray.newArray maxIOVLen
                    iov'' <- liftIO $ MArray.unsafeSnoc iov' (IOVec (castPtr p)
                                                      (fromIntegral alen))
                    return $ D.Skip (GatherYielding iov
                                        (GatherBuffering s iov'' alen))
                else do
                    iov' <- liftIO $ MArray.unsafeSnoc iov (IOVec (castPtr p)
                                                    (fromIntegral alen))
                    return $ D.Skip (GatherBuffering s iov' len')
            D.Skip s -> return $ D.Skip (GatherBuffering s iov len)
            D.Stop -> return $ D.Skip (GatherYielding iov GatherFinish)

    step' _ GatherFinish = return D.Stop

    step' _ (GatherYielding iov next) = return $ D.Yield iov next
#endif
