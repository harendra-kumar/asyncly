-- |
-- Module      : Streamly.Internal.Data.Array.Stream.Foreign
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of immutable arrays.
--
module Streamly.Internal.Data.Array.Stream.Foreign
    (
    -- * Creation
      arraysOf

    -- * Flattening to elements
    , concat
    , concatRev
    , interpose
    , interposeSuffix
    , intercalateSuffix
    , unlines

    -- * Elimination
    , toArray
    , parse
    , parseD

    -- * Compaction
    , lpackArraysChunksOf
#if !defined(mingw32_HOST_OS)
    , groupIOVecsOf
#endif
    , compact

    -- * Splitting
    , splitOn
    , splitOnSuffix
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr)
-- XXX We should evolve the array module to make it so this should not be
-- required
import Foreign.ForeignPtr (plusForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))
import GHC.Types (SPEC(..))
import Prelude hiding (length, null, last, map, (!!), read, concat, unlines)

#if !defined(mingw32_HOST_OS)
import Streamly.Internal.FileSystem.FDIO (IOVec(..))
#endif
import Streamly.Internal.Data.Array.Foreign.Type (Array(..), length)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.SVar (adaptState, defState)

import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Array.Stream.Mut.Foreign as AS
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D
-- XXX Are these dependencies OK?
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as Generate
import qualified Streamly.Internal.Data.Stream.StreamD.Nesting as Nesting

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- > arraysOf n = Stream.chunksOf n (Array.writeN n)
--
-- /Pre-release/
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n str = D.fromStreamD $ A.arraysOf n (D.toStreamD str)

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- XXX efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-- | Convert a stream of arrays into a stream of their elements.
--
-- Same as the following but more efficient:
--
-- > concat = Stream.unfoldMany Array.read
--
-- @since 0.7.0
{-# INLINE concat #-}
concat :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
-- concat m = D.fromStreamD $ A.flattenArrays (D.toStreamD m)
-- concat m = D.fromStreamD $ D.concatMap A.toStreamD (D.toStreamD m)
concat m = D.fromStreamD $ D.unfoldMany A.read (D.toStreamD m)

-- | Convert a stream of arrays into a stream of their elements reversing the
-- contents of each array before flattening.
--
-- > concatRev = Stream.unfoldMany Array.readRev
--
-- @since 0.7.0
{-# INLINE concatRev #-}
concatRev :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
concatRev m = D.fromStreamD $ A.flattenArraysRev (D.toStreamD m)

-------------------------------------------------------------------------------
-- Intersperse and append
-------------------------------------------------------------------------------

-- | Flatten a stream of arrays after inserting the given element between
-- arrays.
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: (MonadIO m, IsStream t, Storable a) => a -> t m (Array a) -> t m a
interpose x = S.interpose x A.read

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (MonadIO m, IsStream t, Storable a)
    => Array a -> t m (Array a) -> t m a
intercalateSuffix arr = S.intercalateSuffix arr A.read

-- | Flatten a stream of arrays appending the given element after each
-- array.
--
-- @since 0.7.0
{-# INLINE interposeSuffix #-}
interposeSuffix :: (MonadIO m, IsStream t, Storable a)
    => a -> t m (Array a) -> t m a
-- interposeSuffix x = D.fromStreamD . A.unlines x . D.toStreamD
interposeSuffix x = S.interposeSuffix x A.read

data FlattenState s a =
      OuterLoop s
    | InnerLoop s !(ForeignPtr a) !(Ptr a) !(Ptr a)

{-# INLINE_NORMAL unlines #-}
unlines :: forall m a. (MonadIO m, Storable a)
    => a -> D.Stream m (Array a) -> D.Stream m a
unlines sep (D.Stream step state) = D.Stream step' (OuterLoop state)
    where
    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield Array{..} s ->
                let p = unsafeForeignPtrToPtr aStart
                in D.Skip (InnerLoop s aStart p aEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | p == end =
        return $ D.Yield sep $ OuterLoop st

    step' _ (InnerLoop st startf p end) = do
        x <- liftIO $ do
                    r <- peek p
                    touchForeignPtr startf
                    return r
        return $ D.Yield x (InnerLoop st startf
                            (p `plusPtr` sizeOf (undefined :: a)) end)

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> D.Stream m (Array a) -> D.Stream m (Array a)
packArraysChunksOf n str =
    D.map A.unsafeFreeze $ AS.packArraysChunksOf n $ D.map A.unsafeThaw str

-- XXX instead of writing two different versions of this operation, we should
-- write it as a pipe.
{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Storable a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf n fld =
    FL.map A.unsafeThaw $ AS.lpackArraysChunksOf n (FL.map A.unsafeFreeze fld)

#if !defined(mingw32_HOST_OS)

-- | @groupIOVecsOf maxBytes maxEntries@ groups arrays in the incoming stream
-- to create a stream of 'IOVec' arrays with a maximum of @maxBytes@ bytes in
-- each array and a maximum of @maxEntries@ entries in each array.
--
-- @since 0.7.0
{-# INLINE_NORMAL groupIOVecsOf #-}
groupIOVecsOf :: MonadIO m
    => Int -> Int -> D.Stream m (Array a) -> D.Stream m (Array IOVec)
groupIOVecsOf n maxIOVLen str =
    D.map A.unsafeFreeze
        $ AS.groupIOVecsOf n maxIOVLen
        $ D.map A.unsafeThaw str
#endif

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (MonadIO m, Storable a)
    => Int -> SerialT m (Array a) -> SerialT m (Array a)
compact n xs = D.fromStreamD $ packArraysChunksOf n (D.toStreamD xs)

-------------------------------------------------------------------------------
-- Split
-------------------------------------------------------------------------------

data SplitState s arr
    = Initial s
    | Buffering s arr
    | Splitting s arr
    | Yielding arr (SplitState s arr)
    | Finishing

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE_NORMAL _splitOn #-}
_splitOn
    :: MonadIO m
    => Word8
    -> D.Stream m (Array Word8)
    -> D.Stream m (Array Word8)
_splitOn byte (D.Stream step state) = D.Stream step' (Initial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Initial st) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                (arr1, marr2) <- A.breakOn byte arr
                return $ case marr2 of
                    Nothing   -> D.Skip (Buffering s arr1)
                    Just arr2 -> D.Skip (Yielding arr1 (Splitting s arr2))
            D.Skip s -> return $ D.Skip (Initial s)
            D.Stop -> return D.Stop

    step' gst (Buffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                (arr1, marr2) <- A.breakOn byte arr
                buf' <- A.spliceTwo buf arr1
                return $ case marr2 of
                    Nothing -> D.Skip (Buffering s buf')
                    Just x -> D.Skip (Yielding buf' (Splitting s x))
            D.Skip s -> return $ D.Skip (Buffering s buf)
            D.Stop -> return $
                if A.byteLength buf == 0
                then D.Stop
                else D.Skip (Yielding buf Finishing)

    step' _ (Splitting st buf) = do
        (arr1, marr2) <- A.breakOn byte buf
        return $ case marr2 of
                Nothing -> D.Skip $ Buffering st arr1
                Just arr2 -> D.Skip $ Yielding arr1 (Splitting st arr2)

    step' _ (Yielding arr next) = return $ D.Yield arr next
    step' _ Finishing = return D.Stop

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, MonadIO m)
    => Word8
    -> t m (Array Word8)
    -> t m (Array Word8)
splitOn byte s =
    D.fromStreamD $ D.splitInnerBy (A.breakOn byte) A.spliceTwo $ D.toStreamD s

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, MonadIO m)
    => Word8
    -> t m (Array Word8)
    -> t m (Array Word8)
-- splitOn byte s = D.fromStreamD $ A.splitOn byte $ D.toStreamD s
splitOnSuffix byte s =
    D.fromStreamD $ D.splitInnerBySuffix (A.breakOn byte) A.spliceTwo $ D.toStreamD s

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

-- CAUTION! length must more than equal to lengths of all the arrays in the
-- stream.
{-# INLINE spliceArraysLenUnsafe #-}
spliceArraysLenUnsafe :: (MonadIO m, Storable a)
    => Int -> SerialT m (MA.Array a) -> m (MA.Array a)
spliceArraysLenUnsafe len buffered = do
    arr <- liftIO $ MA.newArray len
    end <- S.foldlM' writeArr (return $ MA.aEnd arr) buffered
    return $ arr {MA.aEnd = end}

    where

    writeArr dst (MA.Array as ae _) =
        liftIO $ withForeignPtr as $ \src -> do
                        let count = ae `minusPtr` src
                        A.memcpy (castPtr dst) (castPtr src) count
                        return $ dst `plusPtr` count

{-# INLINE _spliceArrays #-}
_spliceArrays :: (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
_spliceArrays s = do
    buffered <- S.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)
    arr <- liftIO $ MA.newArray len
    end <- S.foldlM' writeArr (return $ MA.aEnd arr) s
    return $ A.unsafeFreeze $ arr {MA.aEnd = end}

    where

    writeArr dst (Array as ae) =
        liftIO $ withForeignPtr as $ \src -> do
                        let count = ae `minusPtr` src
                        A.memcpy (castPtr dst) (castPtr src) count
                        return $ dst `plusPtr` count

{-# INLINE _spliceArraysBuffered #-}
_spliceArraysBuffered :: (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
_spliceArraysBuffered s = do
    buffered <- S.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)
    A.unsafeFreeze <$> spliceArraysLenUnsafe len (S.map A.unsafeThaw s)

{-# INLINE spliceArraysRealloced #-}
spliceArraysRealloced :: forall m a. (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
spliceArraysRealloced s = do
    let idst = liftIO $ MA.newArray (A.bytesToElemCount (undefined :: a)
                                  (A.mkChunkSizeKB 4))

    arr <- S.foldlM' MA.spliceWithDoubling idst (S.map A.unsafeThaw s)
    liftIO $ A.unsafeFreeze <$> MA.shrinkToFit arr

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: (MonadIO m, Storable a) => SerialT m (Array a) -> m (Array a)
toArray = spliceArraysRealloced
-- spliceArrays = _spliceArraysBuffered

-- exponentially increasing sizes of the chunks upto the max limit.
-- XXX this will be easier to implement with parsers/terminating folds
-- With this we should be able to reduce the number of chunks/allocations.
-- The reallocation/copy based toArray can also be implemented using this.
--
{-
{-# INLINE toArraysInRange #-}
toArraysInRange :: (IsStream t, MonadIO m, Storable a)
    => Int -> Int -> Fold m (Array a) b -> Fold m a b
toArraysInRange low high (Fold step initial extract) =
-}

{-
-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE _toArraysOf #-}
_toArraysOf :: (MonadIO m, Storable a)
    => Int -> Fold m a (SerialT Identity (Array a))
_toArraysOf n = FL.chunksOf n (A.writeNF n) FL.toStream
-}

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- We use a doubly linked list for parseArray. Below is the type (DLL) a few
-- helper functions (*DLL)

-- Representation of a doubly linked list
{-# ANN type DLL NoSpecConstr #-}
newtype DLL a = DLL ([a], a, [a])

{-# INLINE singleDLL #-}
singleDLL :: a -> DLL a
singleDLL a = DLL ([], a, [])

{-# INLINE bufferRDLL #-}
bufferRDLL :: DLL a -> [a]
bufferRDLL (DLL (_, _, br)) = br

{-# INLINE dropLDLL #-}
dropLDLL :: DLL a -> DLL a
dropLDLL (DLL (_, a, x)) = (DLL ([], a, x))

{-# INLINE nextDLL #-}
nextDLL :: DLL a -> DLL a
nextDLL (DLL (y, a, x:xs)) = (DLL (a:y, x, xs))
nextDLL _ = error "nextDLL: Empty right buffer"

{-# INLINE previousDLL #-}
previousDLL :: DLL a -> DLL a
previousDLL (DLL (y:ys, a, x)) = (DLL (ys, y, a:x))
previousDLL _ = error "previousDLL: Empty left buffer"

{-# INLINE currentDLL #-}
currentDLL :: DLL a -> a
currentDLL (DLL (_, x, _)) = x

-- Inefficient stuff but probably Ok for our use case.
{-# INLINE insertDLL #-}
insertDLL :: a -> DLL a -> DLL a
insertDLL a (DLL (y, a1, x)) = (DLL (y, a1, x ++ [a]))

{-# INLINE_NORMAL parseD #-}
parseD ::
       forall m a b. (MonadIO m, Storable a)
    => PRD.Parser m a b
    -> D.Stream m (Array.Array a)
    -> m (b, D.Stream m (Array.Array a))
parseD (PRD.Parser pstep pinitial pextract) strm@(D.UnStream step state) = do
    res <- pinitial
    case res of
        PRD.IPartial ps -> goInit SPEC ps state
        PRD.IDone b -> return (b, strm)
        PRD.IError err -> error err

    where

    goInit !_ ps s = do
        res <- step defState s
        case res of
            D.Yield arr s1 ->
                go SPEC ps s1 (singleDLL arr) 0 (Array.length arr)
            D.Skip s1 -> goInit SPEC ps s1
            D.Stop -> do
                b <- pextract ps
                return (b, Generate.nil)

    goBreak !_ ps s dl i imax
        | i < 0 =
            let dl1 = previousDLL dl
                arr = currentDLL dl1
                len = Array.length arr
             in goBreak SPEC ps s dl1 (len + i) len
        | otherwise = go SPEC ps s (dropLDLL dl) i imax

    go !_ ps s dl i imax
        | i == imax = do
            res <- step defState s
            case res of
                D.Yield arr s1 -> do
                    let dl1 = nextDLL (insertDLL arr dl)
                        crr = currentDLL dl1
                    go SPEC ps s1 dl1 0 (Array.length crr)
                D.Skip s1 -> go SPEC ps s1 dl i imax
                D.Stop -> do
                    b <- pextract ps
                    return (b, Generate.nil)
        | i < 0 =
            let dl1 = previousDLL dl
                arr = currentDLL dl1
                len = Array.length arr
             in go SPEC ps s dl1 (len + i) len
        | otherwise = do
            let arr = currentDLL dl
                i1 = i + 1
            a <- liftIO $ Array.unsafeIndexIO arr i
            pRes <- pstep ps a
            case pRes of
                PRD.Partial 0 ps1 -> go SPEC ps1 s (dropLDLL dl) i1 imax
                PRD.Partial n ps1 -> goBreak SPEC ps1 s dl (i1 - n) imax
                PRD.Continue 0 ps1 -> go SPEC ps1 s dl i1 imax
                PRD.Continue n ps1 -> go SPEC ps1 s dl (i1 - n) imax
                PRD.Done n b -> goFinal SPEC b s dl (i1 - n) imax
                PRD.Error err -> error err

    goFinal !_ b s dl i imax
        | i == imax =
            let br = bufferRDLL dl
                streamL = Generate.fromList br
                streamR = D.Stream step s
             in return (b, Nesting.append streamL streamR)
        | i < 0 =
            let dl1 = previousDLL dl
                arr = currentDLL dl1
                len = Array.length arr
             in goFinal SPEC b s dl1 (len + i) len
        | otherwise = do
            let (Array.Array fp end) = currentDLL dl
                br = bufferRDLL dl
                elemSize = sizeOf (undefined :: a)
                newArr = Array.Array (fp `plusForeignPtr` (i * elemSize)) end
                streamL = Generate.fromList (newArr : br)
                streamR = D.Stream step s
             in return (b, Nesting.append streamL streamR)

-- | Parse an array stream using the supplied 'Parser'.
--
-- /Internal/
--
{-# INLINE_NORMAL parse #-}
parse ::
       (MonadIO m, Storable a)
    => PRD.Parser m a b
    -> SerialT m (A.Array a)
    -> m (b, SerialT m (A.Array a))
parse p s = fmap D.fromStreamD <$> parseD p (D.toStreamD s)
