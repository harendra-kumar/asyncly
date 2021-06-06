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
    -- ** Element Folds
    , fold
    , parse
    , parseD

    -- ** Array Folds
    , foldArr
    , foldArr_
    -- , parseArr
    , parseArrD
    , foldArrMany

    , toArray

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

import Data.Bifunctor (second)
import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))
import GHC.Types (SPEC(..))
import Prelude hiding (null, last, (!!), read, concat, unlines)

#if !defined(mingw32_HOST_OS)
import Streamly.Internal.FileSystem.FDIO (IOVec(..))
#endif
import Streamly.Internal.Data.Array.Foreign.Type (Array(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.SVar (adaptState, defState)

import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Array.Stream.Mut.Foreign as AS
import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as ASF
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D

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
intercalateSuffix = S.intercalateSuffix A.read

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
-- Elimination - Running folds
-------------------------------------------------------------------------------

-- XXX This should be written using CPS (as foldK) if we want it to scale wrt
-- to the number of times it can be called on the same stream.
--
{-# INLINE_NORMAL foldD #-}
foldD :: forall m a b. (MonadIO m, Storable a) =>
    Fold m a b -> D.Stream m (Array a) -> m (b, D.Stream m (Array a))
foldD (Fold fstep initial extract) stream@(D.Stream step state) = do
    res <- initial
    case res of
        FL.Partial fs -> go SPEC state fs
        FL.Done fb -> return $! (fb, stream)

    where

    {-# INLINE go #-}
    go !_ st !fs = do
        r <- step defState st
        case r of
            D.Yield (Array (ForeignPtr start contents) (Ptr end)) s ->
                goArray SPEC s (ForeignPtr end contents) (Ptr start) fs
            D.Skip s -> go SPEC s fs
            D.Stop -> do
                b <- extract fs
                return (b, D.nil)

    goArray !_ s fp@(ForeignPtr end _) !cur !fs
        | cur == Ptr end = do
            liftIO $ touchForeignPtr fp
            go SPEC s fs
    goArray !_ st fp@(ForeignPtr end contents) !cur !fs = do
        x <- liftIO $ peek cur
        res <- fstep fs x
        let elemSize = sizeOf (undefined :: a)
            next = cur `plusPtr` elemSize
        case res of
            FL.Done b -> do
                let !(Ptr curAddr) = next
                    arr = Array (ForeignPtr curAddr contents) (Ptr end)
                return $! (b, D.cons arr (D.Stream step st))
            FL.Partial fs1 -> goArray SPEC st fp next fs1

-- | Fold an array stream using the supplied 'Fold'. Returns the fold result
-- and the unconsumed stream.
--
-- /Internal/
--
{-# INLINE_NORMAL fold #-}
fold ::
       (MonadIO m, Storable a)
    => FL.Fold m a b
    -> SerialT m (A.Array a)
    -> m (b, SerialT m (A.Array a))
fold f s = fmap D.fromStreamD <$> foldD f (D.toStreamD s)

-------------------------------------------------------------------------------
-- Fold to a single Array
-------------------------------------------------------------------------------

-- When we have to take an array partially, take the last part of the array.
{-# INLINE takeArrayListRev #-}
takeArrayListRev :: forall a. Storable a => Int -> [Array a] -> [Array a]
takeArrayListRev = go

    where

    go _ [] = []
    go n _ | n <= 0 = []
    go n (x:xs) =
        let len = Array.length x
        in if n > len
           then x : go (n - len) xs
           else if n == len
           then [x]
           else let !(Array (ForeignPtr _ contents) end) = x
                    sz = sizeOf (undefined :: a)
                    !(Ptr start) = end `plusPtr` negate (n * sz)
                 in [Array (ForeignPtr start contents) end]

-- When we have to take an array partially, take the last part of the array in
-- the first split.
{-# INLINE splitAtArrayListRev #-}
splitAtArrayListRev :: forall a. Storable a =>
    Int -> [Array a] -> ([Array a],[Array a])
splitAtArrayListRev n ls
  | n <= 0 = ([], ls)
  | otherwise = go n ls
    where
        go :: Int -> [Array a] -> ([Array a], [Array a])
        go _  []     = ([], [])
        go m (x:xs) =
            let len = Array.length x
                (xs', xs'') = go (m - len) xs
             in if m > len
                then (x:xs', xs'')
                else if m == len
                then ([x],xs)
                else let !(Array (ForeignPtr start contents) end) = x
                         sz = sizeOf (undefined :: a)
                         end1 = end `plusPtr` negate (m * sz)
                         arr2 = Array (ForeignPtr start contents) end1
                         !(Ptr addrEnd1) = end1
                         arr1 = Array (ForeignPtr addrEnd1 contents) end
                      in ([arr1], arr2:xs)

-------------------------------------------------------------------------------
-- Fold to a single Array
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
    len <- S.sum (S.map Array.length buffered)
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
    len <- S.sum (S.map Array.length buffered)
    A.unsafeFreeze <$> spliceArraysLenUnsafe len (S.map A.unsafeThaw s)

{-# INLINE spliceArraysRealloced #-}
spliceArraysRealloced :: forall m a. (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
spliceArraysRealloced s = do
    let idst = liftIO $ MA.newArray (A.bytesToElemCount (undefined :: a)
                                  (A.mkChunkSizeKB 4))

    arr <- S.foldlM' MA.spliceWithDoubling idst (S.map A.unsafeThaw s)
    liftIO $ A.unsafeFreeze <$> MA.shrinkToFit arr

-- XXX This should just be "fold A.write"
--
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
-- Elimination - running element parsers
-------------------------------------------------------------------------------

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

-- This can be generalized to any type provided it can be unfolded to a stream
-- and it can be combined using a semigroup operation.
--
-- XXX This should be written using CPS (as parseK) if we want it to scale wrt
-- to the number of times it can be called on the same stream.
{-# INLINE_NORMAL parseD #-}
parseD ::
       forall m a b. (MonadIO m, MonadThrow m, Storable a)
    => PRD.Parser m a b
    -> D.Stream m (Array.Array a)
    -> m (b, D.Stream m (Array.Array a))
parseD (PRD.Parser pstep initial extract) stream@(D.Stream step state) = do
    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (b, stream)
        PRD.IError err -> throwM $ ParseError err

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st backBuf !pst = do
        r <- step defState st
        case r of
            D.Yield (Array (ForeignPtr start contents) (Ptr end)) s ->
                gobuf SPEC s backBuf (ForeignPtr end contents) (Ptr start) pst
            D.Skip s -> go SPEC s backBuf pst
            D.Stop -> do
                b <- extract pst
                return (b, D.nil)

    -- Use strictness on "cur" to keep it unboxed
    gobuf !_ s backBuf fp@(ForeignPtr end _) !cur !pst
        | cur == Ptr end = do
            liftIO $ touchForeignPtr fp
            go SPEC s backBuf pst
    gobuf !_ s backBuf fp@(ForeignPtr end contents) !cur !pst = do
        x <- liftIO $ peek cur
        pRes <- pstep pst x
        let elemSize = sizeOf (undefined :: a)
            next = cur `plusPtr` elemSize
        case pRes of
            PR.Partial 0 pst1 ->
                 gobuf SPEC s (List []) fp next pst1
            PR.Partial n pst1 -> do
                assert (n <= Prelude.length (x:getList backBuf)) (return ())
                let src0 = Prelude.take n (x:getList backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    !(Ptr curAddr) = next
                    arr1 = Array (ForeignPtr curAddr contents) (Ptr end)
                    src = arr0 <> arr1
                let !(Array (ForeignPtr start cont1) (Ptr end1)) = src
                gobuf SPEC s (List []) (ForeignPtr end1 cont1) (Ptr start) pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList backBuf)) fp next pst1
            PR.Continue n pst1 -> do
                assert (n <= Prelude.length (x:getList backBuf)) (return ())
                let (src0, buf1) = splitAt n (x:getList backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    !(Ptr curAddr) = next
                    arr1 = Array (ForeignPtr curAddr contents) (Ptr end)
                    src = arr0 <> arr1
                let !(Array (ForeignPtr start cont1) (Ptr end1)) = src
                gobuf SPEC s (List buf1) (ForeignPtr end1 cont1) (Ptr start) pst1
            PR.Done 0 b -> do
                let !(Ptr curAddr) = next
                    arr = Array (ForeignPtr curAddr contents) (Ptr end)
                return (b, D.cons arr (D.Stream step s))
            PR.Done n b -> do
                assert (n <= Prelude.length (x:getList backBuf)) (return ())
                let src0 = Prelude.take n (x:getList backBuf)
                    -- XXX create the array in reverse instead
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    !(Ptr curAddr) = next
                    arr1 = Array (ForeignPtr curAddr contents) (Ptr end)
                    -- XXX Use StreamK to avoid adding arbitrary layers of
                    -- constructors every time.
                    str = D.cons arr0 (D.cons arr1 (D.Stream step s))
                return (b, str)
            PR.Error err -> throwM $ ParseError err

-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- /Internal/
--
{-# INLINE_NORMAL parse #-}
parse ::
       (MonadIO m, MonadThrow m, Storable a)
    => PRD.Parser m a b
    -> SerialT m (A.Array a)
    -> m (b, SerialT m (A.Array a))
parse p s = fmap D.fromStreamD <$> parseD p (D.toStreamD s)

-------------------------------------------------------------------------------
-- Elimination - Running Array Folds and parsers
-------------------------------------------------------------------------------

{-# INLINE_NORMAL parseArrD #-}
parseArrD ::
       forall m a b. (MonadIO m, MonadThrow m, Storable a)
    => PRD.Parser m (Array a) b
    -> D.Stream m (Array.Array a)
    -> m (b, D.Stream m (Array.Array a))
parseArrD (PRD.Parser pstep initial extract) stream@(D.Stream step state) = do
    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (b, stream)
        PRD.IError err -> throwM $ ParseError err

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st backBuf !pst = do
        r <- step defState st
        case r of
            D.Yield x s -> gobuf SPEC [x] s backBuf pst
            D.Skip s -> go SPEC s backBuf pst
            D.Stop -> do
                b <- extract pst
                return (b, D.nil)

    gobuf !_ [] s backBuf !pst = go SPEC s backBuf pst
    gobuf !_ (x:xs) s backBuf !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                 gobuf SPEC xs s (List []) pst1
            PR.Partial n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC src s (List []) pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC xs s (List (x:getList backBuf)) pst1
            PR.Continue n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC src s (List buf1) pst1
            PR.Done 0 b ->
                return (b, D.Stream step s)
            PR.Done n b -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src = Prelude.reverse src0 ++ xs
                return (b, D.append (D.fromList src) (D.Stream step s))
            PR.Error err -> throwM $ ParseError err

{-
-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- /Internal/
--
{-# INLINE parseArr #-}
parseArr ::
       (MonadIO m, MonadThrow m, Storable a)
    => ASF.Parser m a b
    -> SerialT m (A.Array a)
    -> m (b, SerialT m (A.Array a))
parseArr p s = fmap D.fromStreamD <$> parseD p (D.toStreamD s)
-}

-- | Fold an array stream using the supplied array stream 'Fold'.
--
-- /Pre-release/
--
{-# INLINE foldArr #-}
foldArr :: (MonadIO m, MonadThrow m, Storable a) =>
    ASF.Fold m a b -> SerialT m (A.Array a) -> m b
foldArr (ASF.Fold p) s = fst <$> parseArrD p (D.toStreamD s)

-- | Like 'fold' but also returns the remaining stream.
--
-- /Pre-release/
--
{-# INLINE foldArr_ #-}
foldArr_ :: (MonadIO m, MonadThrow m, Storable a) =>
    ASF.Fold m a b -> SerialT m (A.Array a) -> m (b, SerialT m (A.Array a))
foldArr_ (ASF.Fold p) s = second D.fromStreamD <$> parseArrD p (D.toStreamD s)

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit inpBuf st
    | ParseChunksInitLeftOver inpBuf
    | ParseChunksStream st inpBuf !pst
    | ParseChunksBuf inpBuf st inpBuf !pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

{-# INLINE_NORMAL foldArrManyD #-}
foldArrManyD
    :: (MonadThrow m, Storable a)
    => ASF.Fold m a b
    -> D.Stream m (Array a)
    -> D.Stream m b
foldArrManyD (ASF.Fold (PRD.Parser pstep initial extract)) (D.Stream step state) =
    D.Stream stepOuter (ParseChunksInit [] state)

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit [] st) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                res <- initial
                case res of
                    PRD.IPartial ps ->
                        return $ D.Skip $ ParseChunksBuf [x] s [] ps
                    PRD.IDone pb ->
                        let next = ParseChunksInit [x] s
                         in return $ D.Skip $ ParseChunksYield pb next
                    PRD.IError err -> throwM $ ParseError err
            D.Skip s -> return $ D.Skip $ ParseChunksInit [] s
            D.Stop   -> return D.Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit src st) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ D.Skip $ ParseChunksBuf src st [] ps
            PRD.IDone pb ->
                let next = ParseChunksInit src st
                 in return $ D.Skip $ ParseChunksYield pb next
            PRD.IError err -> throwM $ ParseError err

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver _) = return D.Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream st backBuf pst) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ D.Skip $ ParseChunksStream s [] pst1
                    PR.Partial n pst1 -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let src0 = takeArrayListRev n (x:backBuf)
                            src  = Prelude.reverse src0
                        return $ D.Skip $ ParseChunksBuf src s [] pst1
                    PR.Continue 0 pst1 ->
                        return $ D.Skip $ ParseChunksStream s (x:backBuf) pst1
                    PR.Continue n pst1 -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                            src  = Prelude.reverse src0
                        return $ D.Skip $ ParseChunksBuf src s buf1 pst1
                    PR.Done 0 b -> do
                        return $ D.Skip $
                            ParseChunksYield b (ParseChunksInit [] s)
                    PR.Done n b -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let src0 = takeArrayListRev n (x:backBuf)
                        let src = Prelude.reverse src0
                        return $ D.Skip $
                            ParseChunksYield b (ParseChunksInit src s)
                    PR.Error err -> throwM $ ParseError err
            D.Skip s -> return $ D.Skip $ ParseChunksStream s backBuf pst
            D.Stop   -> do
                b <- extract pst
                let src = Prelude.reverse backBuf
                return $ D.Skip $
                    ParseChunksYield b (ParseChunksInitLeftOver src)

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf [] s buf pst) =
        return $ D.Skip $ ParseChunksStream s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf (x:xs) s backBuf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ D.Skip $ ParseChunksBuf xs s [] pst1
            PR.Partial n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksBuf src s [] pst1
            PR.Continue 0 pst1 ->
                return $ D.Skip $ ParseChunksBuf xs s (x:backBuf) pst1
            PR.Continue n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksBuf src s buf1 pst1
            PR.Done 0 b ->
                return $ D.Skip $ ParseChunksYield b (ParseChunksInit xs s)
            PR.Done n b -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksYield b (ParseChunksInit src s)
            PR.Error err -> throwM $ ParseError err

    stepOuter _ (ParseChunksYield a next) = return $ D.Yield a next

-- | Apply an array stream 'Fold' repeatedly on an array stream and emit the
-- fold outputs in the output stream.
--
-- See "Streamly.Prelude.foldMany" for more details.
--
-- /Pre-release/
{-# INLINE foldArrMany #-}
foldArrMany
    :: (IsStream t, MonadThrow m, Storable a)
    => ASF.Fold m a b
    -> t m (Array a)
    -> t m b
foldArrMany p m = D.fromStreamD $ foldArrManyD p (D.toStreamD m)
