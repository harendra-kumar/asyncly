#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.Handle
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.FileSystem.Handle
    (
    -- ** Read from Handle
      read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , readWithBufferOf

    , toBytes
    , toBytesWithBufferOf
    , getBytes

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf
    , readChunks
    , readChunksWithBufferOf

    , toChunksWithBufferOf
    , toChunks
    , getChunks

    -- ** Write to Handle
    -- Byte stream write (Folds)
    , write
    , write2
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    -- , writeLines
    , writeWithBufferOf

    -- Byte stream write (Streams)
    , fromBytes
    , fromBytesWithBufferOf

    -- -- * Array Write
    , writeArray
    , writeChunks
    , writeChunksWithBufferOf

    -- -- * Array stream Write
    , fromChunksWithBufferOf
    , fromChunks
    , putChunks
    , putStrings
    , putBytes
    , putLines

    -- -- * Random Access (Seek)
    -- -- | Unlike the streaming APIs listed above, these APIs apply to devices or
    -- files that have random access or seek capability.  This type of devices
    -- include disks, files, memory devices and exclude terminals, pipes,
    -- sockets and fifos.
    --
    -- XXX need to decide whether to use readFromStepN style or readFromThenTo
    -- style. The latter is consistent with list enumeration. The former may be
    -- more unambiguous and it is easier and clearer to specify 0 elements.
    --
    -- We can also generate the request pattern using a funciton.
    --
    -- , readIndex
    -- , readFrom -- read from a given position to the end of file
    -- , readFromRev -- read from a given position the beginning of file
    -- , readTo   -- read from beginning up to the given position
    -- , readToRev -- read from end to the given position in file
    -- , readFromTo
    -- , readFromThenTo

    -- , readChunksFrom
    -- , readChunksFromTo
    -- , readChunksFromToWithBufferOf
    -- , readChunksFromThenToWithBufferOf
    , readSliceWith
    , writeSlice

    -- , writeIndex
    -- , writeFrom -- start writing at the given position
    -- , writeFromRev
    -- , writeTo   -- write from beginning up to the given position
    -- , writeToRev
    -- , writeFromTo
    -- , writeFromThenTo
    --
    -- , writeChunksFrom
    -- , writeChunksFromTo
    -- , writeChunksFromToWithBufferOf
    -- , writeChunksFromThenToWithBufferOf
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import System.IO 
    (Handle, SeekMode(..), hGetBufSome, hPutBuf, hSeek, stdin, stdout) 
import Prelude hiding (read)
import Streamly.Prelude (MonadAsync)
import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Fold.Types (Fold2(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Array.Storable.Foreign.Types
       (Array(..), writeNUnsafe, defaultChunkSize, lpackArraysChunksOf
       , unsafeFreezeWithShrink)
import Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types (mutableArray)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream, mkStream)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Array.Storable.Foreign as IA
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

-------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------
--
-- The following references may be useful to build an understanding about the
-- file API design:
--
-- http://www.linux-mag.com/id/308/ for blocking/non-blocking IO on linux.
-- https://lwn.net/Articles/612483/ Non-blocking buffered file read operations
-- https://en.wikipedia.org/wiki/C_file_input/output for C APIs.
-- https://docs.oracle.com/javase/tutorial/essential/io/file.html for Java API.
-- https://www.w3.org/TR/FileAPI/ for http file API.

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINABLE readArrayUpto #-}
readArrayUpto :: Int -> Handle -> IO (Array Word8)
readArrayUpto size h = do
    ptr <- mallocPlainForeignPtrBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    withForeignPtr ptr $ \p -> do
        n <- hGetBufSome h p size
        -- XXX shrink only if the diff is significant
        return $
            unsafeFreezeWithShrink $
            mutableArray ptr (p `plusPtr` n) (p `plusPtr` size)

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @toChunksWithBufferOf size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
{-# INLINABLE _toChunksWithBufferOf #-}
_toChunksWithBufferOf :: (IsStream t, MonadIO m)
    => Int -> Handle -> t m (Array Word8)
_toChunksWithBufferOf size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ readArrayUpto size h
        if A.length arr == 0
        then stp
        else yld arr go

-- | @toChunksWithBufferOf size handle@ reads a stream of arrays from the file
-- handle @handle@.  The maximum size of a single array is limited to @size@.
-- The actual size read may be less than or equal to @size@.
--
-- @since 0.7.0
{-# INLINE_NORMAL toChunksWithBufferOf #-}
toChunksWithBufferOf :: (IsStream t, MonadIO m) => Int -> Handle -> t m (Array Word8)
toChunksWithBufferOf size h = D.fromStreamD (D.Stream step ())
  where
    {-# INLINE_LATE step #-}
    step _ _ = do
        arr <- liftIO $ readArrayUpto size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr ()

-- | Unfold the tuple @(bufsize, handle)@ into a stream of 'Word8' arrays.
-- Read requests to the IO device are performed using a buffer of size
-- @bufsize@.  The size of an array in the resulting stream is always less than
-- or equal to @bufsize@.
--
-- @since 0.7.0
{-# INLINE_NORMAL readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
readChunksWithBufferOf = Unfold step return
    where
    {-# INLINE_LATE step #-}
    step (size, h) = do
        arr <- liftIO $ readArrayUpto size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr (size, h)

-- XXX read 'Array a' instead of Word8
--
-- | @toChunks handle@ reads a stream of arrays from the specified file
-- handle.  The maximum size of a single array is limited to
-- @defaultChunkSize@. The actual size read may be less than or equal to
-- @defaultChunkSize@.
--
-- > toChunks = toChunksWithBufferOf defaultChunkSize
--
-- @since 0.7.0
{-# INLINE toChunks #-}
toChunks :: (IsStream t, MonadIO m) => Handle -> t m (Array Word8)
toChunks = toChunksWithBufferOf defaultChunkSize

-- | Read a stream of chunks from standard input.  The maximum size of a single
-- chunk is limited to @defaultChunkSize@. The actual size read may be less
-- than @defaultChunkSize@.
--
-- > getChunks = toChunks stdin
--
-- /Internal/
--
{-# INLINE getChunks #-}
getChunks :: (IsStream t, MonadIO m) => t m (Array Word8)
getChunks = toChunks stdin

-- | Read a stream of bytes from standard input.
--
-- > getBytes = toBytes stdin
--
-- /Internal/
--
{-# INLINE getBytes #-}
getBytes :: (IsStream t, MonadIO m) => t m Word8
getBytes = toBytes stdin

-- | Unfolds a handle into a stream of 'Word8' arrays. Requests to the IO
-- device are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Storable.Foreign.Types.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Storable.Foreign.Types.defaultChunkSize'.
--
-- @since 0.7.0
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m Handle (Array Word8)
readChunks = UF.supplyFirst readChunksWithBufferOf defaultChunkSize

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Unfolds the tuple @(bufsize, handle)@ into a byte stream, read requests
-- to the IO device are performed using buffers of @bufsize@.
--
-- @since 0.7.0
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Handle) Word8
readWithBufferOf = UF.concat readChunksWithBufferOf A.read

-- | @toBytesWithBufferOf bufsize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @bufsize@.
--
-- /Internal/
{-# INLINE toBytesWithBufferOf #-}
toBytesWithBufferOf :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
toBytesWithBufferOf chunkSize h = AS.concat $ toChunksWithBufferOf chunkSize h

-- TODO
-- Generate a stream of elements of the given type from a file 'Handle'.
-- read :: (IsStream t, MonadIO m, Storable a) => Handle -> t m a
--
-- | Unfolds a file handle into a byte stream. IO requests to the device are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Storable.Foreign.Types.defaultChunkSize'.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: MonadIO m => Unfold m Handle Word8
read = UF.supplyFirst readWithBufferOf defaultChunkSize

-- | Generate a byte stream from a file 'Handle'.
--
-- /Internal/
{-# INLINE toBytes #-}
toBytes :: (IsStream t, MonadIO m) => Handle -> t m Word8
toBytes = AS.concat . toChunks

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an 'Array' to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Storable a => Handle -> Array a -> IO ()
writeArray _ arr | A.length arr == 0 = return ()
writeArray h Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- XXX use an unfold to fromObjects or fromUnfold so that we can put any object
-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE fromChunks #-}
fromChunks :: (MonadIO m, Storable a)
    => Handle -> SerialT m (Array a) -> m ()
fromChunks h = S.mapM_ (liftIO . writeArray h)

-- | Write a stream of chunks to standard output.
--
-- /Internal/
--
{-# INLINE putChunks #-}
putChunks :: (MonadIO m, Storable a) => SerialT m (Array a) -> m ()
putChunks = fromChunks stdout

-- XXX use an unfold so that we can put any type of strings.
-- | Write a stream of strings to standard output using the supplied encoding.
-- Output is flushed to the device for each string.
--
-- /Internal/
--
{-# INLINE putStrings #-}
putStrings :: MonadAsync m
    => (SerialT m Char -> SerialT m Word8) -> SerialT m String -> m ()
putStrings encode = putChunks . S.mapM (IA.fromStream . encode . S.fromList)

-- XXX use an unfold so that we can put lines from any object
-- | Write a stream of strings as separate lines to standard output using the
-- supplied encoding. Output is line buffered i.e. the output is written to the
-- device as soon as a newline is encountered.
--
-- /Internal/
--
{-# INLINE putLines #-}
putLines :: MonadAsync m
    => (SerialT m Char -> SerialT m Word8) -> SerialT m String -> m ()
putLines encode = putChunks . S.mapM
    (\xs -> IA.fromStream $ encode (S.fromList (xs ++ "\n")))

-- | Write a stream of bytes from standard output.
--
-- > putBytes = fromBytes stdout
--
-- /Internal/
--
{-# INLINE putBytes #-}
putBytes :: MonadIO m => SerialT m Word8 -> m ()
putBytes = fromBytes stdout

-- | @fromChunksWithBufferOf bufsize handle stream@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- The chunk size is only a maximum and the actual writes could be smaller as
-- we do not split the arrays to fit exactly to the specified size.
--
-- @since 0.7.0
{-# INLINE fromChunksWithBufferOf #-}
fromChunksWithBufferOf :: (MonadIO m, Storable a)
    => Int -> Handle -> SerialT m (Array a) -> m ()
fromChunksWithBufferOf n h xs = fromChunks h $ AS.compact n xs

-- | @fromBytesWithBufferOf bufsize handle stream@ writes @stream@ to @handle@
-- in chunks of @bufsize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
-- @since 0.7.0
{-# INLINE fromBytesWithBufferOf #-}
fromBytesWithBufferOf :: MonadIO m => Int -> Handle -> SerialT m Word8 -> m ()
fromBytesWithBufferOf n h m = fromChunks h $ S.arraysOf n m
-- fromBytesWithBufferOf n h m = fromChunks h $ AS.arraysOf n m

-- > write = 'writeWithBufferOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Storable.Foreign.Types.defaultChunkSize' before writing.
--
-- NOTE: This may perform better than the 'write' fold, you can try this if you
-- need some extra perf boost.
--
-- @since 0.7.0
{-# INLINE fromBytes #-}
fromBytes :: MonadIO m => Handle -> SerialT m Word8 -> m ()
fromBytes = fromBytesWithBufferOf defaultChunkSize

-- | Write a stream of arrays to a handle. Each array in the stream is written
-- to the device as a separate IO request.
--
-- @since 0.7.0
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Storable a) => Handle -> Fold m (Array a) ()
writeChunks h = FL.drainBy (liftIO . writeArray h)

{-# INLINE writeChunks2 #-}
writeChunks2 :: (MonadIO m, Storable a) => Fold2 m Handle (Array a) ()
writeChunks2 = Fold2 (\h arr -> liftIO $ writeArray h arr >> return h) return (\_ -> return ())

-- | @writeChunksWithBufferOf bufsize handle@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- We never split an array, if a single array is bigger than the specified size
-- it emitted as it is. Multiple arrays are coalesed as long as the total size
-- remains below the specified size.
--
-- @since 0.7.0
{-# INLINE writeChunksWithBufferOf #-}
writeChunksWithBufferOf :: (MonadIO m, Storable a)
    => Int -> Handle -> Fold m (Array a) ()
writeChunksWithBufferOf n h = lpackArraysChunksOf n (writeChunks h)

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- | @writeWithBufferOf reqSize handle@ writes the input stream to @handle@.
-- Bytes in the input stream are collected into a buffer until we have a chunk
-- of @reqSize@ and then written to the IO device.
--
-- @since 0.7.0
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWithBufferOf n h = FL.chunksOf n (writeNUnsafe n) (writeChunks h)

{-# INLINE writeWithBufferOf2 #-}
writeWithBufferOf2 :: MonadIO m => Int -> Fold2 m Handle Word8 ()
writeWithBufferOf2 n = FL.chunksOf2 n (writeNUnsafe n) writeChunks2

-- > write = 'writeWithBufferOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Storable.Foreign.Types.defaultChunkSize' before writing
-- to the IO device.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeWithBufferOf defaultChunkSize

{-# INLINE write2 #-}
write2 :: MonadIO m => Fold2 m Handle Word8 ()
write2 = writeWithBufferOf2 defaultChunkSize

{-# INLINE toChunksWithBufferOfRange #-}
toChunksWithBufferOfRange
    :: (IsStream t, MonadIO m) 
    => Int 
    -> IORef Int 
    -> Handle 
    -> t m (Array Word8)
toChunksWithBufferOfRange size lenRef h = D.fromStreamD (D.Stream step ())
    
    where

    {-# INLINE_LATE step #-}
    step _ _ = do
        remain <- liftIO $ readIORef lenRef
        arr <- liftIO $ readArrayUpto (min remain size) h
        liftIO $ writeIORef lenRef (remain - (A.length arr)) 
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr ()    

{-# INLINE readSliceWith #-}
readSliceWith :: (IsStream t, MonadIO (t m), MonadIO m)
    => Int -> Handle -> Int -> Int -> t m (Array Word8)
readSliceWith chunkSize h pos len = do    
    lenRef <- liftIO $ newIORef len
    liftIO $ hSeek h AbsoluteSeek $ fromIntegral pos
    toChunksWithBufferOfRange chunkSize lenRef h

{-# INLINABLE writeArrayWith #-}
writeArrayWith :: Storable a => Handle -> Array a -> Int -> IO ()
writeArrayWith _ arr _ | A.length arr == 0 = return ()
writeArrayWith h Array{..} len = withForeignPtr aStart $ \p -> hPutBuf h p len  

{-# INLINE fromChunksWith #-}
fromChunksWith :: (MonadIO m, Storable a)
    => Handle -> IORef Int -> SerialT m (Array a) -> m ()
fromChunksWith h lenRef = S.mapM_ func
    
    where
    
    func = \arr -> do 
        remain <- liftIO $ readIORef lenRef
        let size = (A.length arr)
            count = min remain size
        liftIO $ writeIORef lenRef (remain - count) 
        liftIO $ writeArrayWith h arr count

-- | @writeSlice h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE writeSlice #-}
writeSlice :: (MonadIO m)
    => Handle -> Int -> Int -> Int -> SerialT m (Word8) -> m ()
writeSlice h chunkSize pos len s = do
    lenRef <- liftIO $ newIORef len
    liftIO $ hSeek h AbsoluteSeek $ fromIntegral pos 
    let count = ((len `div` chunkSize) + 1)
        strm = S.take count $ S.arraysOf chunkSize s
    fromChunksWith h lenRef strm

{-
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => Handle -> SerialT m a -> m ()
write = toHandleWith A.defaultChunkSize
-}

-- XXX mmap a file into an array. This could be useful for in-place operations
-- on a file. For example, we can quicksort the contents of a file by mmapping
-- it.

-------------------------------------------------------------------------------
-- IO with encoding/decoding Unicode characters
-------------------------------------------------------------------------------

{-
-- |
-- > readUtf8 = decodeUtf8 . read
--
-- Read a UTF8 encoded stream of unicode characters from a file handle.
--
-- @since 0.7.0
{-# INLINE readUtf8 #-}
readUtf8 :: (IsStream t, MonadIO m) => Handle -> t m Char
readUtf8 = decodeUtf8 . read

-- |
-- > writeUtf8 h s = write h $ encodeUtf8 s
--
-- Encode a stream of unicode characters to UTF8 and write it to the given file
-- handle. Default block buffering applies to the writes.
--
-- @since 0.7.0
{-# INLINE writeUtf8 #-}
writeUtf8 :: MonadIO m => Handle -> SerialT m Char -> m ()
writeUtf8 h s = write h $ encodeUtf8 s

-- | Write a stream of unicode characters after encoding to UTF-8 in chunks
-- separated by a linefeed character @'\n'@. If the size of the buffer exceeds
-- @defaultChunkSize@ and a linefeed is not yet found, the buffer is written
-- anyway.  This is similar to writing to a 'Handle' with the 'LineBuffering'
-- option.
--
-- @since 0.7.0
{-# INLINE writeUtf8ByLines #-}
writeUtf8ByLines :: (IsStream t, MonadIO m) => Handle -> t m Char -> m ()
writeUtf8ByLines = undefined

-- | Read UTF-8 lines from a file handle and apply the specified fold to each
-- line. This is similar to reading a 'Handle' with the 'LineBuffering' option.
--
-- @since 0.7.0
{-# INLINE readLines #-}
readLines :: (IsStream t, MonadIO m) => Handle -> Fold m Char b -> t m b
readLines h f = foldLines (readUtf8 h) f

-------------------------------------------------------------------------------
-- Framing on a sequence
-------------------------------------------------------------------------------

-- | Read a stream from a file handle and split it into frames delimited by
-- the specified sequence of elements. The supplied fold is applied on each
-- frame.
--
-- @since 0.7.0
{-# INLINE readFrames #-}
readFrames :: (IsStream t, MonadIO m, Storable a)
    => Array a -> Handle -> Fold m a b -> t m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE writeByFrames #-}
writeByFrames :: (IsStream t, MonadIO m, Storable a)
    => Array a -> Handle -> t m a -> m ()
writeByFrames = undefined

-------------------------------------------------------------------------------
-- Framing by time
-------------------------------------------------------------------------------

-- | Write collecting the input in sessions of n seconds or if chunkSize
-- gets exceeded.
{-# INLINE writeByChunksOrSessionsOf #-}
writeByChunksOrSessionsOf :: MonadIO m
    => Int -> Double -> Handle -> SerialT m Word8 -> m ()
writeByChunksOrSessionsOf chunkSize sessionSize h m = undefined

-- | Write collecting the input in sessions of n seconds or if defaultChunkSize
-- gets exceeded.
{-# INLINE writeBySessionsOf #-}
writeBySessionsOf :: MonadIO m => Double -> Handle -> SerialT m Word8 -> m ()
writeBySessionsOf n = writeByChunksOrSessionsOf defaultChunkSize n

-------------------------------------------------------------------------------
-- Random Access IO (Seek)
-------------------------------------------------------------------------------

-- XXX handles could be shared, so we may not want to use the handle state at
-- all for these APIs. we can use pread and pwrite instead. On windows we will
-- need to use readFile/writeFile with an offset argument.

-------------------------------------------------------------------------------

-- | Read the element at the given index treating the file as an array.
--
-- @since 0.7.0
{-# INLINE readIndex #-}
readIndex :: Storable a => Handle -> Int -> Maybe a
readIndex arr i = undefined

-- NOTE: To represent a range to read we have chosen (start, size) instead of
-- (start, end). This helps in removing the ambiguity of whether "end" is
-- included in the range or not.
--
-- We could avoid specifying the range to be read and instead use "take size"
-- on the stream, but it may end up reading more and then consume it partially.

-- | @readSliceWith chunkSize handle pos len@ reads up to @len@ bytes
-- from @handle@ starting at the offset @pos@ from the beginning of the file.
--
-- Reads are performed in chunks of size @chunkSize@.  For block devices, to
-- avoid reading partial blocks @chunkSize@ must align with the block size of
-- the underlying device. If the underlying block size is unknown, it is a good
-- idea to keep it a multiple 4KiB. This API ensures that the start of each
-- chunk is aligned with @chunkSize@ from second chunk onwards.
--
{-# INLINE readSliceWith #-}
readSliceWith :: (IsStream t, MonadIO m, Storable a)
    => Int -> Handle -> Int -> Int -> t m a
readSliceWith chunkSize h pos len = undefined

-- | @readSlice h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE readSlice #-}
readSlice :: (IsStream t, MonadIO m, Storable a)
    => Handle -> Int -> Int -> t m a
readSlice = readSliceWith A.defaultChunkSize

-- | @readSliceRev h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the reverse direction
-- ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE readSliceRev #-}
readSliceRev :: (IsStream t, MonadIO m, Storable a)
    => Handle -> Int -> Int -> t m a
readSliceRev h i count = undefined

-- | Write the given element at the given index in the file.
--
-- @since 0.7.0
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Storable a) => Handle -> Int -> a -> m ()
writeIndex h i a = undefined

-- | @writeSlice h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE writeSlice #-}
writeSlice :: (IsStream t, Monad m, Storable a)
    => Handle -> Int -> Int -> t m a -> m ()
writeSlice h i len s = undefined

-- | @writeSliceRev h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (IsStream t, Monad m, Storable a)
    => Handle -> Int -> Int -> t m a -> m ()
writeSliceRev arr i len s = undefined
-}
