#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.File
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read and write streams and arrays to and from files specified by their paths
-- in the file system. Unlike the handle based APIs which can have a read/write
-- session consisting of multiple reads and writes to the handle, these APIs
-- are one shot read or write APIs. These APIs open the file handle, perform
-- the requested operation and close the handle. Thease are safer compared to
-- the handle based APIs as there is no possibility of a file descriptor
-- leakage.
--
-- > import qualified Streamly.Internal.FileSystem.File as File
--

module Streamly.Internal.FileSystem.File
    (
    -- * Streaming IO
    -- | Stream data to or from a file or device sequentially.  When reading,
    -- the stream is lazy and generated on-demand as the consumer consumes it.
    -- Read IO requests to the IO device are performed in chunks limited to a
    -- maximum size of 32KiB, this is referred to as @defaultChunkSize@ in the
    -- documentation. One IO request may or may not read the full
    -- chunk. If the whole stream is not consumed, it is possible that we may
    -- read slightly more from the IO device than what the consumer needed.
    -- Unless specified otherwise in the API, writes are collected into chunks
    -- of @defaultChunkSize@ before they are written to the IO device.

    -- Streaming APIs work for all kind of devices, seekable or non-seekable;
    -- including disks, files, memory devices, terminals, pipes, sockets and
    -- fifos. While random access APIs work only for files or devices that have
    -- random access or seek capability for example disks, memory devices.
    -- Devices like terminals, pipes, sockets and fifos do not have random
    -- access capability.

    -- ** File IO Using Handle
      withFile

    -- ** Read From File
    , readWithBufferOf
    , read
    -- , readShared
    -- , readUtf8
    -- , readLines
    -- , readFrames

    , toBytes

    -- -- * Array Read
    , readChunksWithBufferOf
    , readChunks

    , toChunksWithBufferOf
    , toChunks

    -- ** Write To File
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeWithBufferOf

    , fromBytes
    , fromBytesWithBufferOf

    -- -- * Array Write
    , writeArray
    , writeChunks
    , fromChunks

    -- ** Append To File
    , append
    , appendWithBufferOf
    -- , appendShared
    , appendArray
    , appendChunks
    )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import System.IO (Handle, openFile, IOMode(..), hClose)
import Prelude hiding (read)

import qualified Control.Monad.Catch as MC
import qualified System.IO as SIO

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Array.Foreign.Types
       (Array(..), defaultChunkSize, writeNUnsafe)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.SVar (MonadAsync)
-- import Streamly.Data.Fold (Fold)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)

import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
import qualified Streamly.Data.Array.Foreign as A
import qualified Streamly.Prelude as S

-------------------------------------------------------------------------------
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
-- Safe file reading
-------------------------------------------------------------------------------

-- | @'withFile' name mode act@ opens a file using 'openFile' and passes
-- the resulting handle to the computation @act@.  The handle will be
-- closed on exit from 'withFile', whether by normal termination or by
-- raising an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'withFile' rather than any exception
-- raised by 'act'.
--
-- /Internal/
--
{-# INLINE withFile #-}
withFile :: (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath -> IOMode -> (Handle -> t m a) -> t m a
withFile file mode = S.bracket (liftIO $ openFile file mode) (liftIO . hClose)

-- | Transform an 'Unfold' from a 'Handle' to an unfold from a 'FilePath'.  The
-- resulting unfold opens a handle in 'ReadMode', uses it using the supplied
-- unfold and then makes sure that the handle is closed on normal termination
-- or in case of an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'usingFile'.
--
-- /Internal/
--
{-# INLINABLE usingFile #-}
usingFile :: (MonadCatch m, MonadAsync m)
    => Unfold m Handle a -> Unfold m FilePath a
usingFile =
    UF.bracket (\file -> liftIO $ openFile file ReadMode)
               (liftIO . hClose)

{-# INLINABLE usingFile2 #-}
usingFile2 :: (MonadCatch m, MonadAsync m)
    => Unfold m (x, Handle) a -> Unfold m (x, FilePath) a
usingFile2 = UF.bracket before after

    where

    before (x, file) =  do
        h <- liftIO $ openFile file ReadMode
        return (x, h)

    after (_, h) = liftIO $ hClose h

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- TODO readArrayOf

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an array to a file. Overwrites the file if it exists.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Storable a => FilePath -> Array a -> IO ()
writeArray file arr = SIO.withFile file WriteMode (`FH.writeArray` arr)

-- | append an array to a file.
--
-- @since 0.7.0
{-# INLINABLE appendArray #-}
appendArray :: Storable a => FilePath -> Array a -> IO ()
appendArray file arr = SIO.withFile file AppendMode (`FH.writeArray` arr)

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @toChunksWithBufferOf size file@ reads a stream of arrays from file @file@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
{-# INLINABLE toChunksWithBufferOf #-}
toChunksWithBufferOf :: (IsStream t, MonadCatch m, MonadAsync m)
    => Int -> FilePath -> t m (Array Word8)
toChunksWithBufferOf size file =
    withFile file ReadMode (FH.toChunksWithBufferOf size)

-- XXX read 'Array a' instead of Word8
--
-- | @toChunks file@ reads a stream of arrays from file @file@.
-- The maximum size of a single array is limited to @defaultChunkSize@. The
-- actual size read may be less than @defaultChunkSize@.
--
-- > toChunks = toChunksWithBufferOf defaultChunkSize
--
-- @since 0.7.0
{-# INLINE toChunks #-}
toChunks :: (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath -> t m (Array Word8)
toChunks = toChunksWithBufferOf defaultChunkSize

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Unfold the tuple @(bufsize, filepath)@ into a stream of 'Word8' arrays.
-- Read requests to the IO device are performed using a buffer of size
-- @bufsize@. The size of an array in the resulting stream is always less than
-- or equal to @bufsize@.
--
-- /Internal/
--
{-# INLINE readChunksWithBufferOf #-}
readChunksWithBufferOf :: (MonadCatch m, MonadAsync m)
    => Unfold m (Int, FilePath) (Array Word8)
readChunksWithBufferOf = usingFile2 FH.readChunksWithBufferOf

-- | Unfolds a 'FilePath' into a stream of 'Word8' arrays. Requests to the IO
-- device are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Foreign.Types.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Foreign.Types.defaultChunkSize'.
--
-- /Internal/
{-# INLINE readChunks #-}
readChunks :: (MonadCatch m, MonadAsync m) => Unfold m FilePath (Array Word8)
readChunks = usingFile FH.readChunks

-- | Unfolds the tuple @(bufsize, filepath)@ into a byte stream, read requests
-- to the IO device are performed using buffers of @bufsize@.
--
-- /Internal/
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: (MonadCatch m, MonadAsync m) => Unfold m (Int, FilePath) Word8
readWithBufferOf = usingFile2 FH.readWithBufferOf

-- | Unfolds a file path into a byte stream. IO requests to the device are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Foreign.Types.defaultChunkSize'.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (MonadCatch m, MonadAsync m) => Unfold m FilePath Word8
read = UF.concat (usingFile FH.readChunks) A.read

-- | Generate a stream of bytes from a file specified by path. The stream ends
-- when EOF is encountered. File is locked using multiple reader and single
-- writer locking mode.
--
-- /Internal/
--
{-# INLINE toBytes #-}
toBytes :: (IsStream t, MonadCatch m, MonadAsync m) => FilePath -> t m Word8
toBytes file = AS.concat $ withFile file ReadMode FH.toChunks

{-
-- | Generate a stream of elements of the given type from a file 'Handle'. The
-- stream ends when EOF is encountered. File is not locked for exclusive reads,
-- writers can keep writing to the file.
--
-- @since 0.7.0
{-# INLINE readShared #-}
readShared :: (IsStream t, MonadIO m) => Handle -> t m Word8
readShared = undefined
-}

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

{-# INLINE fromChunksMode #-}
fromChunksMode :: (MonadAsync m, MonadCatch m, Storable a)
    => IOMode -> FilePath -> SerialT m (Array a) -> m ()
fromChunksMode mode file xs = S.drain $
    withFile file mode (\h -> S.mapM (liftIO . FH.writeArray h) xs)

-- | Write a stream of arrays to a file. Overwrites the file if it exists.
--
-- @since 0.7.0
{-# INLINE fromChunks #-}
fromChunks :: (MonadAsync m, MonadCatch m, Storable a)
    => FilePath -> SerialT m (Array a) -> m ()
fromChunks = fromChunksMode WriteMode

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE fromBytesWithBufferOf #-}
fromBytesWithBufferOf :: (MonadAsync m, MonadCatch m)
    => Int -> FilePath -> SerialT m Word8 -> m ()
fromBytesWithBufferOf n file xs = fromChunks file $ AS.arraysOf n xs

-- > write = 'writeWithBufferOf' defaultChunkSize
--
-- | Write a byte stream to a file. Combines the bytes in chunks of size
-- up to 'A.defaultChunkSize' before writing. If the file exists it is
-- truncated to zero size before writing. If the file does not exist it is
-- created. File is locked using single writer locking mode.
--
-- /Internal/
{-# INLINE fromBytes #-}
fromBytes :: (MonadAsync m, MonadCatch m) => FilePath -> SerialT m Word8 -> m ()
fromBytes = fromBytesWithBufferOf defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => Handle -> SerialT m a -> m ()
write = toHandleWith A.defaultChunkSize
-}

-- | Write a stream of chunks to a handle. Each chunk in the stream is written
-- to the device as a separate IO request.
--
-- /Internal/
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, MonadCatch m, Storable a)
    => FilePath -> Fold m (Array a) ()
writeChunks path = Fold step initial extract cleanup
    where
    initial = do
        h <- liftIO (openFile path WriteMode)
        fld <- FL.initialize (FH.writeChunks h)
                `MC.onException` liftIO (hClose h)
        return $ FL.Partial (fld, h)
    step t@(fld, h) x = do
        r <- FL.runStep fld x `MC.onException` cleanup t
        return $ FL.Partial (r, h)
    extract (Fold _ initial1 extract1 _, _) = do
        res <- initial1
        case res of
            FL.Partial fs -> extract1 fs
            FL.Done fb -> return fb
    cleanup (Fold _ initial1 _ cleanup1, h) = do
        liftIO $ hClose h
        res <- initial1
        case res of
            FL.Partial fs -> cleanup1 fs
            FL.Done _ -> return ()

-- | @writeWithBufferOf chunkSize handle@ writes the input stream to @handle@.
-- Bytes in the input stream are collected into a buffer until we have a chunk
-- of size @chunkSize@ and then written to the IO device.
--
-- /Internal/
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: (MonadIO m, MonadCatch m)
    => Int -> FilePath -> Fold m Word8 ()
writeWithBufferOf n path =
    FL.chunksOf n (writeNUnsafe n) (writeChunks path)

-- > write = 'writeWithBufferOf' A.defaultChunkSize
--
-- | Write a byte stream to a file. Accumulates the input in chunks of up to
-- 'Streamly.Internal.Data.Array.Foreign.Types.defaultChunkSize' before writing to
-- the IO device.
--
-- /Internal/
--
{-# INLINE write #-}
write :: (MonadIO m, MonadCatch m) => FilePath -> Fold m Word8 ()
write = writeWithBufferOf defaultChunkSize

-- | Append a stream of arrays to a file.
--
-- @since 0.7.0
{-# INLINE appendChunks #-}
appendChunks :: (MonadAsync m, MonadCatch m, Storable a)
    => FilePath -> SerialT m (Array a) -> m ()
appendChunks = fromChunksMode AppendMode

-- | Like 'append' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE appendWithBufferOf #-}
appendWithBufferOf :: (MonadAsync m, MonadCatch m)
    => Int -> FilePath -> SerialT m Word8 -> m ()
appendWithBufferOf n file xs = appendChunks file $ AS.arraysOf n xs

-- | Append a byte stream to a file. Combines the bytes in chunks of size up to
-- 'A.defaultChunkSize' before writing.  If the file exists then the new data
-- is appended to the file.  If the file does not exist it is created. File is
-- locked using single writer locking mode.
--
-- @since 0.7.0
{-# INLINE append #-}
append :: (MonadAsync m, MonadCatch m) => FilePath -> SerialT m Word8 -> m ()
append = appendWithBufferOf defaultChunkSize

{-
-- | Like 'append' but the file is not locked for exclusive writes.
--
-- @since 0.7.0
{-# INLINE appendShared #-}
appendShared :: MonadIO m => Handle -> SerialT m Word8 -> m ()
appendShared = undefined
-}

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
readSlice = readSliceWith defaultChunkSize

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
