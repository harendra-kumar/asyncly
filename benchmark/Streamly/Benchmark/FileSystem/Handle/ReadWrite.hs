-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Handle.ReadWrite
    (allBenchmarks)
where

import System.IO (Handle)
import Prelude hiding (last, length)
import Streamly.Internal.Data.Array.Foreign.Type (defaultChunkSize)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Data.Array.Foreign as A
import qualified Streamly.Prelude as S

import Gauge hiding (env)
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Tuple.Strict as Strict
import qualified Streamly.Internal.Data.Array.Stream.Mut.Foreign as MAS
import qualified Streamly.Internal.Data.Array.Foreign.Type as AT
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- copy chunked
-------------------------------------------------------------------------------

-- | Copy file
copyChunks :: Handle -> Handle -> IO ()
copyChunks inh outh = S.fold (IFH.writeChunks outh) $ IFH.toChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyChunks
inspect $ 'copyChunks `hasNoType` ''Step
#endif

o_1_space_copy_chunked :: BenchEnv -> [Benchmark]
o_1_space_copy_chunked env =
    [ bgroup "copy/toChunks"
        [ mkBench "toNull" env $ \inH _ ->
            copyChunks inH (nullH env)
        , mkBench "raw" env $ \inH outH ->
            copyChunks inH outH
        ]
    ]

-------------------------------------------------------------------------------
-- copy unfold
-------------------------------------------------------------------------------

-- | Copy file
copyStream :: Handle -> Handle -> IO ()
copyStream inh outh = S.fold (FH.write outh) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStream
inspect $ 'copyStream `hasNoType` ''Step -- S.unfold
inspect $ 'copyStream `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'copyStream `hasNoType` ''MA.ReadUState  -- FH.read/A.read
inspect $ 'copyStream `hasNoType` ''AT.ArrayUnsafe -- FH.write/writeNUnsafe
inspect $ 'copyStream `hasNoType` ''Strict.Tuple3' -- FH.write/chunksOf
#endif

o_1_space_copy_read :: BenchEnv -> [Benchmark]
o_1_space_copy_read env =
    [ bgroup "copy/read"
        [ mkBench "rawToNull" env $ \inh _ ->
            copyStream inh (nullH env)
        , mkBench "rawToFile" env $ \inh outh ->
            copyStream inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy stream
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null
readFromBytesNull :: Handle -> Handle -> IO ()
readFromBytesNull inh devNull = IFH.fromBytes devNull $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readFromBytesNull
inspect $ 'readFromBytesNull `hasNoType` ''Step
inspect $ 'readFromBytesNull `hasNoType` ''MAS.SpliceState
inspect $ 'readFromBytesNull `hasNoType` ''AT.ArrayUnsafe -- FH.fromBytes/S.arraysOf
inspect $ 'readFromBytesNull `hasNoType` ''D.FoldMany
#endif

-- | Send the file contents ('defaultChunkSize') to /dev/null
readWithBufferOfFromBytesNull :: Handle -> Handle -> IO ()
readWithBufferOfFromBytesNull inh devNull =
    IFH.fromBytes devNull
        $ S.unfold FH.readWithBufferOf (defaultChunkSize, inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWithBufferOfFromBytesNull
inspect $ 'readWithBufferOfFromBytesNull `hasNoType` ''Step
inspect $ 'readWithBufferOfFromBytesNull `hasNoType` ''MAS.SpliceState
inspect $ 'readWithBufferOfFromBytesNull `hasNoType` ''AT.ArrayUnsafe -- FH.fromBytes/S.arraysOf
inspect $ 'readWithBufferOfFromBytesNull `hasNoType` ''D.FoldMany
#endif

-- | Send the chunk content ('defaultChunkSize') to /dev/null
-- Implicitly benchmarked via 'readFromBytesNull'
_readChunks :: Handle -> Handle -> IO ()
_readChunks inh devNull = IUF.fold fld unf inh

    where

    fld = FH.write devNull
    unf = IUF.many FH.readChunks A.read

-- | Send the chunk content to /dev/null
-- Implicitly benchmarked via 'readWithBufferOfFromBytesNull'
_readChunksWithBufferOf :: Handle -> Handle -> IO ()
_readChunksWithBufferOf inh devNull = IUF.fold fld unf (defaultChunkSize, inh)

    where

    fld = FH.write devNull
    unf = IUF.many FH.readChunksWithBufferOf A.read


o_1_space_copy_fromBytes :: BenchEnv -> [Benchmark]
o_1_space_copy_fromBytes env =
    [ bgroup "copy/fromBytes"
        [ mkBench "rawToNull" env $ \inh _ ->
            readFromBytesNull inh (nullH env)
        , mkBench "FH.readWithBufferOf" env $ \inh _ ->
            readWithBufferOfFromBytesNull inh (nullH env)
        ]
    ]

-- | Send the file contents ('defaultChunkSize') to /dev/null
{-# NOINLINE writeReadWithBufferOf #-}
writeReadWithBufferOf :: Handle -> Handle -> IO ()
writeReadWithBufferOf inh devNull = IUF.fold fld unf (defaultChunkSize, inh)

    where

    fld = FH.writeWithBufferOf defaultChunkSize devNull
    unf = FH.readWithBufferOf

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'writeReadWithBufferOf
inspect $ 'writeReadWithBufferOf `hasNoType` ''Step
inspect $ 'writeReadWithBufferOf `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'writeReadWithBufferOf `hasNoType` ''MA.ReadUState  -- FH.read/A.read
inspect $ 'writeReadWithBufferOf `hasNoType` ''AT.ArrayUnsafe -- FH.write/writeNUnsafe
#endif

-- | Send the file contents ('AT.defaultChunkSize') to /dev/null
{-# NOINLINE writeRead #-}
writeRead :: Handle -> Handle -> IO ()
writeRead inh devNull = IUF.fold fld unf inh

    where

    fld = FH.write devNull
    unf = FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'writeRead
inspect $ 'writeRead `hasNoType` ''Step
inspect $ 'writeRead `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'writeRead `hasNoType` ''MA.ReadUState  -- FH.read/A.read
inspect $ 'writeRead `hasNoType` ''AT.ArrayUnsafe -- FH.write/writeNUnsafe
#endif

o_1_space_copy :: BenchEnv -> [Benchmark]
o_1_space_copy env =
    [ bgroup "copy"
        [ mkBench "FH.write . FH.read" env $ \inh _ ->
            writeRead inh (nullH env)
        , mkBench "FH.writeWithBufferOf . FH.readWithBufferOf" env $ \inh _ ->
            writeReadWithBufferOf inh (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_copy_chunked env
    , o_1_space_copy_read env
    , o_1_space_copy_fromBytes env
    , o_1_space_copy env
    ]
