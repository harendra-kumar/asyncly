-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
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

module Handle.Read
    (allBenchmarks)
where

import Data.Word (Word8)
import GHC.Magic (inline)
#if __GLASGOW_HASKELL__ >= 802
import GHC.Magic (noinline)
#else
#define noinline
#endif
import System.IO (Handle)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as AT
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Unicode.Stream as IUS
import qualified Streamly.Prelude as S
import qualified Streamly.Unicode.Stream as SS

import Gauge hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..), FoldMany)

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Unfold as IUF

import Test.Inspection
#endif

-- TBD reading with unfold

-------------------------------------------------------------------------------
-- unfold read
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
readLast :: Handle -> IO (Maybe Word8)
readLast = S.last . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readLast
inspect $ 'readLast `hasNoType` ''Step -- S.unfold
inspect $ 'readLast `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readLast `hasNoType` ''MA.ReadUState  -- FH.read/A.read
#endif

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
readCountBytes :: Handle -> IO Int
readCountBytes = S.length . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountBytes
inspect $ 'readCountBytes `hasNoType` ''Step -- S.unfold
inspect $ 'readCountBytes `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readCountBytes `hasNoType` ''MA.ReadUState  -- FH.read/A.read
#endif

-- | Count the number of lines in a file.
readCountLines :: Handle -> IO Int
readCountLines =
    S.length
        . IUS.lines FL.drain
        . SS.decodeLatin1
        . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountLines
inspect $ 'readCountLines `hasNoType` ''Step
inspect $ 'readCountLines `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readCountLines `hasNoType` ''MA.ReadUState  -- FH.read/A.read
#endif

-- | Count the number of words in a file.
readCountWords :: Handle -> IO Int
readCountWords =
    S.length
        . IUS.words FL.drain
        . SS.decodeLatin1
        . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountWords
-- inspect $ 'readCountWords `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
readSumBytes :: Handle -> IO Word8
readSumBytes = S.sum . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readSumBytes
inspect $ 'readSumBytes `hasNoType` ''Step
inspect $ 'readSumBytes `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readSumBytes `hasNoType` ''MA.ReadUState  -- FH.read/A.read
#endif

-- XXX When we mark this with INLINE and we have two benchmarks using S.drain
-- in one benchmark group then somehow GHC ends up delaying the inlining of
-- readDrain. Since S.drain has an INLINE[2] for proper rule firing, that does
-- not work well because of delyaed inlining and the code does not fuse. We
-- need some way of propagating the inline phase information up so that we can
-- expedite inlining of the callers too automatically. The minimal example for
-- the problem can be created by using just two benchmarks in a bench group
-- both using "readDrain". Either GHC should be fixed or we can use
-- fusion-plugin to propagate INLINE phase information such that this problem
-- does not occur.
readDrain :: Handle -> IO ()
readDrain inh = S.drain $ S.unfold FH.read inh

-- XXX investigate why we need an INLINE in this case (GHC)
{-# INLINE readDecodeLatin1 #-}
readDecodeLatin1 :: Handle -> IO ()
readDecodeLatin1 inh =
   S.drain
     $ SS.decodeLatin1
     $ S.unfold FH.read inh

readDecodeUtf8 :: Handle -> IO ()
readDecodeUtf8 inh =
   S.drain
     $ SS.decodeUtf8
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readDecodeUtf8
-- inspect $ 'readDecodeUtf8Lax `hasNoType` ''Step
#endif

o_1_space_reduce_read :: BenchEnv -> [Benchmark]
o_1_space_reduce_read env =
    [ bgroup "reduce/read"
        [ -- read raw bytes without any decoding
          mkBench "S.drain" env $ \inh _ ->
            readDrain inh
        , mkBench "S.last" env $ \inh _ ->
            readLast inh
        , mkBench "S.sum" env $ \inh _ ->
            readSumBytes inh

        -- read with Latin1 decoding
        , mkBench "SS.decodeLatin1" env $ \inh _ ->
            readDecodeLatin1 inh
        , mkBench "S.length" env $ \inh _ ->
            readCountBytes inh
        , mkBench "US.lines . SS.decodeLatin1" env $ \inh _ ->
            readCountLines inh
        , mkBench "US.words . SS.decodeLatin1" env $ \inh _ ->
            readCountWords inh

        -- read with utf8 decoding
        , mkBenchSmall "SS.decodeUtf8" env $ \inh _ ->
            readDecodeUtf8 inh
        ]
    ]

-------------------------------------------------------------------------------
-- stream toBytes
-------------------------------------------------------------------------------

-- | Count the number of lines in a file.
toChunksConcatUnfoldCountLines :: Handle -> IO Int
toChunksConcatUnfoldCountLines inh =
    S.length
        $ IUS.lines FL.drain
        $ SS.decodeLatin1
        -- XXX replace with toBytes
        $ S.unfoldMany A.read (IFH.toChunks inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksConcatUnfoldCountLines
inspect $ 'toChunksConcatUnfoldCountLines `hasNoType` ''Step
inspect $ 'toChunksConcatUnfoldCountLines `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_reduce_toBytes :: BenchEnv -> [Benchmark]
o_1_space_reduce_toBytes env =
    [ bgroup "reduce/toBytes"
        [ mkBench "US.lines . SS.decodeLatin1" env $ \inh _ ->
            toChunksConcatUnfoldCountLines inh
        ]
    ]

-------------------------------------------------------------------------------
-- reduce after grouping in chunks
-------------------------------------------------------------------------------

chunksOfSum :: Int -> Handle -> IO Int
chunksOfSum n inh = S.length $ S.chunksOf n FL.sum (S.unfold FH.read inh)

foldManyPostChunksOfSum :: Int -> Handle -> IO Int
foldManyPostChunksOfSum n inh =
    S.length $ IP.foldManyPost (FL.take n FL.sum) (S.unfold FH.read inh)

foldManyChunksOfSum :: Int -> Handle -> IO Int
foldManyChunksOfSum n inh =
    S.length $ IP.foldMany (FL.take n FL.sum) (S.unfold FH.read inh)

-- XXX investigate why we need an INLINE in this case (GHC)
-- Even though allocations remain the same in both cases inlining improves time
-- by 4x.
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    S.length $ S.chunksOf n (AT.writeNUnsafe n) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOf `hasNoType` ''FoldMany
inspect $ 'chunksOf `hasNoType` ''AT.ArrayUnsafe -- AT.writeNUnsafe
inspect $ 'chunksOf `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'chunksOf `hasNoType` ''MA.ReadUState  -- FH.read/A.read
#endif

{-# INLINE arraysOf #-}
arraysOf :: Int -> Handle -> IO Int
arraysOf n inh = S.length $ IP.arraysOf n (S.unfold FH.read inh)

o_1_space_reduce_read_grouped :: BenchEnv -> [Benchmark]
o_1_space_reduce_read_grouped env =
    [ bgroup "reduce/read/chunks"
        [ mkBench ("S.chunksOf " ++ show (bigSize env) ++  " FL.sum") env $
            \inh _ ->
                chunksOfSum (bigSize env) inh
        , mkBench "S.chunksOf 1 FL.sum" env $ \inh _ ->
            chunksOfSum 1 inh

        -- XXX investigate why we need inline/noinline in these cases (GHC)
        -- Chunk using parsers
        , mkBench
            ("S.foldManyPost (FL.take " ++ show (bigSize env) ++ " FL.sum)")
            env
            $ \inh _ -> noinline foldManyPostChunksOfSum (bigSize env) inh
        , mkBench
            "S.foldManyPost (FL.take 1 FL.sum)"
            env
            $ \inh _ -> inline foldManyPostChunksOfSum 1 inh
        , mkBench
            ("S.foldMany (FL.take " ++ show (bigSize env) ++ " FL.sum)")
            env
            $ \inh _ -> noinline foldManyChunksOfSum (bigSize env) inh
        , mkBench
            "S.foldMany (FL.take 1 FL.sum)"
            env
            $ \inh _ -> inline foldManyChunksOfSum 1 inh

        -- folding chunks to arrays
        , mkBenchSmall "S.chunksOf 1" env $ \inh _ ->
            chunksOf 1 inh
        , mkBench "S.chunksOf 10" env $ \inh _ ->
            chunksOf 10 inh
        , mkBench "S.chunksOf 1000" env $ \inh _ ->
            chunksOf 1000 inh

        -- arraysOf may use a different impl than chunksOf
        , mkBenchSmall "S.arraysOf 1" env $ \inh _ ->
            arraysOf 1 inh
        , mkBench "S.arraysOf 10" env $ \inh _ ->
            arraysOf 10 inh
        , mkBench "S.arraysOf 1000" env $ \inh _ ->
            arraysOf 1000 inh
        ]
    ]

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_reduce_read env
    , o_1_space_reduce_toBytes env
    , o_1_space_reduce_read_grouped env
    ]
