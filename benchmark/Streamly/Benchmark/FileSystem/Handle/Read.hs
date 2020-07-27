-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
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

import Data.Char (ord)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import GHC.Magic (inline)
#if __GLASGOW_HASKELL__ >= 802
import GHC.Magic (noinline)
#else
#define noinline
#endif
import System.IO (Handle)
import Prelude hiding (last, length)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Unicode.Stream as SS
import qualified Streamly.FileSystem.Handle as FH
-- import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Unicode.Stream as IUS
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Memory.Array as A
import qualified Streamly.Internal.Memory.Array.Types as AT
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Prelude as S

import Gauge hiding (env)
import Handle.Common

#ifdef INSPECTION
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..), GroupState)

import qualified Streamly.Internal.Data.Unfold as IUF

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- read chunked using toChunks
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
toChunksLast :: Handle -> IO (Maybe Word8)
toChunksLast inh = do
    let s = IFH.toChunks inh
    larr <- S.last s
    return $ case larr of
        Nothing -> Nothing
        Just arr -> A.readIndex arr (A.length arr - 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksLast
inspect $ 'toChunksLast `hasNoType` ''Step
#endif

-- | Count the number of bytes in a file.
toChunksSumLengths :: Handle -> IO Int
toChunksSumLengths inh =
    let s = IFH.toChunks inh
    in S.sum (S.map A.length s)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSumLengths
inspect $ 'toChunksSumLengths `hasNoType` ''Step
#endif

-- | Count the number of lines in a file.
toChunksSplitOnSuffix :: Handle -> IO Int
toChunksSplitOnSuffix = S.length . AS.splitOnSuffix 10 . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOnSuffix
inspect $ 'toChunksSplitOnSuffix `hasNoType` ''Step
#endif

-- XXX use a word splitting combinator instead of splitOn and test it.
-- | Count the number of words in a file.
toChunksSplitOn :: Handle -> IO Int
toChunksSplitOn = S.length . AS.splitOn 32 . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOn
inspect $ 'toChunksSplitOn `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
toChunksCountBytes :: Handle -> IO Word8
toChunksCountBytes inh = do
    let foldlArr' f z = runIdentity . S.foldl' f z . A.toStream
    let s = IFH.toChunks inh
    S.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksCountBytes
inspect $ 'toChunksCountBytes `hasNoType` ''Step
#endif

toChunksDecodeUtf8ArraysLenient :: Handle -> IO ()
toChunksDecodeUtf8ArraysLenient =
   S.drain . IUS.decodeUtf8ArraysLenient . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksDecodeUtf8ArraysLenient
-- inspect $ 'toChunksDecodeUtf8ArraysLenient `hasNoType` ''Step
#endif

o_1_space_read_chunked :: BenchEnv -> [Benchmark]
o_1_space_read_chunked env =
    -- read using toChunks instead of read
    [ bgroup "reduce/toChunks"
        [ mkBench "S.last" env $ \inH _ ->
            toChunksLast inH
        -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
        -- wc uses lseek to just determine the file size rather than reading
        -- and counting characters.
        , mkBench "S.sum . S.map A.length" env $ \inH _ ->
            toChunksSumLengths inH
        , mkBench "AS.splitOnSuffix" env $ \inH _ ->
            toChunksSplitOnSuffix inH
        , mkBench "AS.splitOn" env $ \inH _ ->
            toChunksSplitOn inH
        , mkBench "countBytes" env $ \inH _ ->
            toChunksCountBytes inH
        , mkBenchSmall "US.decodeUtf8ArraysLenient" env $ \inH _ ->
            toChunksDecodeUtf8ArraysLenient inH
        ]
    ]

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
inspect $ 'readLast `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'readLast `hasNoType` ''A.ReadUState  -- FH.read/A.read
#endif

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
readCountBytes :: Handle -> IO Int
readCountBytes = S.length . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountBytes
inspect $ 'readCountBytes `hasNoType` ''Step -- S.unfold
inspect $ 'readCountBytes `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'readCountBytes `hasNoType` ''A.ReadUState  -- FH.read/A.read
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
inspect $ 'readCountLines `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'readCountLines `hasNoType` ''A.ReadUState  -- FH.read/A.read
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
inspect $ 'readSumBytes `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'readSumBytes `hasNoType` ''A.ReadUState  -- FH.read/A.read
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

readDecodeUtf8Lax :: Handle -> IO ()
readDecodeUtf8Lax inh =
   S.drain
     $ SS.decodeUtf8Lax
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readDecodeUtf8Lax
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
        , mkBenchSmall "US.words . SS.decodeLatin1" env $ \inh _ ->
            readCountWords inh

        -- read with utf8 decoding
        , mkBenchSmall "SS.decodeUtf8Lax" env $ \inh _ ->
            readDecodeUtf8Lax inh
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
        $ S.concatUnfold A.read (IFH.toChunks inh)

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

parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    S.length $ IP.parseMany (PR.take n FL.sum) (S.unfold FH.read inh)

-- XXX investigate why we need an INLINE in this case (GHC)
-- Even though allocations remain the same in both cases inlining improves time
-- by 4x.
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    -- XXX replace with S.arraysOf
    S.length $ S.chunksOf n (AT.writeNUnsafe n) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOf `hasNoType` ''GroupState
inspect $ 'chunksOf `hasNoType` ''AT.ArrayUnsafe -- AT.writeNUnsafe
inspect $ 'chunksOf `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'chunksOf `hasNoType` ''A.ReadUState  -- FH.read/A.read
#endif

-- This is to make sure that the concatMap in FH.read, groupsOf and foldlM'
-- together can fuse.
--
-- | Slice in chunks of size n and get the count of chunks.
_chunksOfD :: Int -> Handle -> IO Int
_chunksOfD n inh =
    D.foldlM' (\i _ -> return $ i + 1) (return 0)
        $ D.groupsOf n (AT.writeNUnsafe n)
        $ D.fromStreamK (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses '_chunksOfD
inspect $ '_chunksOfD `hasNoType` ''Step
inspect $ '_chunksOfD `hasNoType` ''GroupState
inspect $ '_chunksOfD `hasNoType` ''AT.ArrayUnsafe -- AT.writeNUnsafe
inspect $ '_chunksOfD `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ '_chunksOfD `hasNoType` ''A.ReadUState  -- FH.read/A.read
#endif

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
        , mkBenchSmall ("S.parseMany (PR.take " ++ show (bigSize env) ++ " FL.sum)")
            env $ \inh _ ->
                noinline parseManyChunksOfSum (bigSize env) inh
        , mkBench "S.parseMany (PR.take 1 FL.sum)" env $ \inh _ ->
                inline parseManyChunksOfSum 1 inh

        -- folding chunks to arrays
        , mkBenchSmall "S.arraysOf 1" env $ \inh _ ->
            chunksOf 1 inh
        , mkBench "S.arraysOf 10" env $ \inh _ ->
            chunksOf 10 inh
        , mkBench "S.arraysOf 1000" env $ \inh _ ->
            chunksOf 1000 inh
        ]
    ]

-------------------------------------------------------------------------------
-- reduce with splitting transformations
-------------------------------------------------------------------------------

lf :: Word8
lf = fromIntegral (ord '\n')

bs :: Word8
bs = fromIntegral (ord '\\')

toarr :: String -> A.Array Word8
toarr = A.fromList . map (fromIntegral . ord)

-- | Split on line feed.
splitOn :: Handle -> IO Int
splitOn inh =
    (S.length $ S.splitOn (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'splitOn `hasNoType` ''A.ReadUState  -- FH.read/A.read
#endif

-- | Split suffix on line feed.
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (S.length $ S.splitOnSuffix (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffix
inspect $ 'splitOnSuffix `hasNoType` ''Step
inspect $ 'splitOnSuffix `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'splitOnSuffix `hasNoType` ''A.ReadUState  -- FH.read/A.read
#endif

-- | Seperate newlines and other characters using deintercalate
deintercalate :: Handle -> IO ()
deintercalate inh = do
    let
        newline = 10
        prsr1 = PR.takeWhile (/=newline) FL.drain
        prsr2 = PR.takeWhile (==newline) FL.drain
        prsr = PR.deintercalate FL.drain prsr1 FL.drain prsr2
    
    (_, _) <- IP.parse prsr (S.unfold FH.read inh)
    return ()

-- | Split on line feed (but do not discard it - take it in next parse)
parseManySepWith :: Handle -> IO Int
parseManySepWith inh =
    (S.length $ IP.parseMany (PR.sliceSepWith (== lf) FL.drain)
                             (S.unfold FH.read inh)) -- >>= print

-- | Split on line feed.
parseManySepBy :: Handle -> IO Int
parseManySepBy inh =
    (S.length $ IP.parseMany (PR.sliceSepBy (== lf) FL.drain)
                             (S.unfold FH.read inh)) -- >>= print

-- | Split on line feed.
parseManySepByP :: Handle -> IO Int
parseManySepByP inh =
    S.length $ 
        IP.parseMany 
        (PR.sliceSepByP (== lf) (PR.takeWhile (\_ -> True) FL.drain))
        (S.unfold FH.read inh)

-- | Split on line feed, and use backslash as escape character
escapedSliceSepBy :: Handle -> IO Int
escapedSliceSepBy inh =
    S.length $ 
        IP.parseMany 
        (PR.escapedSliceSepBy (== lf) (== bs) FL.drain)
        (S.unfold FH.read inh)

-- | Split on line feed (as a suffix)
parseManyEndWith :: Handle -> IO Int
parseManyEndWith inh =
    (S.length $ IP.parseMany (PR.sliceEndWith (== lf) FL.drain)
                             (S.unfold FH.read inh))

-- | Split on line feed (as a prefix)
parseManyBeginWith :: Handle -> IO Int
parseManyBeginWith inh =
    (S.length $ IP.parseMany (PR.sliceBeginWith (== lf) FL.drain)
                             (S.unfold FH.read inh))

-- | Group based on (>) as comp function to compare
-- between starting element and current element
parseManyGroupBy :: Handle -> IO Int
parseManyGroupBy inh =
    (S.length $ IP.parseMany (PR.groupBy (>) FL.drain)
                             (S.unfold FH.read inh))

-- | Words by space but using wordBy parser
parseManyWordBy :: Handle -> IO Int
parseManyWordBy inh =
    (S.length $ wordsBy1 isSp FL.drain
        $ S.unfold FH.read inh)

    where wordsBy1 prd f = IP.parseMany (PR.wordBy prd f)

-- | Words by space
wordsBy :: Handle -> IO Int
wordsBy inh =
    (S.length $ S.wordsBy isSp FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsBy
inspect $ 'wordsBy `hasNoType` ''Step
inspect $ 'wordsBy `hasNoType` ''IUF.ConcatState -- FH.read/UF.concat
inspect $ 'wordsBy `hasNoType` ''A.ReadUState  -- FH.read/A.read
#endif

-- | Split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (S.length $ IP.splitOnSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
#endif

-- | Split on suffix sequence.
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (S.length $ IP.splitOnSuffixSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
#endif

o_1_space_reduce_read_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_read_split env =
    [ bgroup "reduce/read"
        [ mkBench "deintercalate" env
            $ \inh _ -> deintercalate inh
        , mkBench "S.parseMany (PR.sliceSepWith (== lf) FL.drain)" env
            $ \inh _ -> parseManySepWith inh
        , mkBench "S.parseMany (PR.sliceSepBy (== lf) FL.drain)" env
            $ \inh _ -> parseManySepBy inh
        , mkBench "S.parseMany (PR.sliceSepByP (== lf) (PR.takeWhile (\\_ -> True) FL.drain))" env
            $ \inh _ -> parseManySepByP inh
        , mkBench "S.parseMany (PR.escapedSliceSepBy (== lf) (== bs) FL.drain)" env
            $ \inh _ -> escapedSliceSepBy inh
        , mkBench "S.parseMany (PR.sliceEndWith (== lf) FL.drain)" env
            $ \inh _ -> parseManyEndWith inh
        , mkBench "S.parseMany (PR.sliceBeginWith (== lf) FL.drain)" env
            $ \inh _ -> parseManyBeginWith inh
            , mkBench "S.parseMany (PR.groupBy (>) FL.drain)" env
            $ \inh _ -> parseManyGroupBy inh
        , mkBench "IP.parseMany PR.wordBy" env $ \inh _ ->
            parseManyWordBy inh
        , mkBench "S.wordsBy isSpace FL.drain" env $ \inh _ ->
            wordsBy inh
        , mkBench "S.splitOn (== lf) FL.drain" env $ \inh _ ->
            splitOn inh
        , mkBench "S.splitOnSuffix (== lf) FL.drain" env $ \inh _ ->
            splitOnSuffix inh
        , mkBench "S.splitOnSeq \"\" FL.drain" env $ \inh _ ->
            splitOnSeq "" inh
        , mkBench "S.splitOnSuffixSeq \"\" FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "" inh
        , mkBench "S.splitOnSeq \"\\n\" FL.drain" env $ \inh _ ->
            splitOnSeq "\n" inh
        , mkBench "S.splitOnSuffixSeq \"\\n\" FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "\n" inh
        , mkBench "S.splitOnSeq \"a\" FL.drain" env $ \inh _ ->
            splitOnSeq "a" inh
        , mkBench "S.splitOnSeq \"\\r\\n\" FL.drain" env $ \inh _ ->
            splitOnSeq "\r\n" inh
        , mkBench "S.splitOnSuffixSeq \"\\r\\n\" FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "\r\n" inh
        , mkBench "S.splitOnSeq \"aa\" FL.drain" env $ \inh _ ->
            splitOnSeq "aa" inh
        , mkBench "S.splitOnSeq \"aaaa\" FL.drain" env $ \inh _ ->
            splitOnSeq "aaaa" inh
        , mkBench "S.splitOnSeq \"abcdefgh\" FL.drain" env $ \inh _ ->
            splitOnSeq "abcdefgh" inh
        , mkBench "S.splitOnSeq \"abcdefghi\" FL.drain" env $ \inh _ ->
            splitOnSeq "abcdefghi" inh
        , mkBench "S.splitOnSeq \"catcatcatcatcat\" FL.drain" env $ \inh _ ->
            splitOnSeq "catcatcatcatcat" inh
        , mkBench "S.splitOnSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            env $ \inh _ -> splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBenchSmall "S.splitOnSuffixSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            env $ \inh _ -> splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-- | Split on a character sequence.
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    (S.length $ IP.splitOnSeq (A.fromList str) FL.drain
        $ IUS.decodeUtf8ArraysLenient
        $ IFH.toChunks inh) -- >>= print

o_1_space_reduce_toChunks_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_toChunks_split env =
    [ bgroup "reduce/toChunks"
        [ mkBenchSmall ("S.splitOnSeqUtf8 \"abcdefgh\" FL.drain "
            ++ ". US.decodeUtf8ArraysLenient") env $ \inh _ ->
                splitOnSeqUtf8 "abcdefgh" inh
        , mkBenchSmall "S.splitOnSeqUtf8 \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_read_chunked env
    , o_1_space_reduce_read env
    , o_1_space_reduce_toBytes env
    , o_1_space_reduce_read_grouped env
    , o_1_space_reduce_read_split env
    , o_1_space_reduce_toChunks_split env
    ]
