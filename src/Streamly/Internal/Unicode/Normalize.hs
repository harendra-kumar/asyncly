-- |
-- Module      : Streamly.Internal.Unicode.Normalize
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

#include "inline.hs"

module Streamly.Internal.Unicode.Normalize
    ( NormalizationMode(..)
    , partialDecomposeD
    , decomposeD
    , partialComposeD
    , normalizeD
    , normalize
    ) where

import Data.Char (chr, ord)
import Data.Typeable (Typeable)
import Data.Unicode.Properties.Decompose (DecomposeMode(..))
import Streamly.Internal.Data.Stream.IsStream (IsStream)
import Streamly.Internal.Data.Stream.StreamD (Stream(..), Step (..))

import qualified Streamly.Internal.Data.Stream.StreamD as SD
import qualified Data.Unicode.Properties.CombiningClass as CC
import qualified Data.Unicode.Properties.Compositions as C
import qualified Data.Unicode.Properties.Decompose as D
import qualified Data.Unicode.Properties.DecomposeHangul as H

data NormalizationMode
    = NFD    -- ^ Canonical decomposition.
    | NFKD   -- ^ Compatibility decomposition.
    | NFC    -- ^ Canonical decomposition followed by canonical composition.
    | NFKC   -- ^ Compatibility decomposition followed by canonical composition.
      deriving (Eq, Show, Enum, Typeable)

-------------------------------------------------------------------------------
-- Normalization combinators
-------------------------------------------------------------------------------

type ReBuf = [Char]

{-# INLINE insertIntoReBuf #-}
insertIntoReBuf :: Char -> ReBuf -> ReBuf
insertIntoReBuf c [] = [c]
insertIntoReBuf c xxs@(x:xs)
    | CC.getCombiningClass c < CC.getCombiningClass x = c : xxs
    | otherwise = x : insertIntoReBuf c xs

-- {-# ANN type DecomposeState Fuse #-}
data DecomposeState st
    = YieldCharList [Char] (DecomposeState st)
    | ReadInputChar ReBuf st
    | IsHangul Char st
    | IsDecomposable [Char] ReBuf st
    | DecomposeStop

{-# INLINE_NORMAL partialDecomposeD #-}
partialDecomposeD ::
       Monad m => Bool -> DecomposeMode -> Stream m Char -> Stream m Char
partialDecomposeD decomposeHangul mode (Stream step state) =
    Stream sstep (ReadInputChar [] state)

    where

    {-# INLINE_LATE sstep #-}
    -- XXX Does this cause any problem?
    sstep _ (YieldCharList [] ns) = return $ Skip ns
    sstep _ (YieldCharList (ch:chs) ns) =
        return $ Yield ch (YieldCharList chs ns)
    sstep gst (ReadInputChar rebuf st) = do
        res <- step gst st
        return
          $ Skip
          $ case res of
                Yield ch st1
                    | D.isHangul ch ->
                        if decomposeHangul
                        then YieldCharList rebuf (IsHangul ch st1)
                        else YieldCharList
                                 (rebuf ++ [ch])
                                 (ReadInputChar [] st1)
                    | D.isDecomposable mode ch ->
                        IsDecomposable (D.decomposeChar mode ch) rebuf st1
                    | otherwise ->
                        if CC.isCombining ch
                        then ReadInputChar (insertIntoReBuf ch rebuf) st1
                        else YieldCharList
                                 (rebuf ++ [ch])
                                 (ReadInputChar [] st1)
                Skip st1 -> ReadInputChar rebuf st1
                Stop -> YieldCharList rebuf DecomposeStop
    sstep _ (IsHangul ch st) =
        return
          $ Skip
          $ let (l, v, t) = D.decomposeCharHangul ch
             in if t == chr H.jamoTFirst
                then YieldCharList [l, v] (ReadInputChar [] st)
                else YieldCharList [l, v, t] (ReadInputChar [] st)
    sstep _ (IsDecomposable [] rebuf st) =
        return $ Skip $ ReadInputChar rebuf st
    sstep _ (IsDecomposable (ch:chs) rebuf st)
        | D.isDecomposable mode ch =
            return
              $ Skip $ IsDecomposable (D.decomposeChar mode ch ++ chs) rebuf st
        | otherwise =
            return
              $ Skip
              $ if CC.isCombining ch
                then IsDecomposable chs (insertIntoReBuf ch rebuf) st
                else YieldCharList (rebuf ++ [ch]) (IsDecomposable chs [] st)
    sstep _ DecomposeStop = return Stop

{-# INLINE decomposeD #-}
decomposeD :: Monad m => DecomposeMode -> Stream m Char -> Stream m Char
decomposeD = partialDecomposeD True

-- Hold an L to wait for V, hold an LV to wait for T.
data JamoBuf
    = Jamo !Char -- Jamo L, V or T
    | Hangul !Char -- Hangul Syllable LV or LVT
    | HangulLV !Char

{-# INLINE fromJamoBuf #-}
fromJamoBuf :: JamoBuf -> Char
fromJamoBuf (Jamo ch) = ch
fromJamoBuf (Hangul ch) = ch
fromJamoBuf (HangulLV ch) = ch


-- {-# ANN type ComposeState Fuse #-}
data ComposeState st
    = YieldChar Char (ComposeState st)
    | YieldList [Char] (ComposeState st)
    | ComposeNone st
    | ComposeReg Int [Char] st
    | ComposeJamo JamoBuf st
    | ComposeStop

-- Assumes every character except hangul characters are fully decomposed and the
-- combining characters are reordered. Hangul characters may or may not be
-- decomposed.
{-# INLINE_EARLY partialComposeD #-}
partialComposeD :: Monad m => Stream m Char -> Stream m Char
partialComposeD (Stream step state) = Stream step' (ComposeNone state)

    where

    {-# INLINE_NORMAL step' #-}
    step' _ ComposeStop = return Stop
    step' _ (YieldChar ch ns) = return $ Yield ch ns
    step' _ (YieldList [] ns) = return $ Skip ns
    step' _ (YieldList (x:xs) ns) = return $ Yield x $ YieldList xs ns
    step' gst (ComposeNone st) = do
        r <- step gst st
        return
          $ case r of
                Yield x st1 -> Skip $ composeNone x st1
                Skip st1 -> Skip $ ComposeNone st1
                Stop -> Stop
    step' gst (ComposeJamo jbuf st) = do
        r <- step gst st
        return
          $ case r of
                Yield x st1 -> Skip $ composeJamo jbuf x st1
                Skip st1 -> Skip $ ComposeJamo jbuf st1
                Stop -> Skip $ YieldChar (fromJamoBuf jbuf) ComposeStop
    step' gst (ComposeReg i rbuf st) = do
        r <- step gst st
        return
          $ case r of
                Yield x st1 -> Skip $ composeReg i rbuf x st1
                Skip st1 -> Skip $ ComposeReg i rbuf st1
                Stop -> Skip $ YieldList rbuf ComposeStop

    {-# INLINE initHangul #-}
    initHangul c st = ComposeJamo (Hangul c) st

    {-# INLINE initJamo #-}
    initJamo c st = ComposeJamo (Jamo c) st

    {-# INLINE initReg #-}
    initReg !c st = ComposeReg 0 [c] st

    {-# INLINE composeNone #-}
    composeNone ch st
        | H.isHangul ch = initHangul ch st
        | H.isJamo ch = initJamo ch st
        | otherwise = initReg ch st

    {-# INLINE composeCharHangul #-}
    composeCharHangul jbuf ch st =
        YieldChar (fromJamoBuf jbuf) $ ComposeJamo (Hangul ch) st

    {-# INLINE composeCharJamo #-}
    composeCharJamo jbuf ch st
        | ich <= H.jamoLLast =
            YieldChar (fromJamoBuf jbuf) $ ComposeJamo (Jamo ch) st
        | ich < H.jamoVFirst = flushAndWrite jbuf ch st
        | ich <= H.jamoVLast =
            case jbuf of
                Jamo c ->
                    case H.jamoLIndex c of
                        Just li ->
                            let vi = ich - H.jamoVFirst
                                lvi = li * H.jamoNCount + vi * H.jamoTCount
                                lv = chr (H.hangulFirst + lvi)
                             in ComposeJamo (HangulLV lv) st
                        Nothing -> writeTwo c ch st
                Hangul c -> writeTwo c ch st
                HangulLV c -> writeTwo c ch st
        | ich <= H.jamoTFirst = flushAndWrite jbuf ch st
        | otherwise = do
            let ti = ich - H.jamoTFirst
            case jbuf of
                Jamo c -> writeTwo c ch st
                Hangul c
                    | H.isHangulLV c -> writeLVT c ti st
                    | otherwise -> writeTwo c ch st
                HangulLV c -> writeLVT c ti st

        where

        flushAndWrite jb c s = YieldList [fromJamoBuf jb, c] $ ComposeNone s

        writeLVT lv ti s =
            let lvt = chr $ ord lv + ti
             in YieldChar lvt $ ComposeNone s

        writeTwo c1 c2 s = YieldList [c1, c2] $ ComposeNone s

        ich = ord ch

    {-# INLINE composeJamo #-}
    composeJamo jbuf ch st
        | H.isJamo ch = composeCharJamo jbuf ch st
        | H.isHangul ch = composeCharHangul jbuf ch st
        | otherwise = YieldChar (fromJamoBuf jbuf) (ComposeReg 0 [ch] st)

    -- i ~ CC.getCombiningClass (last rbuf)
    {-# INLINE composeCharCombining #-}
    composeCharCombining i rbuf ch st =
        if cch > i
        then case C.composePair str ch of
                 Nothing -> ComposeReg cch (rbuf ++ [ch]) st
                 Just x -> ComposeReg i (x : tail rbuf) st
        else ComposeReg i (rbuf ++ [ch]) st

        where

        str = head rbuf
        cch = CC.getCombiningClass ch

    {-# INLINE composeReg #-}
    composeReg i rbuf !ch !st
        | H.isHangul ch = YieldList rbuf $ initHangul ch st
        | H.isJamo ch = YieldList rbuf $ initJamo ch st
        | CC.isCombining ch = composeCharCombining i rbuf ch st
        | [s] <- rbuf
        , C.isSecondStarter ch
        , Just x <- C.composeStarterPair s ch = ComposeReg 0 [x] st
        | otherwise = YieldList rbuf $ ComposeReg 0 [ch] st

normalizeD :: Monad m => NormalizationMode -> Stream m Char -> Stream m Char
normalizeD NFD = partialDecomposeD True DecomposeNFD
normalizeD NFKD = partialDecomposeD True DecomposeNFKD
normalizeD NFC = partialComposeD . partialDecomposeD False DecomposeNFD
normalizeD NFKC = partialComposeD . partialDecomposeD False DecomposeNFKD

normalize :: (IsStream t, Monad m) => NormalizationMode -> t m Char -> t m Char
normalize mode = SD.fromStreamD . normalizeD mode  . SD.toStreamD
