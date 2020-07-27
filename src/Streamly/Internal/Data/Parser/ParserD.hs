#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style parser implementation with stream fusion.

module Streamly.Internal.Data.Parser.ParserD
    (
      Parser (..)
    , ParseError (..)
    , Step (..)

    -- First order parsers
    -- * Accumulators
    , fromFold
    , toParserK
    , fromParserK
    , any
    , all
    , yield
    , yieldM
    , die
    , dieM

    -- * Element parsers
    , peek
    , eof
    , satisfy

    -- * Sequence parsers
    --
    -- Parsers chained in series, if one parser terminates the composition
    -- terminates. Currently we are using folds to collect the output of the
    -- parsers but we can use Parsers instead of folds to make the composition
    -- more powerful. For example, we can do:
    --
    -- sliceSepByMax cond n p = sliceBy cond (take n p)
    -- sliceSepByBetween cond m n p = sliceBy cond (takeBetween m n p)
    -- takeWhileBetween cond m n p = takeWhile cond (takeBetween m n p)
    --
    -- Grab a sequence of input elements without inspecting them
    , take
    , takeBetween
    -- , takeLE -- take   -- takeBetween 0 n
    -- , takeLE1 -- take1 -- takeBetween 1 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , lookAhead
    , takeWhileP
    , takeWhile
    , takeWhile1
    , sliceSepByP
    , sliceSepBy
    , sliceSepByMax
    , escapedSliceSepBy
    , escapedFrameBy
    -- , sliceSepByBetween
    , sliceEndWith
    , sliceBeginWith
    , sliceSepWith
    --
    -- , frameSepBy -- parse frames escaped by an escape char/sequence
    -- , frameEndWith
    --
    , wordBy
    , groupBy
    , eqBy
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- ** Sequential Applicative
    , splitWith
    , split_

    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate

    -- ** Sequential Alternative
    , alt

    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest

    -- * N-ary Combinators
    -- ** Sequential Collection
    , sequence
    , concatMap

    -- ** Sequential Repetition
    , count
    , countBetween
    -- , countBetweenTill

    , many
    , some
    , manyTill

    -- -- ** Special cases
    -- XXX traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by "\r\n". The main parser
    -- will have to detect and exclude the sequence "\r\n" anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- , endBy
    -- , sepBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy

    -- -- * Distribution
    --
    -- A simple and stupid impl would be to just convert the stream to an array
    -- and give the array reference to all consumers. The array can be grown on
    -- demand by any consumer and truncated when nonbody needs it.
    --
    -- -- ** Distribute to collection
    -- -- ** Distribute to repetition

    -- -- ** Interleaved collection
    -- Round robin
    -- Priority based
    -- -- ** Interleaved repetition
    -- repeat one parser and when it fails run an error recovery parser
    -- e.g. to find a key frame in the stream after an error

    -- ** Collection of Alternatives
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    , choice   -- first successful in position

    -- -- ** Repeated Alternatives
    -- , retryMax    -- try N times
    -- , retryUntil  -- try until successful
    -- , retryUntilN -- try until successful n times
    )
where

import Control.Exception (assert)
import Control.Monad.Catch (MonadCatch, MonadThrow(..), catchAll)
import Prelude
       hiding (any, all, take, takeWhile, sequence, concatMap)

import Streamly.Internal.Data.Fold.Types (Fold(..))

import qualified Streamly.Internal.Data.Parser.ParserK.Types as K
import qualified Streamly.Internal.Data.Zipper as Z

import Streamly.Internal.Data.Parser.ParserD.Tee
import Streamly.Internal.Data.Parser.ParserD.Types
import Streamly.Internal.Data.Strict

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.fromFold'.
--
-- /Internal/
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser m a b
fromFold (Fold fstep finitial fextract) = Parser step finitial fextract

    where

    step s a = Partial 0 <$> fstep s a

-------------------------------------------------------------------------------
-- Convert to and from CPS style parser representation
-------------------------------------------------------------------------------

-- | Convert a direct style 'Parser' to a CPS style 'K.Parser'.
--
-- /Internal/
--
{-# INLINE_LATE toParserK #-}
toParserK :: MonadCatch m => Parser m a b -> K.Parser m a b
toParserK (Parser step initial extract) =
    K.MkParser $ \inp yieldk ->
        Z.parse step initial extract inp >>= yieldk

-- | Convert a CPS style 'K.Parser' to a direct style 'Parser'.
--
-- /Unimplemented/
--
{-# NOINLINE fromParserK #-}
fromParserK :: Monad m => K.Parser m a b -> Parser m a b
fromParserK _ = Parser step initial extract

    where

    initial = return ()
    step () _ = error "fromParserK: unimplemented"
    extract () = error "fromParserK: unimplemented"

#ifndef DISABLE_FUSION
{-# RULES "fromParserK/toParserK fusion" [2]
    forall s. toParserK (fromParserK s) = s #-}
{-# RULES "toParserK/fromParserK fusion" [2]
    forall s. fromParserK (toParserK s) = s #-}
#endif

-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Parser m a Bool
any predicate = Parser step initial return

    where

    initial = return False

    step s a = return (if s || predicate a then Done 0 True else Partial 0 False)

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parser m a Bool
all predicate = Parser step initial return

    where

    initial = return True

    step s a = return (if s && predicate a then Partial 0 True else Done 0 False)

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.peek'.
--
-- /Internal/
--
{-# INLINABLE peek #-}
peek :: MonadThrow m => Parser m a a
peek = Parser step initial extract

    where

    initial = return ()

    step () a = return $ Done 1 a

    extract () = throwM $ ParseError "peek: end of input"

-- | See 'Streamly.Internal.Data.Parser.eof'.
--
-- /Internal/
--
{-# INLINABLE eof #-}
eof :: Monad m => Parser m a ()
eof = Parser step initial return

    where

    initial = return ()

    step () _ = return $ Error "eof: not at end of input"

-- | See 'Streamly.Internal.Data.Parser.satisfy'.
--
-- /Internal/
--
{-# INLINE satisfy #-}
satisfy :: MonadThrow m => (a -> Bool) -> Parser m a a
satisfy predicate = Parser step initial extract

    where

    initial = return ()

    step () a = return $
        if predicate a
        then Done 0 a
        else Error "satisfy: predicate failed"

    extract _ = throwM $ ParseError "satisfy: end of input"

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.take'.
--
-- /Internal/
--
{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Parser m a b
take n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a
        | i < n = do
            res <- fstep r a
            let i1 = i + 1
                s1 = Tuple' i1 res
            if i1 < n
            then return $ Partial 0 s1
            else Done 0 <$> fextract res
        | otherwise = Done 1 <$> fextract r

    extract (Tuple' _ r) = fextract r

--
-- | See 'Streamly.Internal.Data.Parser.takeBetween'.
--
-- /Internal/
--
{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> Int -> Fold m a b -> Parser m a b
takeBetween low high (Fold fstep finitial fextract) = 
    
    Parser step initial extract

    where
    
    initial = Tuple' 0 <$> finitial

    step (Tuple' numTaken s) a =
        if high == 0
        then Done 1 <$> fextract s
        else
            do
                nextS <- fstep s a
                let nextTaken = numTaken + 1
                if nextTaken < low 
                then return $ Continue 0 (Tuple' nextTaken nextS)
                else
                    if nextTaken < high
                    then return $ Partial 0 (Tuple' nextTaken nextS)
                    else Done 0 <$> fextract nextS

    extract (Tuple' numTaken s) =
        if numTaken >= low
        then fextract s
        else throwM $ ParseError err

        where
        
        err =
               "takeBetween: Expecting alteast " ++ show low
            ++ " elements, got " ++ show numTaken

-- | See 'Streamly.Internal.Data.Parser.takeEQ'.
--
-- /Internal/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeEQ cnt (Fold fstep finitial fextract) = Parser step initial extract

    where

    n = max cnt 0

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a
      | i < n = do
          res <- fstep r a
          let i1 = i + 1
              s1 = Tuple' i1 res
          if i1 < n
          then return (Continue 0 s1)
          else Done 0 <$> fextract res
      | otherwise = Done 1 <$> fextract r

    extract (Tuple' i r) =
        if n == i
        then fextract r
        else throwM $ ParseError err

        where

        err =
               "takeEQ: Expecting exactly " ++ show n
            ++ " elements, got " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeGE'.
--
-- /Internal/
--
{-# INLINE takeGE #-}
takeGE :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeGE cnt (Fold fstep finitial fextract) = Parser step initial extract
    where

    n = max cnt 0

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        return $
            if i1 < n
            then Continue 0 s1
            else Partial 0 s1

    extract (Tuple' i r) = fextract r >>= f

        where

        err =
              "takeGE: Expecting at least " ++ show n
           ++ " elements, got only " ++ show i

        f x =
            if i >= n
            then return x
            else throwM $ ParseError err

-- | See 'Streamly.Internal.Data.Parser.takeWhileP'.
--
-- /Internal/
--
{-# INLINE takeWhileP #-}
takeWhileP :: MonadCatch m => (a -> Bool) -> Parser m a b -> Parser m a b
takeWhileP predicate (Parser stepP initialP extractP) = 

    Parser step initial extract

    where
    
    initial = initialP

    step s a =
        if not $ predicate a
        then Done 1 <$> extractP s
        else stepP s a
    
    extract = extractP

-- | See 'Streamly.Internal.Data.Parser.takeWhile'.
--
-- /Internal/
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial

    step s a =
        if predicate a
        then Partial 0 <$> fstep s a
        else Done 1 <$> fextract s

-- | See 'Streamly.Internal.Data.Parser.takeWhile1'.
--
-- /Internal/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadThrow m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = return Nothing

    step Nothing a =
        if predicate a
        then do
            s <- finitial
            r <- fstep s a
            return $ Partial 0 (Just r)
        else return $ Error "takeWhile1: empty"
    step (Just s) a =
        if predicate a
        then do
            r <- fstep s a
            return $ Partial 0 (Just r)
        else do
            b <- fextract s
            return $ Done 1 b

    extract Nothing = throwM $ ParseError "takeWhile1: end of input"
    extract (Just s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.sliceSepByP'.
--
-- /Internal/
--
{-# INLINE sliceSepByP #-}
sliceSepByP :: MonadCatch m => (a -> Bool) -> Parser m a b -> Parser m a b
sliceSepByP predicate (Parser stepP initialP extractP) = 

    Parser step initial extract

    where
    
    initial = initialP

    step s a =
        if predicate a
        then Done 0 <$> extractP s
        else stepP s a
    
    extract = extractP

-- | See 'Streamly.Internal.Data.Parser.sliceSepBy'.
--
-- /Internal/
--
{-# INLINABLE sliceSepBy #-}
sliceSepBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceSepBy predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial
    step s a =
        if not (predicate a)
        then Partial 0 <$> fstep s a
        else Done 0 <$> fextract s

-- | See 'Streamly.Internal.Data.Parser.sliceSepWith'.
--
-- /Internal/
--
{-# INLINE sliceSepWith #-}
sliceSepWith :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceSepWith predicate (Fold fstep finitial fextract) =

    Parser step initial extract

    where
    
    initial = Tuple' True <$> finitial

    step (Tuple' isFirstElement s) a =
        if predicate a
        then
            if isFirstElement
            then
                do
                    nextS <- fstep s a
                    Done 0 <$> fextract nextS
            else
                Done 1 <$> fextract s
        else
            do
                nextS <- fstep s a
                return $ Partial 0 (Tuple' False nextS)

    extract (Tuple' _ s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.sliceEndWith'.
--
-- /Internal/
--
{-# INLINE sliceEndWith #-}
sliceEndWith :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceEndWith predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial
    step s a =
        if not (predicate a)
        then Partial 0 <$> fstep s a
        else Done 0 <$> (fstep s a >>= fextract)

-- | See 'Streamly.Internal.Data.Parser.sliceBeginWith'.
--
-- /Internal/
--
{-# INLINE sliceBeginWith #-}
sliceBeginWith :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceBeginWith predicate (Fold fstep finitial fextract) = 
    Parser step initial extract

    where
    
    initial = (Tuple' True) <$> finitial

    step (Tuple' isFirstElement s) a =
        if not (predicate a) || isFirstElement
        then
            do
                nextS <- fstep s a
                return $ Partial 0 (Tuple' False nextS)
        else Done 1 <$> fextract s

    extract (Tuple' _ s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.sliceSepByMax'.
--
-- /Internal/
--
{-# INLINABLE sliceSepByMax #-}
sliceSepByMax :: Monad m
    => (a -> Bool) -> Int -> Fold m a b -> Parser m a b
sliceSepByMax predicate cnt (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a
        | not (predicate a) =
            if i < cnt
            then do
                res <- fstep r a
                let i1 = i + 1
                    s1 = Tuple' i1 res
                return $ Partial 0 s1
            else Done 1 <$> fextract r
        | otherwise = Done 0 <$> fextract r

    extract (Tuple' _ r) = fextract r

-- | See 'Streamly.Internal.Data.Parser.escapedSliceSepBy'.
--
-- /Internal/
--
{-# INLINE escapedSliceSepBy #-}
escapedSliceSepBy :: 
    MonadCatch m => (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser m a b
escapedSliceSepBy isSep isEsc (Fold fstep finitial fextract) =

    Parser step initial extract

    where
    
    initial = Tuple' Nothing <$> finitial

    step (Tuple' maybePrevEscape s) a =
        case maybePrevEscape of
            Nothing ->
                if isEsc a
                then return $ Partial 0 (Tuple' (Just a) s)
                else
                    if isSep a
                    then Done 0 <$> fextract s
                    else
                        do
                            nextS <- fstep s a
                            return $ Partial 0 (Tuple' Nothing nextS)
            Just prevEsc ->
                if isEsc a || isSep a
                then
                    do
                        nextS <- fstep s a
                        return $ Partial 0 (Tuple' Nothing nextS)
                else
                    if isSep prevEsc
                    then Done 1 <$> fextract s
                    else 
                        do
                            s1 <- fstep s prevEsc
                            s2 <- fstep s1 a
                            return $ Partial 0 (Tuple' Nothing s2)

    extract (Tuple' maybePrevEscape s) =
        case maybePrevEscape of
            Nothing -> fextract s
            Just prevEsc ->
                do
                    nextS <- fstep s prevEsc
                    fextract nextS

-- | See 'Streamly.Internal.Data.Parser.escapedFrameBy'.
--
-- /Internal/
--
{-# INLINE escapedFrameBy #-}
escapedFrameBy ::
    MonadCatch m 
    => (a -> Bool)
    -> (a -> Bool)
    -> (a -> Bool)
    -> Fold m a b
    -> Parser m a b
escapedFrameBy begin end escape (Fold fstep finitial fextract) =

    Parser step initial extract

    where
    
    initial = Tuple3' Nothing (0 :: Int) <$> finitial

    step (Tuple3' maybePrevEsc openMinusClose s) a =
        if (begin a && end a) || (end a && escape a) || (escape a && begin a)
            then return $ 
                Error "Element found to satisfy more than one predicate"
        else
            case maybePrevEsc of
                Nothing ->
                    if escape a
                    then
                        return $ 
                            Continue
                            0 
                            (Tuple3' (Just a) openMinusClose s)
                    else
                        if begin a
                        then
                            return $
                                Continue
                                0 
                                (Tuple3' Nothing (openMinusClose + 1) s)
                        else
                            if end a
                            then
                                case openMinusClose of
                                    0 -> return $ Error "Found end before any begin"
                                    1 -> Done 0 <$> fextract s
                                    _ ->
                                        return $ 
                                            Continue
                                            0
                                            (Tuple3'
                                            Nothing
                                            (openMinusClose - 1)
                                            s)
                            else
                                do
                                    nextS <- fstep s a
                                    return $
                                        Continue
                                        0
                                        (Tuple3' Nothing openMinusClose nextS)
                (Just prevEsc) ->
                    if escape a || begin a || end a
                    then
                        do
                            nextS <- fstep s a
                            return $
                                Continue 0 (Tuple3' Nothing openMinusClose nextS)
                    else
                        do
                            s1 <- fstep s prevEsc
                            s2 <- fstep s1 a
                            return $
                                Continue
                                0
                                (Tuple3' Nothing openMinusClose s2)

    extract _ = 
        throwM $ ParseError "Unterminated begin"

-- | See 'Streamly.Internal.Data.Parser.wordBy'.
--
-- /Internal/
--
{-# INLINE wordBy #-}
wordBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
wordBy predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where
    
    initial = Tuple3' False False <$> finitial

    step (Tuple3' sawElement sawSepAfterElement s) a =
        if sawElement
        then
            if not (predicate a)
            then
                if sawSepAfterElement 
                then Done 1 <$> fextract s
                else
                    do
                        nextS <- fstep s a
                        return $ Partial 0 (Tuple3' True False nextS)
            else return $ Partial 0 (Tuple3' True True s)
        else
            if not (predicate a)
            then 
                do
                    nextS <- fstep s a
                    return $ Partial 0 (Tuple3' True sawSepAfterElement nextS)
            else return $ Partial 0 (Tuple3' False sawSepAfterElement s)

    extract (Tuple3' _ _ s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.groupBy'.
--
-- /Internal/
--
{-# INLINE groupBy #-}
groupBy :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy cmp (Fold fstep finitial fextract) =
    Parser step initial extract

    where

        initial = Tuple' Nothing <$> finitial

        step (Tuple' maybeInitElement s) a =
            case maybeInitElement of
                Nothing ->
                    do
                        nextS <- fstep s a
                        return $ Partial 0 (Tuple' (Just a) nextS)
                Just initElement ->
                    if cmp initElement a
                    then
                        do
                            nextS <- fstep s a
                            return $ Partial 0 (Tuple' (Just initElement) nextS)
                    else Done 1 <$> fextract s

        extract (Tuple' _ s) = fextract s

-- XXX use an Unfold instead of a list?
-- XXX custom combinators for matching list, array and stream?
--
-- | See 'Streamly.Internal.Data.Parser.eqBy'.
--
-- /Internal/
--
{-# INLINE eqBy #-}
eqBy :: MonadThrow m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp str = Parser step initial extract

    where

    initial = return str

    step [] _ = return $ Done 0 ()
    step [x] a = return $
        if x `cmp` a
        then Done 0 ()
        else Error "eqBy: failed, yet to match the last element"
    step (x:xs) a = return $
        if x `cmp` a
        then Continue 0 xs
        else Error $
            "eqBy: failed, yet to match " ++ show (length xs + 1) ++ " elements"

    extract xs = throwM $ ParseError $
        "eqBy: end of input, yet to match " ++ show (length xs) ++ " elements"

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.lookahead'.
--
-- /Internal/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Parser m a b -> Parser m a b
lookAhead (Parser step1 initial1 _) =
    Parser step initial extract

    where

    initial = Tuple' 0 <$> initial1

    step (Tuple' cnt st) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        return $ case r of
            Partial n s -> Continue n (Tuple' (cnt1 - n) s)
            Continue n s -> Continue n (Tuple' (cnt1 - n) s)
            Done _ b -> Done cnt1 b
            Error err -> Error err

    -- XXX returning an error let's us backtrack.  To implement it in a way so
    -- that it terminates on eof without an error then we need a way to
    -- backtrack on eof, that will require extract to return 'Step' type.
    extract (Tuple' n _) = throwM $ ParseError $
        "lookAhead: end of input after consuming " ++ show n ++ " elements"

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

data ParserTurn = Parser1 | Parser2

--
-- | See 'Streamly.Internal.Data.Parser.deintercalate'.
--
-- /Internal/
--
{-# INLINE deintercalate #-}
deintercalate ::
    MonadCatch m
    => Fold m a y 
    -> Parser m x a
    -> Fold m b z 
    -> Parser m x b
    -> Parser m x (y, z)
deintercalate
    (Fold fstep1 finitial1 fextract1)
    (Parser pstep1 pinitial1 pextract1)
    (Fold fstep2 finitial2 fextract2)
    (Parser pstep2 pinitial2 pextract2) =

    Parser step initial extract

    where
    
    initial = do
        finit1 <- finitial1
        pinit1 <- pinitial1
        finit2 <- finitial2
        pinit2 <- pinitial2
        return (finit1, pinit1, finit2, pinit2, Parser1, 0)
    
    step (fs1, ps1, fs2, ps2, currentParser, numBuffered) a =
        case currentParser of
            Parser1 -> do
                st <- pstep1 ps1 a
                case st of
                    
                    Partial n ps1new ->
                        return $ 
                            Partial n (fs1, ps1new, fs2, ps2, currentParser, 0)

                    Continue n ps1new -> do
                        let 
                            newNumBuffered =
                                if n == 0 
                                then numBuffered + 1
                                else numBuffered - n

                        return $ 
                            Continue 
                            n 
                            (fs1, ps1new, fs2, ps2, currentParser, newNumBuffered)

                    Done n result -> do
                        fs1new <- fstep1 fs1 result
                        pinit1 <- pinitial1
                        pinit2 <- pinitial2
                        return $ 
                            Partial 
                            n 
                            ( fs1new
                            , pinit1
                            , fs2
                            , pinit2
                            , Parser2
                            , numBuffered
                            )
                    
                    Error _ -> do
                        result1 <- fextract1 fs1
                        result2 <- fextract2 fs2
                        return $ Done numBuffered (result1, result2)

            Parser2 -> do
                st <- pstep2 ps2 a
                case st of
                    
                    Partial n ps2new ->
                        return $ 
                            Partial n (fs1, ps1, fs2, ps2new, currentParser, 0)

                    Continue n ps2new -> do
                        let 
                            newNumBuffered =
                                if n == 0 
                                then numBuffered + 1
                                else numBuffered - n

                        return $ 
                            Continue 
                            n 
                            (fs1, ps1, fs2, ps2new, currentParser, newNumBuffered)

                    Done n result -> do
                        fs2new <- fstep2 fs2 result
                        pinit1 <- pinitial1
                        pinit2 <- pinitial2
                        return $ 
                            Partial 
                            n
                            ( fs1
                            , pinit1
                            , fs2new
                            , pinit2
                            , Parser1
                            , numBuffered
                            )

                    Error _ -> do
                        result1 <- fextract1 fs1
                        result2 <- fextract2 fs2
                        return $ Done numBuffered (result1, result2)
        
    extract (fs1, ps1, fs2, ps2, currentParser, _) =
        case currentParser of
            Parser1 -> do
                maybeRes <- catchAll 
                    (fmap Just $ pextract1 ps1) 
                    (\_ -> return Nothing)
                fs1new <- case maybeRes of
                    Nothing -> return fs1
                    Just res -> fstep1 fs1 res
                result1 <- fextract1 fs1new
                result2 <- fextract2 fs2
                return (result1, result2)
            
            Parser2 -> do
                maybeRes <- catchAll 
                    (fmap Just $ pextract2 ps2) 
                    (\_ -> return Nothing)
                fs2new <- case maybeRes of
                    Nothing -> return fs2
                    Just res -> fstep2 fs2 res
                result1 <- fextract1 fs1
                result2 <- fextract2 fs2new
                return (result1, result2)

-------------------------------------------------------------------------------
-- Sequential Collection
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.sequence'.
--
-- /Unimplemented/
--
{-# INLINE sequence #-}
sequence ::
    -- Foldable t =>
    Fold m b c -> t (Parser m a b) -> Parser m a c
sequence _f _p = undefined

-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.choice'.
--
-- /Unimplemented/
--
{-# INLINE choice #-}
choice ::
    -- Foldable t =>
    t (Parser m a b) -> Parser m a b
choice _ps = undefined

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.many'.
--
-- /Internal/
--
{-# INLINE many #-}
many :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
many = splitMany
-- many = countBetween 0 maxBound

-- | See 'Streamly.Internal.Data.Parser.some'.
--
-- /Internal/
--
{-# INLINE some #-}
some :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
some = splitSome
-- some f p = many (takeGE 1 f) p
-- many = countBetween 1 maxBound

-- | See 'Streamly.Internal.Data.Parser.countBetween'.
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween ::
    -- MonadCatch m =>
    Int -> Int -> Fold m b c -> Parser m a b -> Parser m a c
countBetween _m _n _f = undefined
-- countBetween m n f p = many (takeBetween m n f) p

-- | See 'Streamly.Internal.Data.Parser.count'.
--
-- /Unimplemented/
--
{-# INLINE count #-}
count ::
    -- MonadCatch m =>
    Int -> Fold m b c -> Parser m a b -> Parser m a c
count n = countBetween n n
-- count n f p = many (takeEQ n f) p

data ManyTillState fs sr sl = ManyTillR Int fs sr | ManyTillL fs sl

-- | See 'Streamly.Internal.Data.Parser.manyTill'.
--
-- /Internal/
--
{-# INLINE manyTill #-}
manyTill :: MonadCatch m
    => Fold m b c -> Parser m a b -> Parser m a x -> Parser m a c
manyTill (Fold fstep finitial fextract)
         (Parser stepL initialL extractL)
         (Parser stepR initialR _) =
    Parser step initial extract

    where

    initial = do
        fs <- finitial
        ManyTillR 0 fs <$> initialR

    step (ManyTillR cnt fs st) a = do
        r <- stepR st a
        case r of
            Partial n s -> return $ Partial n (ManyTillR 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillR (cnt + 1 - n) fs s)
            Done n _ -> do
                b <- fextract fs
                return $ Done n b
            Error _ -> do
                rR <- initialL
                return $ Continue (cnt + 1) (ManyTillL fs rR)

    step (ManyTillL fs st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ManyTillL fs s)
            Continue n s -> return $ Continue n (ManyTillL fs s)
            Done n b -> do
                fs1 <- fstep fs b
                l <- initialR
                return $ Partial n (ManyTillR 0 fs1 l)
            Error err -> return $ Error err

    extract (ManyTillL fs sR) = extractL sR >>= fstep fs >>= fextract
    extract (ManyTillR _ fs _) = fextract fs
