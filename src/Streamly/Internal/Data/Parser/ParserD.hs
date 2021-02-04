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
    , yield
    , yieldM
    , die
    , dieM

    -- * Element parsers
    , peek
    , eof
    , satisfy
    , maybe
    , either

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
    , takeBetween
    -- , takeLE -- take   -- takeBetween 0 n
    -- , takeLE1 -- take1 -- takeBetween 1 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , lookAhead
    , takeWhile
    , takeWhile1
    , sliceSepByP
    -- , sliceSepByBetween
    , sliceBeginWith
    -- , sliceSepWith
    --
    -- , frameSepBy -- parse frames escaped by an escape char/sequence
    -- , frameEndWith
    --
    , wordBy
    , groupBy
    , groupByRolling
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
import Control.Monad.Catch (MonadCatch, MonadThrow(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Fold.Types as FL

import Prelude hiding
       (any, all, take, takeWhile, sequence, concatMap, maybe, either)
import Streamly.Internal.Data.Parser.ParserD.Tee
import Streamly.Internal.Data.Parser.ParserD.Types

-- XXX Parsers will also have a cleanup action later. The correct behaviour is
-- for the parsers to clean up the Fold resources while extracting. This is
-- omitted now.

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
fromFold (Fold fstep finitial fextract _) = Parser step finitial extract

    where

    step (FL.Partial s) a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 (FL.Partial s1)
                  FL.Done b -> Done 0 b
    step (FL.Done b) _ = return $ Done 1 b

    extract (FL.Partial s) = fextract s
    extract (FL.Done b) = return b

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

-- | See 'Streamly.Internal.Data.Parser.maybe'.
--
-- /Internal/
--
{-# INLINE maybe #-}
maybe :: MonadThrow m => (a -> Maybe b) -> Parser m a b
maybe parser = Parser step initial extract

    where

    initial = return ()

    step () a = return $
        case parser a of
            Just b -> Done 0 b
            Nothing -> Error "maybe: predicate failed"

    extract _ = throwM $ ParseError "maybe: end of input"

-- | See 'Streamly.Internal.Data.Parser.either'.
--
-- /Internal/
--
{-# INLINE either #-}
either :: MonadThrow m => (a -> Either String b) -> Parser m a b
either parser = Parser step initial extract

    where

    initial = return ()

    step () a = return $
        case parser a of
            Right b -> Done 0 b
            Left err -> Error $ "either: " ++ err

    extract _ = throwM $ ParseError "either: end of input"

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.takeBetween'.
--
-- /Internal/
--
{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> Int -> Fold m a b -> Parser m a b
takeBetween low high (Fold fstep finitial fextract fcleanup) =

    Parser step initial extract

    where

    initial = do
        res <- finitial
        case res of
            FL.Partial s -> return $ Tuple' 0 s
            FL.Done _ ->
                error "takeBetween: Done/Error in initial not implemented yet."

    step (Tuple' i s) a
        | low > high =
            throwM
                $ ParseError
                $ "takeBetween: lower bound - " ++ show low
                    ++ " is greater than higher bound - " ++ show high
        | high <= 0 = Done 1 <$> FL.finalExtract fextract fcleanup s
        | i1 < low = do
            res <- fstep s a
            return
                $ case res of
                    FL.Partial s1 -> Continue 0 $ Tuple' i1 s1
                    FL.Done _ ->
                        Error
                            $ "takeBetween: the collecting fold terminated after"
                                ++ " consuming" ++ show i1 ++ " elements"
                                ++ " minimum" ++ show low ++ " elements needed"
        | otherwise = do
            res <- fstep s a
            case res of
                FL.Partial s1 ->
                    if i1 >= high
                    then Done 0 <$> FL.finalExtract fextract fcleanup s1
                    else return $ Partial 0 $ Tuple' i1 s1
                FL.Done b -> return $ Done 0 b

        where

        i1 = i + 1

    extract (Tuple' i s)
        | i >= low && i <= high = fextract s
        | otherwise = throwM $ ParseError err

        where

        err =
               "takeBetween: Expecting alteast " ++ show low
            ++ " elements, got " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeEQ'.
--
-- /Internal/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeEQ n (Fold fstep finitial fextract fcleanup) = Parser step initial extract

    where

    cnt = max n 0

    initial = do
        res <- finitial
        case res of
            FL.Partial s -> return $ Tuple' 0 s
            FL.Done _ ->
                error "takeEQ: Done/Error in initial not implemented yet."

    step (Tuple' i r) a
        | i1 < cnt = do
            res <- fstep r a
            return
                $ case res of
                    FL.Partial s -> Continue 0 $ Tuple' i1 s
                    FL.Done _ ->
                        Error
                            $ "takeEQ: Expecting exactly " ++ show cnt
                                ++ " elements, fold terminated on " ++ show i1
        | i1 == cnt = do
            res <- fstep r a
            Done 0
                <$> case res of
                        FL.Partial s -> FL.finalExtract fextract fcleanup s
                        FL.Done b -> return b
        -- XXX we should not reach here when initial returns Step type
        -- reachable only when n == 0
        | otherwise = Done 1 <$> FL.finalExtract fextract fcleanup r

        where

        i1 = i + 1

    extract (Tuple' i r)
        | i == 0 && cnt == 0 = fextract r
        | otherwise =
            throwM
                $ ParseError
                $ "takeEQ: Expecting exactly " ++ show cnt
                    ++ " elements, input terminated on " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeGE'.
--
-- /Internal/
--
{-# INLINE takeGE #-}
takeGE :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeGE n (Fold fstep finitial fextract _) = Parser step initial extract

    where

    cnt = max n 0
    initial = do
        res <- finitial
        case res of
            FL.Partial s -> return $ Tuple' 0 s
            FL.Done _ ->
                error "takeGE: Done/Error in initial not implemented yet."

    step (Tuple' i r) a
        | i1 < cnt = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Continue 0 $ Tuple' i1 s
                      FL.Done _ ->
                        Error
                            $ "takeGE: Expecting at least " ++ show cnt
                                ++ " elements, fold terminated on " ++ show i1
        | otherwise = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Partial 0 $ Tuple' i1 s
                      FL.Done b -> Done 0 b

        where

        i1 = i + 1

    extract (Tuple' i r)
        | i >= cnt = fextract r
        | otherwise =
            throwM
                $ ParseError
                $ "takeGE: Expecting at least " ++ show cnt
                    ++ " elements, input terminated on " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeWhile'.
--
-- /Internal/
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile predicate (Fold fstep finitial fextract fcleanup) =
    Parser step initial fextract

    where

    initial = do
        res <- finitial
        case res of
            FL.Partial s -> return s
            FL.Done _ ->
                error "takeWhile: Done/Error in initial not implemented yet."

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      FL.Partial s1 -> Partial 0 s1
                      FL.Done b -> Done 0 b
        else Done 1 <$> FL.finalExtract fextract fcleanup s

-- | See 'Streamly.Internal.Data.Parser.takeWhile1'.
--
-- /Internal/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadThrow m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 predicate (Fold fstep finitial fextract fcleanup) =
    Parser step initial extract

    where

    initial = return Nothing

    step Nothing a =
        let err = Error "takeWhile1: predicate failed on first element"
         in if predicate a
            then do
                res <- finitial
                case res of
                    FL.Partial s -> do
                        sr <- fstep s a
                        return
                            $ case sr of
                                  FL.Partial s1 -> Partial 0 (Just s1)
                                  FL.Done b -> Done 0 b
                    FL.Done _ -> return err
            else return err
    step (Just s) a =
        if predicate a
        then do
            sr <- fstep s a
            case sr of
                FL.Partial r -> return $ Partial 0 (Just r)
                FL.Done b -> return $ Done 0 b
        else do
            b <- FL.finalExtract fextract fcleanup s
            return $ Done 1 b

    extract Nothing = throwM $ ParseError "takeWhile1: end of input"
    extract (Just s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.sliceSepByP'.
--
-- /Internal/
--
sliceSepByP :: MonadCatch m =>
    (a -> Bool) -> Parser m a b -> Parser m a b
sliceSepByP cond (Parser pstep pinitial pextract) =

    Parser step initial pextract

    where

    initial = pinitial

    step s a =
        if cond a
        then do
            res <- pextract s
            return $ Done 0 res
        else pstep s a

-- | See 'Streamly.Internal.Data.Parser.sliceBeginWith'.
--
-- /Unimplemented/
--
{-# INLINABLE sliceBeginWith #-}
sliceBeginWith ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceBeginWith = undefined

data WordByState s b = WBLeft !s | WBWord !s | WBRight !b

-- | See 'Streamly.Internal.Data.Parser.wordBy'.
--
--
{-# INLINE wordBy #-}
wordBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
wordBy predicate (Fold fstep finitial fextract fcleanup) =
    Parser step initial extract

    where

    {-# INLINE worder #-}
    worder s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 $ WBWord s1
                  FL.Done b -> Done 0 b

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> WBLeft s
                  FL.Done _ -> error "wordBy: Done in initial not implemented"

    step (WBLeft s) a =
        if not (predicate a)
        then worder s a
        else return $ Partial 0 $ WBLeft s
    step (WBWord s) a =
        if not (predicate a)
        then worder s a
        else do
            b <- FL.finalExtract fextract fcleanup s
            return $ Partial 0 $ WBRight b
    step (WBRight b) a =
        return
            $ if not (predicate a)
              then Done 1 b
              else Partial 0 $ WBRight b

    extract (WBLeft s) = fextract s
    extract (WBWord s) = fextract s
    extract (WBRight b) = return b

data GroupByState a s
    = GroupByInit !s
    | GroupByGrouping !a !s

-- | See 'Streamly.Internal.Data.Parser.groupBy'.
--
{-# INLINE groupBy #-}
groupBy :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy eq (Fold fstep finitial fextract fcleanup) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a0 a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a0 s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> GroupByInit s
                  FL.Done _ -> error "groupBy: Done in initial not implemented"

    step (GroupByInit s) a = grouper s a a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a0 a
        else Done 1 <$> FL.finalExtract fextract fcleanup s

    extract (GroupByInit s) = fextract s
    extract (GroupByGrouping _ s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.groupByRolling'.
--
{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupByRolling eq (Fold fstep finitial fextract fcleanup) =
    Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> GroupByInit s
                  FL.Done _ -> error "groupByRolling: Done in initial not implemented"

    step (GroupByInit s) a = grouper s a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a
        else Done 1 <$> FL.finalExtract fextract fcleanup s

    extract (GroupByInit s) = fextract s
    extract (GroupByGrouping _ s) = fextract s

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
    step [x] a =
        return
            $ if x `cmp` a
              then Done 0 ()
              else Error "eqBy: failed, yet to match the last element"
    step (x:xs) a =
        return
            $ if x `cmp` a
              then Continue 0 xs
              else Error
                       $ "eqBy: failed, yet to match "
                       ++ show (length xs + 1) ++ " elements"

    extract xs =
        throwM
            $ ParseError
            $ "eqBy: end of input, yet to match "
            ++ show (length xs) ++ " elements"

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.lookahead'.
--
-- /Internal/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Parser m a b -> Parser m a b
lookAhead (Parser step1 initial1 _) = Parser step initial extract

    where

    initial = Tuple' 0 <$> initial1

    step (Tuple' cnt st) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        return
            $ case r of
                  Partial n s -> Continue n (Tuple' (cnt1 - n) s)
                  Continue n s -> Continue n (Tuple' (cnt1 - n) s)
                  Done _ b -> Done cnt1 b
                  Error err -> Error err

    -- XXX returning an error let's us backtrack.  To implement it in a way so
    -- that it terminates on eof without an error then we need a way to
    -- backtrack on eof, that will require extract to return 'Step' type.
    extract (Tuple' n _) =
        throwM
            $ ParseError
            $ "lookAhead: end of input after consuming "
            ++ show n ++ " elements"

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.deintercalate'.
--
-- /Unimplemented/
--
{-# INLINE deintercalate #-}
deintercalate ::
    -- Monad m =>
       Fold m a y -> Parser m x a
    -> Fold m b z -> Parser m x b
    -> Parser m x (y, z)
deintercalate = undefined

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

data ManyTillState fs sr sl
    = ManyTillR Int fs sr
    | ManyTillL Int fs sl

-- | See 'Streamly.Internal.Data.Parser.manyTill'.
--
-- /Internal/
--
{-# INLINE manyTill #-}
manyTill :: MonadCatch m
    => Fold m b c -> Parser m a b -> Parser m a x -> Parser m a c
manyTill (Fold fstep finitial fextract fcleanup)
         (Parser stepL initialL extractL)
         (Parser stepR initialR _) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> ManyTillR 0 fs <$> initialR
            FL.Done _ -> error "manyTill: Done in initial not implemented"

    step (ManyTillR cnt fs st) a = do
        r <- stepR st a
        case r of
            Partial n s -> return $ Partial n (ManyTillR 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillR (cnt + 1 - n) fs s)
            Done n _ -> do
                b <- FL.finalExtract fextract fcleanup fs
                return $ Done n b
            Error _ -> do
                rR <- initialL
                return $ Continue (cnt + 1) (ManyTillL 0 fs rR)
    -- XXX the cnt is being used only by the assert
    step (ManyTillL cnt fs st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ManyTillL 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillL (cnt + 1 - n) fs s)
            Done n b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s -> do
                        l <- initialR
                        return $ Partial n (ManyTillR 0 s l)
                    FL.Done b1 -> return $ Done n b1
            Error err -> return $ Error err

    extract (ManyTillL _ fs sR) = do
        res <- extractL sR >>= fstep fs
        case res of
            FL.Partial s -> fextract s
            FL.Done b -> return b
    extract (ManyTillR _ fs _) = fextract fs
