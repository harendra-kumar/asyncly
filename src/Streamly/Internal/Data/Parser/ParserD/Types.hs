{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD.Types
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming and backtracking parsers.
--
-- Parsers just extend folds.  Please read the 'Fold' design notes in
-- "Streamly.Internal.Data.Fold.Types" for background on the design.
--
-- = Parser Design
--
-- The 'Parser' type or a parsing fold is a generalization of the 'Fold' type.
-- The 'Fold' type /always/ succeeds on each input. Therefore, it does not need
-- to buffer the input. In contrast, a 'Parser' may fail and backtrack to
-- replay the input again to explore another branch of the parser. Therefore,
-- it needs to buffer the input. Therefore, a 'Parser' is a fold with some
-- additional requirements.  To summarize, unlike a 'Fold', a 'Parser':
--
-- 1. may not generate a new value of the accumulator on every input, it may
-- generate a new accumulator only after consuming multiple input elements
-- (e.g. takeEQ).
-- 2. on success may return some unconsumed input (e.g. takeWhile)
-- 3. may fail and return all input without consuming it (e.g. satisfy)
-- 4. backtrack and start inspecting the past input again (e.g. alt)
--
-- These use cases require buffering and replaying of input.  To facilitate
-- this, the step function of the 'Fold' is augmented to return the next state
-- of the fold along with a command tag using a 'Step' functor, the tag tells
-- the fold driver to manipulate the future input as the parser wishes. The
-- 'Step' functor provides the following commands to the fold driver
-- corresponding to the use cases outlined in the previous para:
--
-- 1. 'Skip': buffer the current input and go back to a previous
--    position in the stream
-- 2. 'Yield': buffer the current input and go back to a previous
--    position in the stream, drop the buffer before that position.
-- 3. 'Stop': parser succeeded, returns how much input was leftover
-- 4. 'Error': indicates that the parser has failed without a result
--
-- = How a Parser Works?
--
-- A parser is just like a fold, it keeps consuming inputs from the stream and
-- accumulating them in an accumulator. The accumulator of the parser could be
-- a singleton value or it could be a collection of values e.g. a list.
--
-- The parser may build a new output value from multiple input items. When it
-- consumes an input item but needs more input to build a complete output item
-- it uses @Skip 0 s@, yielding the intermediate state @s@ and asking the
-- driver to provide more input.  When the parser determines that a new output
-- value is complete it can use a @Stop n b@ to terminate the parser with @n@
-- items of input unused and the final value of the accumulator returned as
-- @b@. If at any time the parser determines that the parse has failed it can
-- return @Error err@.
--
-- A parser building a collection of values (e.g. a list) can use the @Yield@
-- constructor whenever a new item in the output collection is generated. If a
-- parser building a collection of values has yielded at least one value then
-- it considered successful and cannot fail after that. In the current
-- implementation, this is not automatically enforced, there is a rule that the
-- parser MUST use only @Stop@ for termination after the first @Yield@, it
-- cannot use @Error@. It may be possible to change the implementation so that
-- this rule is not required, but there may be some performance cost to it.
--
-- 'Streamly.Internal.Data.Parser.takeWhile' and
-- 'Streamly.Internal.Data.Parser.some' combinators are good examples of
-- efficient implementations using all features of this representation.  It is
-- possible to idiomatically build a collection of parsed items using a
-- singleton parser and @Alternative@ instance instead of using a
-- multi-yield parser.  However, this implementation is amenable to stream
-- fusion and can therefore be much faster.
--
-- = Error Handling
--
-- When a parser's @step@ function is invoked it may terminate by either a
-- 'Stop' or an 'Error' return value. In an 'Alternative' composition an error
-- return can make the composed parser backtrack and try another parser.
--
-- If the stream stops before a parser could terminate then we use the
-- @extract@ function of the parser to retrieve the last yielded value of the
-- parser. If the parser has yielded at least one value then @extract@ MUST
-- return a value without throwing an error, otherwise it uses the 'ParseError'
-- exception to throw an error.
--
-- We chose the exception throwing mechanism for @extract@ instead of using an
-- explicit error return via an 'Either' type for keeping the interface simple
-- as most of the time we do not need to catch the error in intermediate
-- layers. Note that we cannot use exception throwing mechanism in @step@
-- function because of performance reasons. 'Error' constructor in that case
-- allows loop fusion and better performance.
--
-- = Future Work
--
-- It may make sense to move "takeWhile" type of parsers, which cannot fail but
-- need some lookahead, to splitting folds.  This will allow such combinators
-- to be accepted where we need an unfailing "Fold" type.
--
-- Based on application requirements it should be possible to design even a
-- richer interface to manipulate the input stream/buffer. For example, we
-- could randomly seek into the stream in the forward or reverse directions or
-- we can even seek to the end or from the end or seek from the beginning.
--
-- We can distribute and scan/parse a stream using both folds and parsers and
-- merge the resulting streams using different merge strategies (e.g.
-- interleaving or serial).

module Streamly.Internal.Data.Parser.ParserD.Types
    (
      Step (..)
    , Parser (..)
    , ParseError (..)

    , yield
    , yieldM
    , splitWith
    , split_

    , die
    , dieM
    , splitSome
    , splitMany
    , alt
    , concatMap
    )
where

import Control.Applicative (Alternative(..))
import Control.Exception (assert, Exception(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Catch (MonadCatch, try, throwM, MonadThrow)
import Prelude hiding (concatMap)

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold (Fold(..), toList)
import Streamly.Internal.Data.Strict (Tuple3'(..))

-- | The return type of a 'Parser' step.
--
-- A parser is driven by a parse driver one step at a time, at any time the
-- driver may @extract@ the result of the parser. The parser may ask the driver
-- to backtrack at any point, therefore, the driver holds the input up to a
-- point of no return in a backtracking buffer.  The buffer grows or shrinks
-- based on the return values of the parser step execution.
--
-- When a parser step is executed it generates a new intermediate state of the
-- parse result along with a command to the driver. The command tells the
-- driver whether to keep the input stream for a potential backtracking later
-- on or drop it, and how much to keep. The constructors of 'Step' represent
-- the commands to the driver.
--
-- /Internal/
--
{-# ANN type Step Fuse #-}
data Step s b =
      Yield Int s
      -- ^ Trim the backtracking buffer keeping only most recent @n@ items.
      -- @Yield n state@ indicates that the parser has yielded a new result
      -- which is a point of no return. The result can be extracted using
      -- @extract@. The driver drops the buffer except @n@ most recent
      -- elements.  The rule is that if a parser has yielded at least once it
      -- cannot return a failure result.

      | YieldB Int s
      -- ^ Go back by @n@ items in the input buffer and drop the rest.
      -- @YieldB n state@ indicates that the parser has yielded a new result
      -- which is a point of no return. The result can be extracted using
      -- @extract@. The driver moves back the current position of the cursor by
      -- @n@ elements and drops any buffer before that.

     -- only rewind
    | Skip Int s
    -- ^ Go back by @n@ items in the input buffer.
    -- @Skip n state@ indicates that the parser has consumed the current input
    -- but no new result has been generated. A new @state@ is generated.
    -- However, if we use @extract@ on @state@ it will generate a result from
    -- the previous @Yield@.  When @n@ is non-zero it is a backward offset from
    -- the current position in the stream from which the driver will replay the
    -- input to the parser. The offset cannot be beyond the latest point of no
    -- return created by @Yield@.

    | Stop Int b
    -- ^ Trim the backtracking buffer keeping only most recent @n@ items.
    -- @Stop n result@ asks the driver to stop driving the parser because it
    -- has reached a fixed point and further input will not change the result.
    -- @n@ is the count of unused input elements which includes the element on
    -- which 'Stop' occurred.

    | Error String
    -- ^ Go back to the start of the backtracking buffer.
    -- An error makes the parser backtrack to the last checkpoint and try
    -- another alternative.

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap _ (Yield n s) = Yield n s
    fmap _ (YieldB n s) = YieldB n s
    fmap _ (Skip n s) = Skip n s
    fmap f (Stop n b) = Stop n (f b)
    fmap _ (Error err) = Error err

-- | A parser is a fold that can fail and is represented as @Parser step
-- initial extract@. Before we drive a parser we call the @initial@ action to
-- retrieve the initial state of the fold. The parser driver invokes @step@
-- with the state returned by the previous step and the next input element. It
-- results into a new state and a command to the driver represented by 'Step'
-- type. The driver keeps invoking the step function until it stops or fails.
-- At any point of time the driver can call @extract@ to inspect the result of
-- the fold. It may result in an error or an output value.
--
-- /Internal/
--
data Parser m a b =
    forall s. Parser (s -> a -> m (Step s b)) (m s) (s -> m b)

-- | This exception is used for two purposes:
--
-- * When a parser ultimately fails, the user of the parser is intimated via
--    this exception.
-- * When the "extract" function of a parser needs to throw an error.
--
-- /Internal/
--
newtype ParseError = ParseError String
    deriving Show

instance Exception ParseError where
    displayException (ParseError err) = err

instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f (Parser step1 initial extract) =
        Parser step initial (fmap2 f extract)

        where

        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

-- | See 'Streamly.Internal.Data.Parser.yield'.
--
-- /Internal/
--
{-# INLINE_NORMAL yield #-}
yield :: Monad m => b -> Parser m a b
yield b = Parser (\_ _ -> pure $ Stop 1 b)  -- step
                 (pure ())                  -- initial
                 (\_ -> pure b)             -- extract

-- | See 'Streamly.Internal.Data.Parser.yieldM'.
--
-- /Internal/
--
{-# INLINE yieldM #-}
yieldM :: Monad m => m b -> Parser m a b
yieldM b = Parser (\_ _ -> Stop 1 <$> b) -- step
                  (pure ())              -- initial
                  (\_ -> b)              -- extract

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

{-# ANN type SeqParseState Fuse #-}
data SeqParseState sl f sr = SeqParseL sl | SeqParseR f sr

-- Note: this implementation of splitWith is fast because of stream fusion but
-- has quadratic time complexity, because each composition adds a new branch
-- that each subsequent parse's input element has to go through, therefore, it
-- cannot scale to a large number of compositions. After around 100
-- compositions the performance starts dipping rapidly beyond a CPS style
-- unfused implementation.
--
-- | See 'Streamly.Internal.Data.Parser.splitWith'.
--
-- /Internal/
--
{-# INLINE splitWith #-}
splitWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
splitWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = SeqParseL <$> initialL

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Stop at some point.
    step (SeqParseL st) a = do
        r <- stepL st a
        case r of
            -- Note: We need to buffer the input for a possible Alternative
            -- e.g. in ((,) <$> p1 <*> p2) <|> p3, if p2 fails we have to
            -- backtrack and start running p3. So we need to keep the input
            -- buffered until we know that the applicative cannot fail.
            Yield _ s -> return $ Skip 0 (SeqParseL s)
            YieldB n s -> return $ Skip n (SeqParseL s)
            Skip n s -> return $ Skip n (SeqParseL s)
            Stop n b -> Skip n <$> (SeqParseR (func b) <$> initialR)
            Error err -> return $ Error err

    step (SeqParseR f st) a = do
        r <- stepR st a
        return $ case r of
            Yield n s -> Yield n (SeqParseR f s)
            YieldB n s -> YieldB n (SeqParseR f s)
            Skip n s -> Skip n (SeqParseR f s)
            Stop n b -> Stop n (f b)
            Error err -> Error err

    extract (SeqParseR f sR) = fmap f (extractR sR)
    extract (SeqParseL sL) = do
        rL <- extractL sL
        sR <- initialR
        rR <- extractR sR
        return $ func rL rR

{-# ANN type SeqAState Fuse #-}
data SeqAState sl sr = SeqAL sl | SeqAR sr

-- This turns out to be slightly faster than splitWith
-- | See 'Streamly.Internal.Data.Parser.split_'.
--
-- /Internal/
--
{-# INLINE split_ #-}
split_ :: Monad m => Parser m x a -> Parser m x b -> Parser m x b
split_ (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = SeqAL <$> initialL

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Stop at some point.
    step (SeqAL st) a = do
        (\r i -> case r of
            -- Note: this leads to buffering even if we are not in an
            -- Alternative composition.
            Yield _ s -> Skip 0 (SeqAL s)
            YieldB n s -> Skip n (SeqAL s)
            Skip n s -> Skip n (SeqAL s)
            Stop n _ -> Skip n (SeqAR i)
            -- XXX should we sequence initialR monadically?
            Error err -> Error err) <$> stepL st a <*> initialR

    step (SeqAR st) a = do
        (\r -> case r of
            Yield n s -> Yield n (SeqAR s)
            YieldB n s -> YieldB n (SeqAR s)
            Skip n s -> Skip n (SeqAR s)
            Stop n b -> Stop n b
            Error err -> Error err) <$> stepR st a

    extract (SeqAR sR) = extractR sR
    extract (SeqAL sL) = do
        _ <- extractL sL
        initialR >>= extractR

-- | 'Applicative' form of 'splitWith'.
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = yield

    {-# INLINE (<*>) #-}
    (<*>) = splitWith id

    {-# INLINE (*>) #-}
    (*>) = split_

-------------------------------------------------------------------------------
-- Sequential Alternative
-------------------------------------------------------------------------------

{-# ANN type AltParseState Fuse #-}
data AltParseState sl sr = AltParseL Int sl | AltParseR sr

-- Note: this implementation of alt is fast because of stream fusion but has
-- quadratic time complexity, because each composition adds a new branch that
-- each subsequent alternative's input element has to go through, therefore, it
-- cannot scale to a large number of compositions
--
-- | See 'Streamly.Internal.Data.Parser.alt'.
--
-- /Internal/
--
{-# INLINE alt #-}
alt :: Monad m => Parser m x a -> Parser m x a -> Parser m x a
alt (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = AltParseL 0 <$> initialL

    -- Once a parser yields at least one value it cannot fail.  This
    -- restriction helps us make backtracking more efficient, as we do not need
    -- to keep the consumed items buffered after a yield. Note that we do not
    -- enforce this and if a misbehaving parser does not honor this then we can
    -- get unexpected results.
    step (AltParseL cnt st) a = do
        r <- stepL st a
        case r of
            Yield n s -> return $ Yield n (AltParseL 0 s)
            YieldB n s -> return $ YieldB n (AltParseL 0 s)
            Skip n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Skip n (AltParseL (cnt + 1 - n) s)
            Stop n b -> return $ Stop n b
            Error _ -> do
                rR <- initialR
                return $ Skip (cnt + 1) (AltParseR rR)

    step (AltParseR st) a = do
        r <- stepR st a
        return $ case r of
            Yield n s -> Yield n (AltParseR s)
            YieldB n s -> YieldB n (AltParseR s)
            Skip n s -> Skip n (AltParseR s)
            Stop n b -> Stop n b
            Error err -> Error err

    extract (AltParseR sR) = extractR sR
    extract (AltParseL _ sL) = extractL sL

-- | See documentation of 'Streamly.Internal.Data.Parser.many'.
--
-- /Internal/
--
{-# INLINE splitMany #-}
splitMany :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
splitMany (Fold fstep finitial fextract) (Parser step1 initial1 extract1) =
    Parser step initial extract

    where

    initial = do
        ps <- initial1 -- parse state
        fs <- finitial -- fold state
        pure (Tuple3' ps 0 fs)

    {-# INLINE step #-}
    step (Tuple3' st cnt fs) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Yield _ s -> return $ Skip 0 (Tuple3' s cnt1 fs)
            YieldB n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Skip n (Tuple3' s (cnt1 - n) fs)
            Skip n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Skip n (Tuple3' s (cnt1 - n) fs)
            Stop n b -> do
                s <- initial1
                fs1 <- fstep fs b
                return $ YieldB n (Tuple3' s 0 fs1)
            Error _ -> do
                xs <- fextract fs
                return $ Stop cnt1 xs

    -- XXX The "try" may impact performance if this parser is used as a scan
    extract (Tuple3' s _ fs) = do
        r <- try $ extract1 s
        case r of
            Left (_ :: ParseError) -> fextract fs
            Right b -> fstep fs b >>= fextract

-- | See documentation of 'Streamly.Internal.Data.Parser.some'.
--
-- /Internal/
--
{-# INLINE splitSome #-}
splitSome :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
splitSome (Fold fstep finitial fextract) (Parser step1 initial1 extract1) =
    Parser step initial extract

    where

    initial = do
        ps <- initial1 -- parse state
        fs <- finitial -- fold state
        pure (Tuple3' ps 0 (Left fs))

    {-# INLINE step #-}
    step (Tuple3' st _ (Left fs)) a = do
        r <- step1 st a
        case r of
            Yield _ s -> return $ Skip 0 (Tuple3' s undefined (Left fs))
            YieldB n s -> return $ Skip n (Tuple3' s undefined (Left fs))
            Skip  n s -> return $ Skip n (Tuple3' s undefined (Left fs))
            Stop n b -> do
                s <- initial1
                fs1 <- fstep fs b
                return $ YieldB n (Tuple3' s 0 (Right fs1))
            Error err -> return $ Error err
    step (Tuple3' st cnt (Right fs)) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        case r of
            Yield _ s -> return $ Yield 0 (Tuple3' s cnt1 (Right fs))
            YieldB n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ YieldB n (Tuple3' s (cnt1 - n) (Right fs))
            Skip n s -> do
                assert (cnt1 - n >= 0) (return ())
                return $ Skip n (Tuple3' s (cnt1 - n) (Right fs))
            Stop n b -> do
                s <- initial1
                fs1 <- fstep fs b
                return $ YieldB n (Tuple3' s 0 (Right fs1))
            Error _ -> Stop cnt1 <$> fextract fs

    -- XXX The "try" may impact performance if this parser is used as a scan
    extract (Tuple3' s _ (Left fs)) = extract1 s >>= fstep fs >>= fextract
    extract (Tuple3' s _ (Right fs)) = do
        r <- try $ extract1 s
        case r of
            Left (_ :: ParseError) -> fextract fs
            Right b -> fstep fs b >>= fextract

-- | See 'Streamly.Internal.Data.Parser.die'.
--
-- /Internal/
--
{-# INLINE_NORMAL die #-}
die :: MonadThrow m => String -> Parser m a b
die err =
    Parser (\_ _ -> pure $ Error err)      -- step
           (pure ())                       -- initial
           (\_ -> throwM $ ParseError err) -- extract

-- | See 'Streamly.Internal.Data.Parser.dieM'.
--
-- /Internal/
--
{-# INLINE dieM #-}
dieM :: MonadThrow m => m String -> Parser m a b
dieM err =
    Parser (\_ _ -> Error <$> err)         -- step
           (pure ())                       -- initial
           (\_ -> err >>= throwM . ParseError) -- extract

-- Note: The default implementations of "some" and "many" loop infinitely
-- because of the strict pattern match on both the arguments in applicative and
-- alternative. With the direct style parser type we cannot use the mutually
-- recursive definitions of "some" and "many".
--
-- Note: With the direct style parser type, the list in "some" and "many" is
-- accumulated strictly, it cannot be consumed lazily.

-- | 'Alternative' instance using 'alt'.
--
-- Note: The implementation of '<|>' is not lazy in the second
-- argument. The following code will fail:
--
-- >>> S.parse (PR.satisfy (> 0) <|> undefined) $ S.fromList [1..10]
--
instance MonadCatch m => Alternative (Parser m a) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    (<|>) = alt

    {-# INLINE many #-}
    many = splitMany toList

    {-# INLINE some #-}
    some = splitSome toList

{-# ANN type ConcatParseState Fuse #-}
data ConcatParseState sl p = ConcatParseL sl | ConcatParseR p

-- | See 'Streamly.Internal.Data.Parser.concatMap'.
--
-- /Internal/
--
{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Parser m a c) -> Parser m a b -> Parser m a c
concatMap func (Parser stepL initialL extractL) = Parser step initial extract

    where

    initial = ConcatParseL <$> initialL

    step (ConcatParseL st) a = do
        r <- stepL st a
        return $ case r of
            Yield _ s -> Skip 0 (ConcatParseL s)
            YieldB n s -> Skip n (ConcatParseL s)
            Skip n s -> Skip n (ConcatParseL s)
            Stop n b -> Skip n (ConcatParseR (func b))
            Error err -> Error err

    step (ConcatParseR (Parser stepR initialR extractR)) a = do
        st <- initialR
        r <- stepR st a
        return $ case r of
            Yield n s ->
                Yield n (ConcatParseR (Parser stepR (return s) extractR))
            YieldB n s ->
                YieldB n (ConcatParseR (Parser stepR (return s) extractR))
            Skip n s ->
                Skip n (ConcatParseR (Parser stepR (return s) extractR))
            Stop n b -> Stop n b
            Error err -> Error err

    extract (ConcatParseR (Parser _ initialR extractR)) =
        initialR >>= extractR

    extract (ConcatParseL sL) = extractL sL >>= f . func

        where

        f (Parser _ initialR extractR) = initialR >>= extractR

-- Note: The monad instance has quadratic performance complexity. It works fine
-- for small number of compositions but for a scalable implementation we need a
-- CPS version.

-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Types.Parser'.
--
instance Monad m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

-- | See documentation of 'Streamly.Internal.Data.Parser.ParserK.Types.Parser'.
--
instance MonadCatch m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = alt
