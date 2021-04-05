-- |
-- Module      : Streamly.Internal.Data.Fold.Type
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- = Stream Consumers
--
-- We can classify stream consumers in the following categories in order of
-- increasing complexity and power:
--
-- == Accumulators
--
-- These are the simplest folds that never fail and never terminate, they
-- accumulate the input values forever and can always accept new inputs (never
-- terminate) and always have a valid result value.  A
-- 'Streamly.Internal.Data.Fold.sum' operation is an example of an accumulator.
-- Traditional Haskell left folds like 'foldl' are accumulators.
--
-- We can distribute an input stream to two or more accumulators using a @tee@
-- style composition.  Accumulators cannot be applied on a stream one after the
-- other, which we call a @serial@ append style composition of folds. This is
-- because accumulators never terminate, since the first accumulator in a
-- series will never terminate, the next one will never get to run.
--
-- == Terminating Folds
--
-- Terminating folds are accumulators that can terminate. Once a fold
-- terminates it no longer accepts any more inputs.  Terminating folds can be
-- used in a @serial@ append style composition where one fold can be applied
-- after the other on an input stream. We can apply a terminating fold
-- repeatedly on an input stream, splitting the stream and consuming it in
-- fragments.  Terminating folds never fail, therefore, they do not need
-- backtracking.
--
-- The 'Streamly.Internal.Data.Fold.take' operation is an example of a
-- terminating fold  It terminates after consuming @n@ items. Coupled with an
-- accumulator (e.g. sum) it can be used to split and process the stream into
-- chunks of fixed size.
--
-- == Terminating Folds with Leftovers
--
-- The next upgrade after terminating folds is terminating folds with leftover
-- inputs.  Consider the example of @takeWhile@ operation, it needs to inspect
-- an element for termination decision. However, it does not consume the
-- element on which it terminates. To implement @takeWhile@ a terminating fold
-- will have to implement a way to return unconsumed input to the fold driver.
--
-- Single element leftover case is the most common and its easy to implement it
-- in terminating folds using a @Done1@ constructor in the 'Step' type which
-- indicates that the last element was not consumed by the fold. The following
-- additional operations can be implemented as terminating folds if we do that.
--
-- @
-- takeWhile
-- groupBy
-- wordBy
-- @
--
-- However, it creates several complications.  The 'many' combinator  requires
-- a @Partial1@ ('Partial' with leftover) to handle a @Done1@ from the top
-- level fold, for efficient implementation.  If the collecting fold in "many"
-- returns a @Partial1@ or @Done1@ then what to do with all the elements that
-- have been consumed?
--
-- Similarly, in distribute, if one fold consumes a value and others say its a
-- leftover then what do we do?  Folds like "many" require the leftover to be
-- fed to it again. So in a distribute operation those folds which gave a
-- leftover will have to be fed the leftover while the folds that consumed will
-- have to be fed the next input.  This is very complicated to implement. We
-- have the same issue in backtracking parsers being used in a distribute
-- operation.
--
-- To avoid these issues we want to enforce by typing that the collecting folds
-- can never return a leftover. So we need a fold type without @Done1@ or
-- @Partial1@. This leads us to design folds to never return a leftover and the
-- use cases of single leftover are transferred to parsers where we have
-- general backtracking mechanism and single leftover is just a special case of
-- backtracking.
--
-- This means: takeWhile, groupBy, wordBy would be implemented as parsers.
-- "take 0" can implemented as a fold if we make initial return @Step@ type.
-- "takeInterval" can be implemented without @Done1@.
--
-- == Parsers
--
-- The next upgrade after terminating folds with a leftover are parsers.
-- Parsers are terminating folds that can fail and backtrack. Parsers can be
-- composed using an @alternative@ style composition where they can backtrack
-- and apply another parser if one parser fails.
-- 'Streamly.Internal.Data.Parser.satisfy' is a simple example of a parser, it
-- would succeed if the condition is satisfied and it would fail otherwise, on
-- failure an alternative parser can be used on the same input.
--
-- = Types for Stream Consumers
--
-- In streamly, there is no separate type for accumulators. Terminating folds
-- are a superset of accumulators and to avoid too many types we represent both
-- using the same type, 'Fold'.
--
-- We do not club the leftovers functionality with terminating folds because of
-- the reasons explained earlier. Instead combinators that require leftovers
-- are implemented as the 'Streamly.Internal.Data.Parser.Parser' type.  This is
-- a sweet spot to balance ease of use, type safety and performance.  Using
-- separate Accumulator and terminating fold types would encode more
-- information in types but it would make ease of use, implementation,
-- maintenance effort worse. Combining Accumulator, terminating folds and
-- Parser into a single 'Streamly.Internal.Data.Parser.Parser' type would make
-- ease of use even better but type safety and performance worse.
--
-- One of the design requirements that we have placed for better ease of use
-- and code reuse is that 'Streamly.Internal.Data.Parser.Parser' type should be
-- a strict superset of the 'Fold' type i.e. it can do everything that a 'Fold'
-- can do and more. Therefore, folds can be easily upgraded to parsers and we
-- can use parser combinators on folds as well when needed.
--
-- = Fold Design
--
-- A fold is represented by a collection of "initial", "step" and "extract"
-- functions. The "initial" action generates the initial state of the fold. The
-- state is internal to the fold and maintains the accumulated output. The
-- "step" function is invoked using the current state and the next input value
-- and results in a @Partial@ or @Done@. A @Partial@ returns the next intermediate
-- state of the fold, a @Done@ indicates that the fold has terminated and
-- returns the final value of the accumulator.
--
-- Every @Partial@ indicates that a new accumulated output is available.  The
-- accumulated output can be extracted from the state at any point using
-- "extract". "extract" can never fail. A fold returns a valid output even
-- without any input i.e. even if you call "extract" on "initial" state it
-- provides an output. This is not true for parsers.
--
-- In general, "extract" is used in two cases:
--
-- * When the fold is used as a scan @extract@ is called on the intermediate
-- state every time it is yielded by the fold, the resulting value is yielded
-- as a stream.
-- * When the fold is used as a regular fold, @extract@ is called once when
-- we are done feeding input to the fold.
--
-- = Alternate Designs
--
-- An alternate and simpler design would be to return the intermediate output
-- via @Partial@ along with the state, instead of using "extract" on the yielded
-- state and remove the extract function altogether.
--
-- This may even facilitate more efficient implementation.  Extract from the
-- intermediate state after each yield may be more costly compared to the fold
-- step itself yielding the output. The fold may have more efficient ways to
-- retrieve the output rather than stuffing it in the state and using extract
-- on the state.
--
-- However, removing extract altogether may lead to less optimal code in some
-- cases because the driver of the fold needs to thread around the intermediate
-- output to return it if the stream stops before the fold could @Done@.  When
-- using this approach, the @parseMany (FL.take filesize)@ benchmark shows a
-- 2x worse performance even after ensuring everything fuses.  So we keep the
-- "extract" approach to ensure better perf in all cases.
--
-- But we could still yield both state and the output in @Partial@, the output
-- can be used for the scan use case, instead of using extract. Extract would
-- then be used only for the case when the stream stops before the fold
-- completes.
--
-- = Accumulators and Terminating Folds
--
-- Folds in this module can be classified in two categories viz. accumulators
-- and terminating folds. Accumulators do not have a terminating condition,
-- they run forever and consume the entire stream, for example the 'length'
-- fold. Terminating folds have a terminating condition and can terminate
-- without consuming the entire stream, for example, the 'head' fold.
--
-- = Monoids
--
-- Monoids allow generalized, modular folding.  The accumulators in this module
-- can be expressed using 'mconcat' and a suitable 'Monoid'.  Instead of
-- writing folds we can write Monoids and turn them into folds.
--
-- = Performance Notes
--
-- 'Streamly.Prelude' module provides fold functions to directly fold streams
-- e.g.  Streamly.Prelude/'Streamly.Prelude.sum' serves the same purpose as
-- Fold/'sum'.  However, the functions in Streamly.Prelude cannot be
-- efficiently combined together e.g. we cannot drive the input stream through
-- @sum@ and @length@ fold functions simultaneously.  Using the 'Fold' type we
-- can efficiently split the stream across multiple folds because it allows the
-- compiler to perform stream fusion optimizations.
--
module Streamly.Internal.Data.Fold.Type
    (
    -- * Types
      Step (..)
    , Fold (..)

    -- * Constructors
    , mkFoldl
    , mkFoldlM
    , mkFoldl1
    , mkFoldr
    , mkFoldrM
    , mkFold
    , mkFold_
    , mkFoldM
    , mkFoldM_

    -- * Folds
    , yield
    , yieldM
    , drain
    , toList

    -- * Combinators

    -- ** Mapping output
    , rmapM

    -- ** Mapping Input
    , map
    , lmap
    , lmapM

    -- ** Filtering
    , filter
    , filterM
    , catMaybes

    -- ** Trimming
    , take
    , takeInterval

    -- ** Serial Append
    , serialWith
    , serial_

    -- ** Parallel Distribution
    , GenericRunner(..)
    , teeWith
    , teeWithFst
    , teeWithMin

    -- ** Parallel Alternative
    , shortest
    , longest

    -- ** Splitting
    , ManyState
    , many
    , manyPost
    , chunksOf
    , intervalsOf

    -- ** Nesting
    , concatMap

    -- * Running Partially
    , duplicate
    , initialize
    , runStep

    -- * Fold2
    , Fold2 (..)
    , simplify
    , chunksOf2
    )
where

import Control.Monad (void, (>=>))
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar, readMVar)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (control)
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (isJust, fromJust)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.SVar (MonadAsync)

import Prelude hiding (concatMap, filter, map, take)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (concatMap, filter, map)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold

------------------------------------------------------------------------------
-- Step of a fold
------------------------------------------------------------------------------

-- The Step functor around b allows expressing early termination like a right
-- fold. Traditional list right folds use function composition and laziness to
-- terminate early whereas we use data constructors. It allows stream fusion in
-- contrast to the foldr/build fusion when composing with functions.

-- | Represents the result of the @step@ of a 'Fold'.  'Partial' returns an
-- intermediate state of the fold, the fold step can be called again with the
-- state or the driver can use @extract@ on the state to get the result out.
-- 'Done' returns the final result and the fold cannot be driven further.
--
-- /Pre-release/
--
{-# ANN type Step Fuse #-}
data Step s b
    = Partial !s
    | Done !b

-- | 'first' maps over 'Partial' and 'second' maps over 'Done'.
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
    second f (Done a) = Done (f a)

-- | 'fmap' maps over 'Done'.
--
-- @
-- fmap = 'second'
-- @
--
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial s -> pure $ Partial s
        Done b -> Done <$> f b

------------------------------------------------------------------------------
-- The Fold type
------------------------------------------------------------------------------

-- | The type @Fold m a b@ having constructor @Fold step initial extract@
-- represents a fold over an input stream of values of type @a@ to a final
-- value of type @b@ in 'Monad' @m@.
--
-- The fold uses an intermediate state @s@ as accumulator, the type @s@ is
-- internal to the specific fold definition. The initial value of the fold
-- state @s@ is returned by @initial@. The @step@ function consumes an input
-- and either returns the final result @b@ if the fold is done or the next
-- intermediate state (see 'Step'). At any point the fold driver can extract
-- the result from the intermediate state using the @extract@ function.
--
-- NOTE: The constructor is not yet exposed via exposed modules, smart
-- constructors are provided to create folds.  If you think you need the
-- constructor of this type please consider using the smart constructors in
-- "Streamly.Internal.Data.Fold' instead.
--
-- /since 0.8.0 (type changed)/
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b)

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a fold.
--
-- @since 0.8.0
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
rmapM f (Fold step initial extract) = Fold step1 initial1 (extract >=> f)

    where

    initial1 = initial >>= mapMStep f
    step1 s a = step s a >>= mapMStep f

------------------------------------------------------------------------------
-- Left fold constructors
------------------------------------------------------------------------------

-- | Make a fold from a left fold style pure step function and initial value of
-- the accumulator.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @mkFoldl*@ constructors.
--
-- A fold with an extract function can be expressed using fmap:
--
-- @
-- mkfoldlx :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
-- mkfoldlx step initial extract = fmap extract (mkFoldl step initial)
-- @
--
-- See also: "Streamly.Prelude.foldl'"
--
-- @since 0.8.0
--
{-# INLINE mkFoldl #-}
mkFoldl :: Monad m => (b -> a -> b) -> b -> Fold m a b
mkFoldl step initial =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        return

-- | Make a fold from a left fold style monadic step function and initial value
-- of the accumulator.
--
-- A fold with an extract function can be expressed using rmapM:
--
-- @
-- mkFoldlxM :: Functor m => (s -> a -> m s) -> m s -> (s -> m b) -> Fold m a b
-- mkFoldlxM step initial extract = rmapM extract (mkFoldlM step initial)
-- @
--
-- See also: "Streamly.Prelude.foldlM'"
--
-- @since 0.8.0
--
{-# INLINE mkFoldlM #-}
mkFoldlM :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
mkFoldlM step initial =
    Fold (\s a -> Partial <$> step s a) (Partial <$> initial) return

-- | Make a strict left fold, for non-empty streams, using first element as the
-- starting value. Returns Nothing if the stream is empty.
--
-- See also: "Streamly.Prelude.foldl1'"
--
-- /Pre-release/
{-# INLINE mkFoldl1 #-}
mkFoldl1 :: Monad m => (a -> a -> a) -> Fold m a (Maybe a)
mkFoldl1 step = fmap toMaybe $ mkFoldl step1 Nothing'

    where

    step1 Nothing' a = Just' a
    step1 (Just' x) a = Just' $ step x a

------------------------------------------------------------------------------
-- Right fold constructors
------------------------------------------------------------------------------

-- | Make a fold using a right fold style step function and a terminal value.
-- It performs a strict right fold via a left fold using function composition.
-- Note that this is strict fold, it can only be useful for constructing strict
-- structures in memory. For reductions this will be very inefficient.
--
-- For example,
--
-- > toList = mkFoldr (:) []
--
-- See also: "Streamly.Prelude.foldr"
--
-- @since 0.8.0
{-# INLINE mkFoldr #-}
mkFoldr :: Monad m => (a -> b -> b) -> b -> Fold m a b
mkFoldr g z = fmap ($ z) $ mkFoldl (\f x -> f . g x) id

-- XXX we have not seen any use of this yet, not releasing until we have a use
-- case.
--
-- | Like 'mkFoldr' but with a monadic step function.
--
-- For example,
--
-- > toList = mkFoldrM (\a xs -> return $ a : xs) (return [])
--
-- See also: "Streamly.Prelude.foldrM"
--
-- /Pre-release/
{-# INLINE mkFoldrM #-}
mkFoldrM :: Monad m => (a -> b -> m b) -> m b -> Fold m a b
mkFoldrM g z =
    rmapM (z >>=) $ mkFoldlM (\f x -> return $ g x >=> f) (return return)

------------------------------------------------------------------------------
-- General fold constructors
------------------------------------------------------------------------------

-- XXX If the Step yield gives the result each time along with the state then
-- we can make the type of this as
--
-- mkFold :: Monad m => (s -> a -> Step s b) -> Step s b -> Fold m a b
--
-- Then similar to mkFoldl and mkFoldr we can just fmap extract on it to extend
-- it to the version where an 'extract' function is required. Or do we even
-- need that?
--
-- Until we investigate this we are not releasing these.

-- | Make a terminating fold using a pure step function, a pure initial state
-- and a pure state extraction function.
--
-- /Pre-release/
--
{-# INLINE mkFold #-}
mkFold :: Monad m => (s -> a -> Step s b) -> Step s b -> (s -> b) -> Fold m a b
mkFold step initial extract =
    Fold (\s a -> return $ step s a) (return initial) (return . extract)

-- | Similar to 'mkFold' but the final state extracted is identical to the
-- intermediate state.
--
-- @
-- mkFold_ step initial = mkFold step initial id
-- @
--
-- /Pre-release/
--
{-# INLINE mkFold_ #-}
mkFold_ :: Monad m => (b -> a -> Step b b) -> Step b b -> Fold m a b
mkFold_ step initial = mkFold step initial id

-- | Make a terminating fold with an effectful step function and initial state,
-- and a state extraction function.
--
-- > mkFoldM = Fold
--
--  We can just use 'Fold' but it is provided for completeness.
--
-- /Pre-release/
--
{-# INLINE mkFoldM #-}
mkFoldM :: (s -> a -> m (Step s b)) -> m (Step s b) -> (s -> m b) -> Fold m a b
mkFoldM = Fold

-- | Similar to 'mkFoldM' but the final state extracted is identical to the
-- intermediate state.
--
-- @
-- mkFoldM_ step initial = mkFoldM step initial return
-- @
--
-- /Pre-release/
--
{-# INLINE mkFoldM_ #-}
mkFoldM_ :: Monad m => (b -> a -> m (Step b b)) -> m (Step b b) -> Fold m a b
mkFoldM_ step initial = mkFoldM step initial return

------------------------------------------------------------------------------
-- Fold2
------------------------------------------------------------------------------

-- | Experimental type to provide a side input to the fold for generating the
-- initial state. For example, if we have to fold chunks of a stream and write
-- each chunk to a different file, then we can generate the file name using a
-- monadic action. This is a generalized version of 'Fold'.
--
-- /Internal/
data Fold2 m c a b =
  -- | @Fold @ @ step @ @ inject @ @ extract@
  forall s. Fold2 (s -> a -> m s) (c -> m s) (s -> m b)

-- | Convert more general type 'Fold2' into a simpler type 'Fold'
--
-- /Internal/
simplify :: Functor m => Fold2 m c a b -> c -> Fold m a b
simplify (Fold2 step inject extract) c =
    Fold (\x a -> Partial <$> step x a) (Partial <$> inject c) extract

------------------------------------------------------------------------------
-- Basic Folds
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- > drain = drainBy (const (return ()))
--
-- @since 0.7.0
{-# INLINABLE drain #-}
drain :: Monad m => Fold m a ()
drain = mkFoldl (\_ _ -> ()) ()

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Data.Array.Foreign"
-- instead.
--
-- > toList = mkFoldr (:) []
--
-- @since 0.7.0
{-# INLINABLE toList #-}
toList :: Monad m => Fold m a [a]
toList = mkFoldr (:) []

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step1 initial1 extract) = Fold step initial (fmap2 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

-- This is the dual of stream "yield".
--
-- | A fold that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE yield #-}
yield :: Applicative m => b -> Fold m a b
yield b = Fold undefined (pure $ Done b) pure

-- This is the dual of stream "yieldM".
--
-- | A fold that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE yieldM #-}
yieldM :: Applicative m => m b -> Fold m a b
yieldM b = Fold undefined (Done <$> b) pure

{-# ANN type Step Fuse #-}
data SeqFoldState sl f sr = SeqFoldL !sl | SeqFoldR !f !sr

-- | Sequential fold application. Apply two folds sequentially to an input
-- stream.  The input is provided to the first fold, when it is done - the
-- remaining input is provided to the second fold. When the second fold is done
-- or if the input stream is over, the outputs of the two folds are combined
-- using the supplied function.
--
-- >>> f = Fold.serialWith (,) (Fold.take 8 Fold.toList) (Fold.takeEndBy (== '\n') Fold.toList)
-- >>> Stream.fold f $ Stream.fromList "header: hello\n"
-- ("header: ","hello\n")
--
-- Note: This is dual to appending streams using 'Streamly.Prelude.serial'.
--
-- Note: this implementation allows for stream fusion but has quadratic time
-- complexity, because each composition adds a new branch that each subsequent
-- fold's input element has to traverse, therefore, it cannot scale to a large
-- number of compositions. After around 100 compositions the performance starts
-- dipping rapidly compared to a CPS style implementation.
--
-- /Time: O(n^2) where n is the number of compositions./
--
-- @since 0.8.0
--
{-# INLINE serialWith #-}
serialWith :: Monad m => (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
serialWith func (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            Partial sl -> return $ Partial $ SeqFoldL sl
            Done bl -> do
                resR <- initialR
                return $ bimap (SeqFoldR (func bl)) (func bl) resR

    step (SeqFoldL st) a = do
        r <- stepL st a
        case r of
            Partial s -> return $ Partial (SeqFoldL s)
            Done b -> do
                res <- initialR
                return $ bimap (SeqFoldR (func b)) (func b) res
    step (SeqFoldR f st) a = do
        r <- stepR st a
        return
            $ case r of
                  Partial s -> Partial (SeqFoldR f s)
                  Done b -> Done (f b)

    extract (SeqFoldR f sR) = fmap f (extractR sR)
    extract (SeqFoldL sL) = do
        rL <- extractL sL
        res <- initialR
        case res of
            Partial sR -> do
                rR <- extractR sR
                return $ func rL rR
            Done rR -> return $ func rL rR

-- | Same as applicative '*>'. Run two folds serially one after the other
-- discarding the result of the first.
--

{-# ANN type SerialRunner Fuse #-}
data SerialRunner sL sR
    = SerialLeft !sL 
    | SerialRight !sR

{-# INLINE serial_ #-}
serial_ :: Monad m =>
    Fold m x a -> Fold m x b -> Fold m x b
serial_ (Fold stepL beginL _) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return
            $ case resL of
                  Partial sl -> Partial $ SerialLeft sl                         
                  Done _ -> first SerialRight resR

    step (SerialLeft sL) a = do
        resL <- stepL sL a
        case resL of
            Partial sL1 -> return $ Partial $ SerialLeft sL1                
            Done _ -> do
                resR <- beginR
                case resR of
                    Partial sr -> return $ Partial $ SerialRight sr  
                    Done bR -> return $ Done bR    

    step (SerialRight sR) a = do
        resR <- stepR sR a
        case resR of
                  Partial sR1 -> return $ Partial $ SerialRight sR1
                  Done bR -> return $ Done bR

    done (SerialLeft _) = error "Done from left fold is invalid."
       
    done (SerialRight s) = doneR s     


{-# ANN type GenericRunner Fuse #-}
data GenericRunner sL sR bL bR
    = RunBoth !sL !sR
    | RunLeft !sL !bR
    | RunRight !bL !sR

-- | @teeWith k f1 f2@ distributes its input to both @f1@ and @f2@ until both
-- of them terminate and combines their output using @k@.
--
-- >>> avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> Stream.fold avg $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- > teeWith k f1 f2 = fmap (uncurry k) ((Fold.tee f1 f2)
--
-- For applicative composition using this combinator see
-- "Streamly.Internal.Data.Fold.Tee".
--
-- See also: "Streamly.Internal.Data.Fold.Tee"
--
-- @since 0.8.0
--
{-# INLINE teeWith #-}
teeWith :: Monad m => (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
teeWith f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return
            $ case resL of
                  Partial sl ->
                      Partial
                          $ case resR of
                                Partial sr -> RunBoth sl sr
                                Done br -> RunLeft sl br
                  Done bl -> bimap (RunRight bl) (f bl) resR

    step (RunBoth sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        case resL of
            Partial sL1 ->
                return
                    $ Partial
                    $ case resR of
                          Partial sR1 -> RunBoth sL1 sR1
                          Done bR -> RunLeft sL1 bR
            Done bL ->
                return
                    $ case resR of
                          Partial sR1 -> Partial $ RunRight bL sR1
                          Done bR -> Done $ f bL bR
    step (RunLeft sL bR) a = do
        resL <- stepL sL a
        return
            $ case resL of
                  Partial sL1 -> Partial $ RunLeft sL1 bR
                  Done bL -> Done $ f bL bR
    step (RunRight bL sR) a = do
        resR <- stepR sR a
        return
            $ case resR of
                  Partial sR1 -> Partial $ RunRight bL sR1
                  Done bR -> Done $ f bL bR

    done (RunBoth sL sR) = do
        bL <- doneL sL
        bR <- doneR sR
        return $ f bL bR
    done (RunLeft sL bR) = do
        bL <- doneL sL
        return $ f bL bR
    done (RunRight bL sR) = do
        bR <- doneR sR
        return $ f bL bR

-- | Like 'teeWith' but terminates as soon as the first fold terminates.
--
-- /Unimplemented/
--
{-# INLINE teeWithFst #-}
teeWithFst :: Monad m => (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithFst f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR

        case resL of
            Partial sl ->
                return $ Partial $
                    case resR of
                        Partial sr -> RunBoth sl sr
                        Done br -> RunLeft sl br
            Done bl -> do
                case resR of
                    Partial sr -> do
                        br <- doneR sr
                        return $ Done $ f bl br
                    Done br -> return $ Done $ f bl br

    step (RunBoth sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        case resL of
            Partial sL1 ->
                return
                    $ Partial
                    $ case resR of
                          Partial sR1 -> RunBoth sL1 sR1
                          Done bR -> RunLeft sL1 bR
            Done bL ->
                     case resR of
                          Partial sR1 -> do
                                br <- doneR sR1
                                return $ Done $ f bL br
                          Done bR -> return $ Done $ f bL bR
    step (RunLeft sL bR) a = do
        resL <- stepL sL a
        return
            $ case resL of
                  Partial sL1 -> Partial $ RunLeft sL1 bR
                  Done bL -> Done $ f bL bR
    step (RunRight bL sR) a = do
        resR <- stepR sR a
        return
            $ case resR of
                  Partial sR1 -> Partial $ RunRight bL sR1
                  Done bR -> Done $ f bL bR

    done (RunBoth sL sR) = do
        bL <- doneL sL
        bR <- doneR sR
        return $ f bL bR
    done (RunLeft sL bR) = do
        bL <- doneL sL
        return $ f bL bR
    done (RunRight bL sR) = do
        bR <- doneR sR
        return $ f bL bR

-- | Like 'teeWith' but terminates as soon as any one of the two folds
-- terminates.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin :: Monad m => (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithMin f (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR
        case resL of
            Partial sL1 -> do
                case resR of
                    Partial sR1 -> return $ Partial $ RunBoth sL1 sR1   
                    Done br -> do
                        bl <- doneL sL1
                        return $ Done $ f bl br
                    
            Done bl -> do
                case resR of
                    Partial sr -> do
                        br <- doneR sr
                        return $ Done $ f bl br
                    Done br -> return $ Done $ f bl br

    step (RunBoth sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        case resL of
            Partial sL1 ->
                case resR of
                    Partial sR1 -> return $ Partial $ RunBoth sL1 sR1
                    Done bR -> do
                        bL <- doneL sL1
                        return $ Done $ f bL bR
            Done bL ->
                case resR of
                    Partial sR1 -> do
                        br <- doneR sR1
                        return $ Done $ f bL br
                    Done bR -> return $ Done $ f bL bR
    step (RunLeft sL bR) a = do
        resL <- stepL sL a
        return
            $ case resL of
                Partial sL1 -> Partial $ RunLeft sL1 bR
                Done bL -> Done $ f bL bR
    step (RunRight bL sR) a = do
        resR <- stepR sR a
        return
            $ case resR of
                Partial sR1 -> Partial $ RunRight bL sR1
                Done bR -> Done $ f bL bR

    done (RunBoth sL sR) = do
        bL <- doneL sL
        bR <- doneR sR
        return $ f bL bR
    done (RunLeft sL bR) = do
        bL <- doneL sL
        return $ f bL bR
    done (RunRight bL sR) = do
        bR <- doneR sR
        return $ f bL bR

-- | Shortest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- fold.
--
-- /Unimplemented/
--
{-# INLINE shortest #-}
shortest ::  Monad m =>
    Fold m x a -> Fold m x a -> Fold m x a
shortest (Fold stepL beginL doneL) (Fold stepR beginR _) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return $
            case resL of
                Partial sl ->
                    case resR of
                        Partial sr -> Partial $ RunBoth sl sr
                        Done br -> Done br
                Done bl -> Done bl

    step (RunBoth sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        return $
            case resL of
                Partial sL1 ->
                    case resR of
                        Partial sR1 -> Partial $ RunBoth sL1 sR1
                        Done bR -> Done bR
                Done bL -> Done bL

    step (RunLeft _ _) _ = error "Invalid RunLeft option"

    step (RunRight _ _) _ = error "Invalid RunRight option"

    done (RunBoth sL _) = doneL sL -- which one to pick left or right?

    done (RunLeft _ bR) = return bR

    done (RunRight bL _) = return bL

-- | Longest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- fold.
--
-- /Unimplemented/
--
{-# INLINE longest #-}
longest :: Monad m =>
    Fold m x a -> Fold m x a -> Fold m x a
longest (Fold stepL beginL doneL) (Fold stepR beginR doneR) =
    Fold step begin done

    where

    begin = do
        resL <- beginL
        resR <- beginR
        return
            $ case resL of
                    Partial sl ->
                        Partial
                            $ case resR of
                                Partial sr -> RunBoth sl sr
                                Done br -> RunLeft sl br
                    Done bl -> first (RunRight bl) resR

    step (RunBoth sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        case resL of
            Partial sL1 ->
                return
                    $ Partial
                    $ case resR of
                        Partial sR1 -> RunBoth sL1 sR1
                        Done bR -> RunLeft sL1 bR
            Done bL ->
                return
                    $ case resR of
                        Partial sR1 -> Partial $ RunRight bL sR1
                        Done bR -> Done bR
    step (RunLeft sL bR) a = do
        resL <- stepL sL a
        return
            $ case resL of
                Partial sL1 -> Partial $ RunLeft sL1 bR
                Done bL -> Done bL
    step (RunRight bL sR) a = do
        resR <- stepR sR a
        return
            $ case resR of
                Partial sR1 -> Partial $ RunRight bL sR1
                Done bR -> Done bR

    --done (RunBoth sL sR) = do -- which one to pick ?
    done (RunBoth sL _) = doneL sL

    done (RunLeft sL _) = doneL sL
    
    done (RunRight _ sR) = doneR sR

data ConcatMapState m sa a c
    = B !sa
    | forall s. C (s -> a -> m (Step s c)) !s (s -> m c)

-- Compare with foldIterate.
--
-- | Map a 'Fold' returning function on the result of a 'Fold' and run the
-- returned fold. This operation can be used to express data dependencies
-- between fold operations.
--
-- Let's say the first element in the stream is a count of the following
-- elements that we have to add, then:
--
-- >>> import Data.Maybe (fromJust)
-- >>> count = fmap fromJust Fold.head
-- >>> total n = Fold.take n Fold.sum
-- >>> Stream.fold (Fold.concatMap total count) $ Stream.fromList [10,9..1]
-- 45
--
-- /Time: O(n^2) where @n@ is the number of compositions./
--
-- See also: "Streamly.Internal.Data.Stream.IsStream.foldIterateM"
--
-- @since 0.8.0
--
{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Fold m a c) -> Fold m a b -> Fold m a c
concatMap f (Fold stepa initiala extracta) = Fold stepc initialc extractc
  where
    initialc = do
        r <- initiala
        case r of
            Partial s -> return $ Partial (B s)
            Done b -> initInnerFold (f b)

    stepc (B s) a = do
        r <- stepa s a
        case r of
            Partial s1 -> return $ Partial (B s1)
            Done b -> initInnerFold (f b)

    stepc (C stepInner s extractInner) a = do
        r <- stepInner s a
        return $ case r of
            Partial sc -> Partial (C stepInner sc extractInner)
            Done c -> Done c

    extractc (B s) = do
        r <- extracta s
        initExtract (f r)
    extractc (C _ sInner extractInner) = extractInner sInner

    initInnerFold (Fold step i e) = do
        r <- i
        return $ case r of
            Partial s -> Partial (C step s e)
            Done c -> Done c

    initExtract (Fold _ i e) = do
        r <- i
        case r of
            Partial s -> e s
            Done c -> return c

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @lmap f fold@ maps the function @f@ on the input of the fold.
--
-- >>> Stream.fold (Fold.lmap (\x -> x * x) Fold.sum) (Stream.enumerateFromTo 1 100)
-- 338350
--
-- > lmap = Fold.lmapM return
--
-- @since 0.8.0
{-# INLINABLE lmap #-}
lmap :: (a -> b) -> Fold m b r -> Fold m a r
lmap f (Fold step begin done) = Fold step' begin done
    where
    step' x a = step x (f a)

-- XXX should be removed
-- |
-- /Internal/
{-# INLINE map #-}
map :: (a -> b) -> Fold m b r -> Fold m a r
map = lmap

-- | @lmapM f fold@ maps the monadic function @f@ on the input of the fold.
--
-- @since 0.8.0
{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = f a >>= step x

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- >>> Stream.fold (Fold.filter (> 5) Fold.sum) $ Stream.fromList [1..10]
-- 40
--
-- > filter f = Fold.filterM (return . f)
--
-- @since 0.8.0
{-# INLINABLE filter #-}
filter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
filter f (Fold step begin done) = Fold step' begin done
    where
    step' x a = if f a then step x a else return $ Partial x

-- | Like 'filter' but with a monadic predicate.
--
-- @since 0.8.0
{-# INLINABLE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
filterM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = do
      use <- f a
      if use then step x a else return $ Partial x

-- | Modify a fold to receive a 'Maybe' input, the 'Just' values are unwrapped
-- and sent to the original fold, 'Nothing' values are discarded.
--
-- @since 0.8.0
{-# INLINE catMaybes #-}
catMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
catMaybes = filter isJust . map fromJust

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- | Take at most @n@ input elements and fold them using the supplied fold. A
-- negative count is treated as 0.
--
-- >>> Stream.fold (Fold.take 2 Fold.toList) $ Stream.fromList [1..10]
-- [1,2]
--
-- @since 0.8.0
{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Fold m a b
take n (Fold fstep finitial fextract) = Fold step initial extract

    where

    initial = do
        res <- finitial
        case res of
            Partial s ->
                if n > 0
                then return $ Partial $ Tuple' 0 s
                else Done <$> fextract s
            Done b -> return $ Done b

    step (Tuple' i r) a = do
        res <- fstep r a
        case res of
            Partial sres -> do
                let i1 = i + 1
                    s1 = Tuple' i1 sres
                if i1 < n
                then return $ Partial s1
                else Done <$> fextract sres
            Done bres -> return $ Done bres

    extract (Tuple' _ r) = fextract r

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | Modify the fold such that it returns a new 'Fold' instead of the output.
-- If the fold was already done the returned fold would always yield the
-- result. If the fold was partial, the returned fold starts from where we left
-- i.e. it uses the last accumulator value as the initial value of the
-- accumulator. Thus we can resume the fold later and feed it more input.
--
-- >>> :{
-- do
--  more <- Stream.fold (Fold.duplicate Fold.sum) (Stream.enumerateFromTo 1 10)
--  evenMore <- Stream.fold (Fold.duplicate more) (Stream.enumerateFromTo 11 20)
--  Stream.fold evenMore (Stream.enumerateFromTo 21 30)
-- :}
-- 465
--
-- /Pre-release/
{-# INLINABLE duplicate #-}
duplicate :: Monad m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step1 initial1 extract1) =
    Fold step initial (\s -> pure $ Fold step1 (pure $ Partial s) extract1)

    where

    initial = second yield <$> initial1

    step s a = second yield <$> step1 s a

-- | Run the initialization effect of a fold. The returned fold would use the
-- value returned by this effect as its initial value.
--
-- /Pre-release/
{-# INLINABLE initialize #-}
initialize :: Monad m => Fold m a b -> m (Fold m a b)
initialize (Fold step initial extract) = do
    i <- initial
    return $ Fold step (return i) extract

-- | Run one step of a fold and store the accumulator as an initial value in
-- the returned fold.
--
-- /Pre-release/
{-# INLINABLE runStep #-}
runStep :: Monad m => Fold m a b -> a -> m (Fold m a b)
runStep (Fold step initial extract) a = return $ Fold step initial1 extract

    where

    initial1 = do
        res <- initial
        case res of
              Partial fs -> step fs a
              b@(Done _) -> return b

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream. groupBy et al can be written as terminating
-- folds and then we can apply "many" to use those repeatedly on a stream.

{-# ANN type ManyState Fuse #-}
data ManyState s1 s2
    = ManyFirst !s1 !s2
    | ManyLoop !s1 !s2

-- | Collect zero or more applications of a fold.  @many split collect@ applies
-- the @split@ fold repeatedly on the input stream and accumulates zero or more
-- fold results using @collect@.
--
-- >>> two = Fold.take 2 Fold.toList
-- >>> twos = Fold.many two Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- Stops when @collect@ stops.
--
-- See also: "Streamly.Prelude.concatMap", "Streamly.Prelude.foldMany"
--
-- @since 0.8.0
--
{-# INLINE many #-}
many :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
many (Fold sstep sinitial sextract) (Fold cstep cinitial cextract) =
    Fold step initial extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE handleSplitStep #-}
    handleSplitStep branch cs sres =
        case sres of
            Partial ss1 -> return $ Partial $ branch ss1 cs
            Done sb -> runCollector ManyFirst cs sb

    {-# INLINE handleCollectStep #-}
    handleCollectStep branch cres =
        case cres of
            Partial cs -> do
                sres <- sinitial
                handleSplitStep branch cs sres
            Done cb -> return $ Done cb

    -- Do not inline this
    runCollector branch cs sb = cstep cs sb >>= handleCollectStep branch

    initial = cinitial >>= handleCollectStep ManyFirst

    {-# INLINE step_ #-}
    step_ ss cs a = do
        sres <- sstep ss a
        handleSplitStep ManyLoop cs sres

    {-# INLINE step #-}
    step (ManyFirst ss cs) a = step_ ss cs a
    step (ManyLoop ss cs) a = step_ ss cs a

    extract (ManyFirst _ cs) = cextract cs
    extract (ManyLoop ss cs) = do
        sb <- sextract ss
        cres <- cstep cs sb
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | Like many, but inner fold emits an output at the end even if no input is
-- received.
--
-- /Internal/
--
-- /See also: Streamly.Prelude.concatMap, Streamly.Prelude.foldMany/
--
{-# INLINE manyPost #-}
manyPost :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
manyPost (Fold sstep sinitial sextract) (Fold cstep cinitial cextract) =
    Fold step initial extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE handleSplitStep #-}
    handleSplitStep cs sres =
        case sres of
            Partial ss1 -> return $ Partial $ Tuple' ss1 cs
            Done sb -> runCollector cs sb

    {-# INLINE handleCollectStep #-}
    handleCollectStep cres =
        case cres of
            Partial cs -> do
                sres <- sinitial
                handleSplitStep cs sres
            Done cb -> return $ Done cb

    -- Do not inline this
    runCollector cs sb = cstep cs sb >>= handleCollectStep

    initial = cinitial >>= handleCollectStep

    {-# INLINE step #-}
    step (Tuple' ss cs) a = do
        sres <- sstep ss a
        handleSplitStep cs sres

    extract (Tuple' ss cs) = do
        sb <- sextract ss
        cres <- cstep cs sb
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | @chunksOf n split collect@ repeatedly applies the @split@ fold to chunks
-- of @n@ items in the input stream and supplies the result to the @collect@
-- fold.
--
-- >>> twos = Fold.chunksOf 2 Fold.toList Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- > chunksOf n split = many (take n split)
--
-- Stops when @collect@ stops.
--
-- @since 0.8.0
--
{-# INLINE chunksOf #-}
chunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
chunksOf n split = many (take n split)

-- |
--
-- /Internal/
{-# INLINE chunksOf2 #-}
chunksOf2 :: Monad m => Int -> Fold m a b -> Fold2 m x b c -> Fold2 m x a c
chunksOf2 n (Fold step1 initial1 extract1) (Fold2 step2 inject2 extract2) =
    Fold2 step' inject' extract'

    where

    loopUntilPartial s = do
        res <- initial1
        case res of
            Partial fs -> return $ Tuple3' 0 fs s
            Done _ -> loopUntilPartial s

    inject' x = inject2 x >>= loopUntilPartial

    step' (Tuple3' i r1 r2) a =
        if i < n
        then do
            res <- step1 r1 a
            case res of
                Partial s -> return $ Tuple3' (i + 1) s r2
                Done b -> step2 r2 b >>= loopUntilPartial
        else extract1 r1 >>= step2 r2 >>= loopUntilPartial

    extract' (Tuple3' _ r1 r2) = extract1 r1 >>= step2 r2 >>= extract2

-- XXX We can use asyncClock here. A parser can be used to return an input that
-- arrives after the timeout.
-- XXX If n is 0 return immediately in initial.
-- XXX we should probably discard the input received after the timeout like
-- takeEndBy_.
--
-- | @takeInterval n fold@ uses @fold@ to fold the input items arriving within
-- a window of first @n@ seconds.
--
-- >>> Stream.fold (Fold.takeInterval 1.0 Fold.toList) $ Stream.delay 0.1 $ Stream.fromList [1..]
-- [1,2,3,4,5,6,7,8,9,10,11]
--
-- Stops when @fold@ stops or when the timeout occurs. Note that the fold needs
-- an input after the timeout to stop. For example, if no input is pushed to
-- the fold until one hour after the timeout had occurred, then the fold will
-- be done only after consuming that input.
--
-- /Pre-release/
--
{-# INLINE takeInterval #-}
takeInterval :: MonadAsync m => Double -> Fold m a b -> Fold m a b
takeInterval n (Fold step initial done) = Fold step' initial' done'

    where

    initial' = do
        res <- initial
        case res of
            Partial s -> do
                mv <- liftIO $ newMVar False
                t <-
                    control $ \run ->
                        mask $ \restore -> do
                            tid <-
                                forkIO
                                  $ catch
                                        (restore $ void $ run (timerThread mv))
                                        (handleChildException mv)
                            run (return tid)
                return $ Partial $ Tuple3' s mv t
            Done b -> return $ Done b

    step' (Tuple3' s mv t) a = do
        val <- liftIO $ readMVar mv
        if val
        then do
            res <- step s a
            case res of
                Partial sres -> Done <$> done sres
                Done bres -> return $ Done bres
        else do
            res <- step s a
            case res of
                Partial fs -> return $ Partial $ Tuple3' fs mv t
                Done b -> liftIO (killThread t) >> return (Done b)

    done' (Tuple3' s _ _) = done s

    timerThread mv = do
        liftIO $ threadDelay (round $ n * 1000000)
        -- Use IORef + CAS? instead of MVar since its a Bool?
        liftIO $ void $ swapMVar mv True

    handleChildException :: MVar Bool -> SomeException -> IO ()
    handleChildException mv _ = void $ swapMVar mv True

-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.

-- | Group the input stream into windows of n second each using the first fold
-- and then fold the resulting groups using the second fold.
--
-- >>> intervals = Fold.intervalsOf 0.5 Fold.toList Fold.toList
-- >>> Stream.fold intervals $ Stream.delay 0.2 $ Stream.fromList [1..10]
-- [[1,2,3,4],[5,6,7],[8,9,10]]
--
-- > intervalsOf n split = many (takeInterval n split)
--
-- /Pre-release/
--
{-# INLINE intervalsOf #-}
intervalsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
intervalsOf n split = many (takeInterval n split)
