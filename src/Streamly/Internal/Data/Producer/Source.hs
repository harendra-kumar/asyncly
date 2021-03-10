-- |
-- Module      : Streamly.Internal.Data.Producer.Source
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Source' is a seed that can be unfolded to a stream with a buffer.  Allows
-- to 'unread' data i.e.  push unused data back to the source buffer. This is
-- useful in parsing applications with backtracking.
--

module Streamly.Internal.Data.Producer.Source
    ( Source

    -- * Creation
    , source

    -- * Transformation
    , unread

    -- * Consumption
    , isEmpty
    , producer

    -- * Parsing
    , parse
    , parseMany
    , parseManyD
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow, throwM)
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser.ParserD (ParseError(..), Step(..))
import Streamly.Internal.Data.Producer.Type (Producer(..), Step(..))

import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Parser.ParserK.Types as ParserK

import Prelude hiding (read)

-- | A seed with a buffer. It allows us to 'unread' or return some data
-- after reading it. Useful in backtracked parsing.
--
data Source a b = Source [b] (Maybe a)

-- | Make a source from a seed value. The buffer would start as empty.
--
-- /Pre-release/
source :: Maybe a -> Source a b
source = Source []

-- | Return some unused data back to the source. The data is prepended (or
-- consed) to the source.
--
-- /Pre-release/
unread :: [b] -> Source a b -> Source a b
unread xs (Source ys seed) = Source (xs ++ ys) seed

-- | Determine if the source is empty.
isEmpty :: Source a b -> Bool
isEmpty (Source [] Nothing) = True
isEmpty _ = False

-- | Convert a producer to a producer from a buffered source. Any buffered data
-- is read first and then the seed is unfolded.
--
-- /Pre-release/
{-# INLINE_NORMAL producer #-}
producer :: Monad m => Producer m a b -> Producer m (Source a b) b
producer (Producer step1 inject1 extract1) = Producer step inject extract

    where

    inject (Source [] (Just a)) = do
        s <- inject1 a
        return $ Left s
    inject (Source xs a) = return $ Right (xs, a)

    {-# INLINE_LATE step #-}
    step (Left s) = do
        r <- step1 s
        return $ case r of
            Yield x s1 -> Yield x (Left s1)
            Skip s1 -> Skip (Left s1)
            Stop res -> Stop $ fmap (Source [] . Just) res
    step (Right ([], Nothing)) = return $ Stop Nothing
    step (Right ([], Just _)) = error "Bug: unreachable"
    step (Right (x:[], Just a)) = do
        s <- inject1 a
        return $ Yield x (Left s)
    step (Right (x:xs, a)) = return $ Yield x (Right (xs, a))

    extract (Left s) = fmap (Source [] . Just) <$> extract1 s
    extract (Right (xs, a)) = return $ Just $ Source xs a

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

{-# INLINE_NORMAL parseD #-}
parseD
    :: MonadThrow m
    => ParserD.Parser m a b
    -> Producer m (Source s a) a
    -> Source s a
    -> m (b, Source s a)
parseD
    (ParserD.Parser pstep initial extract)
    (Producer ustep uinject uextract)
    seed = do

    res <- initial
    case res of
        ParserD.IPartial s -> do
            state <- uinject seed
            go SPEC state (List []) s
        ParserD.IDone b -> return (b, seed)
        ParserD.IError err -> throwM $ ParseError err

    where

    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st buf !pst = do
        r <- ustep st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    Partial 0 pst1 -> go SPEC s (List []) pst1
                    Partial n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1
                    Continue 0 pst1 -> go SPEC s (List (x:getList buf)) pst1
                    Continue n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1
                    Done n b -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        s1 <- uextract s
                        let s2 = case s1 of
                                Nothing -> source Nothing
                                Just v -> v
                        return (b, unread src s2)
                    Error err -> throwM $ ParseError err
            Skip s -> go SPEC s buf pst
            Stop res -> do
                b <- extract pst
                let src = case res of
                        Nothing -> source Nothing
                        Just v -> v
                return (b, unread (reverse $ getList buf) src)

    gobuf !_ s buf (List []) !pst = go SPEC s buf pst
    gobuf !_ s buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            Partial 0 pst1 ->
                gobuf SPEC s (List []) (List xs) pst1
            Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List []) (List src) pst1
            Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList buf)) (List xs) pst1
            Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1
            Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                s1 <- uextract s
                let s2 = case s1 of
                        Nothing -> source Nothing
                        Just v -> v
                return (b, unread src s2)
            Error err -> throwM $ ParseError err

-- | Parse a buffered source using a parser, returning the parsed value and the
-- remaining source.
--
-- /Pre-release/
{-# INLINE [3] parse #-}
parse
    :: MonadThrow m
    => ParserK.Parser m a b
    -> Producer m (Source s a) a
    -> Source s a
    -> m (b, Source s a)
parse = parseD . ParserK.fromParserK

-------------------------------------------------------------------------------
-- Nested parsing
-------------------------------------------------------------------------------

{-# INLINE parseManyD #-}
parseManyD :: MonadThrow m =>
       ParserD.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseManyD parser reader = Producer step return (return . Just)

    where

    {-# INLINE_LATE step #-}
    step src = do
        if isEmpty src
        then return $ Stop $ Just src
        else do
            (b, s1) <- parseD parser reader src
            return $ Yield b s1

-- | Apply a parser repeatedly on a buffered source producer to generate a
-- producer of parsed values.
--
-- /Pre-release/
{-# INLINE parseMany #-}
parseMany :: MonadThrow m =>
       ParserK.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseMany parser = parseManyD (ParserK.fromParserK parser)
