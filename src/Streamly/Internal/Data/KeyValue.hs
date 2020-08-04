{-# LANGUAGE TupleSections       #-}

-- |
-- Module      : Streamly.Internal.Data.KeyValue
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.KeyValue
    (
      IsKeyValue (..)
    , KVList (..)
    )
where

import Data.Function (on)
import Data.List (deleteBy)

-- We need a typeclass to which we can add to and delete from. Monoid only
-- allows appends.

class IsKeyValue f where
    kvEmpty :: f k v
    kvInsert :: (k, v) -> f k v -> f k v
    kvDelete :: Eq k => k -> f k v -> f k v

data KVList k a = KVList [(k, a)]

instance IsKeyValue KVList where
    kvEmpty = KVList []
    kvInsert (k, v) (KVList xs) = KVList ((k, v) : xs)
    kvDelete k (KVList xs) =
        KVList (deleteBy ((==) `on` fst) (k, undefined) xs)

instance Functor (KVList k) where
    fmap f (KVList xs) = KVList $ fmap (\(k,v) -> (k, f v)) xs

-- XXX check perf of these instances
-- XXX we can write a custom mapM_
instance Foldable (KVList k) where
    foldMap f (KVList xs) = Prelude.foldMap (\(_,v) -> f v) xs

instance Traversable (KVList k) where
    traverse f (KVList xs) = KVList <$> traverse (\(k,v) -> (k,) <$> f v) xs

