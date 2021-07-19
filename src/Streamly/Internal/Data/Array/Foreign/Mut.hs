#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Foreign.Mut
    (
    -- , writeIndex
    -- , writeFrom -- start writing at the given position
    -- , writeFromRev
    -- , writeTo   -- write from beginning up to the given position
    -- , writeToRev
    -- , writeFromTo
    -- , writeFromThenTo
    --
    -- , writeChunksOfFrom
    -- , ...

    writeIndex
    --, writeIndices
    --, writeRanges
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable(..))
import Streamly.Internal.BaseCompat (unsafeWithForeignPtr)

import Prelude hiding (length)
import Streamly.Internal.Data.Array.Foreign.Mut.Type

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- /Pre-release/
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Storable a) => Array a -> Int -> a -> m ()
writeIndex arr i a = do
    let maxIndex = length arr - 1
    if i < 0
    then error "writeIndex: negative array index"
    else if i > maxIndex
         then error $ "writeIndex: specified array index " ++ show i
                    ++ " is beyond the maximum index " ++ show maxIndex
         else
            liftIO $ unsafeWithForeignPtr (aStart arr) $ \p ->
                pokeElemOff p i a
