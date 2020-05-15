{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Streamly.Internal.Data.IORef
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.IORef
    (
      newFinalizedIORef
    , runIORefFinalizer
    , clearIORefFinalizer
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef, IORef)

import Streamly.Internal.Data.SVar

-- | Create an IORef holding a finalizer that is called automatically when the
-- IORef is garbage collected. The IORef can be written to with a 'Nothing'
-- value to deactivate the finalizer.
newFinalizedIORef :: (MonadIO m, MonadBaseControl IO m)
    => m a -> m (IORef (Maybe (IO ())))
newFinalizedIORef finalizer = do
    mrun <- captureMonadState
    ref <- liftIO $ newIORef $ Just $ liftIO $ void $ do
                _ <- runInIO mrun finalizer
                return ()
    let finalizer1 = do
            res <- readIORef ref
            case res of
                Nothing -> return ()
                Just f -> f
    _ <- liftIO $ mkWeakIORef ref finalizer1
    return ref

-- | Run the finalizer stored in an IORef and deactivate it so that it is run
-- only once.
--
runIORefFinalizer :: MonadIO m => IORef (Maybe (IO ())) -> m ()
runIORefFinalizer ref = liftIO $ do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just f -> writeIORef ref Nothing >> f

-- | Deactivate the finalizer stored in an IORef without running it.
--
clearIORefFinalizer :: MonadIO m => IORef (Maybe (IO ())) -> m ()
clearIORefFinalizer ref = liftIO $ writeIORef ref Nothing
