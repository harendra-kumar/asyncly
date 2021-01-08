-- |
-- Module      : Streamly.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.FileSystem.Event
    ( Config
    , Event
    , Toggle
    , defaultConfig
    , setAllEvents

    -- ** Events of Interest
    , setRootMoved

    -- ** Watch APIs
    , watch

    -- * Handling Events
    , getAbsolutePath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified

    -- ** Exception Conditions
    , isOverflow

    -- * Debugging
    , showEvent
    )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Storable.Foreign (Array)
import Streamly.Prelude (SerialT)

#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform"
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

type Toggle = Event.Toggle
type Config = Event.Config
type Event = Event.Event

setRootMoved :: Toggle -> Config -> Config
setRootMoved = Event.setRootMoved

setAllEvents :: Toggle -> Config -> Config
#if defined(CABAL_OS_DARWIN)
setAllEvents s cfg = defaultConfig
#else
setAllEvents = Event.setAllEvents
#endif

getAbsolutePath :: Event -> String
getAbsolutePath = Event.getAbsPath

isCreated :: Event -> Bool
isCreated = Event.isCreated

isDeleted :: Event -> Bool
isDeleted = Event.isDeleted

isMoved :: Event -> Bool
#if defined(CABAL_OS_DARWIN)
isMoved = Event.isMoved
#else
isMoved ev = Event.isMovedFrom ev || Event.isMovedTo ev
#endif

isModified :: Event -> Bool
isModified = Event.isModified

isOverflow :: Event -> Bool
#if defined(CABAL_OS_DARWIN)
isOverflow = Event.isMustScanSubdirs
#else
isOverflow = Event.isOverflow
#endif

showEvent :: Event -> String
showEvent = Event.showEvent

defaultConfig :: Config
#if defined(CABAL_OS_DARWIN)
defaultConfig = Event.Config 0.0 0
#else
defaultConfig = Event.Config True 0
#endif

watch :: Bool -> Config -> NonEmpty (Array Word8) -> SerialT IO Event.Event
watch = Event.watch
