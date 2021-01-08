-- |
-- Module      : Streamly.Internal.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- =Overview
--
-- Use 'watchTrees' with a list of file system paths you want to watch as
-- argument. It returns a stream of 'Event' representing the file system events
-- occurring under the watched paths.
--
-- @
-- {-\# LANGUAGE MagicHash #-}
-- Stream.mapM_ (putStrLn . 'showEvent') $ 'watchTrees' [Array.fromCString\# "path"#]
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'showEvent' above)
-- provided in this module are used to determine the attributes of the event.
--
-- =Design notes
--
-- For reference documentation see:
--
-- * "<https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html Apple FS Events Programming Guide>"
-- * "<https://developer.apple.com/documentation/coreservices/file_system_events?language=objc Apple FS Events Reference>"
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =BUGS
--
-- There may be some idiosyncrasies in event reporting.
--
-- 1. Multiple events may be coalesced into a single event having multiple
-- flags set.  For example, on macOS 10.15.6, "touch x; rm x" or "touch x; mv x
-- y" produces an event with both "Created" and "Deleted/Moved" flags set.
--
-- 2. The same event can occur multiple times. For example, "touch x; sleep 1;
-- rm x" generates a "Created" event followed by an event with both "Created"
-- and "Deleted" flags set. Similarly, a cloned event can also occur multiple
-- times.
--
-- 3. Some events can go missing and only the latest one is reported. See
-- 'isCreated' for one such case.
--
-- 4. When @rm -rf@ is used on the watched root directory, only one 'isDeleted'
-- event occurs and that is for the root. However, @rm -rf@ on a directory
-- inside the watch root produces 'isDeleted' events for all files.
--
-- 5. When a file is moved, a move event occurs for both the source and the
-- destination files. There seems to be no way to distinguish the source and
-- the destination. Perhaps the lower eventId can be considered as source.
--
-- 6. When a file is cloned both source and destination generate a cloned
-- event, however the source has lower eventId and destination may have
-- OwnerGroupModeChanged, InodeAttrsChanged, Created flags as well.
--
-- You may have to stat the path of the event to know more information about
-- what might have happened.  It may be a good idea to watch the behavior of
-- events you are handling before you can rely on it. See the individual event
-- APIs for some of the known issues marked with `BUGS`. Also see the "Handling
-- Events" section in the "Apple FS Events Programming Guide".
--
-- * "<https://stackoverflow.com/questions/18415285/osx-fseventstreameventflags-not-working-correctly Stack overflow question on event coalescing>"
--
-- =TODO
--
-- APIs for persistent event-id related functionality, to get events from
-- specific event-id etc are not implemented.

#include <config.h>

-- macOS 10.7+
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS

module Streamly.Internal.FileSystem.Event.Darwin
    (
    -- * Creating a Watch

    -- ** Default configuration
      Config (..)
    , Toggle (..)
    , defaultConfig

    -- ** Watch Behavior
    , BatchInfo (..)
    , setEventBatching

    -- ** Events of Interest
    , setRootMoved
    , setFileEvents
    , setIgnoreSelf
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
    , setFullHistory
#endif

    -- ** Watch APIs
    , watch
    , watchTrees
    , watchTreesWith

    -- * Handling Events
    , Event
    , getEventId
    , getAbsPath

    -- ** Exception Conditions
    , isEventIdWrapped
    , isMustScanSubdirs
    , isKernelDropped
    , isUserDropped

    -- ** Root Level Events
    -- | Events that belong to the root path as a whole and not to specific
    -- itmes contained in it.
    , isMount
    , isUnmount
    , isHistoryDone
    , isRootChanged

    -- ** Item Level Metadata change
    , isOwnerGroupModeChanged
    , isInodeAttrsChanged
    , isFinderInfoChanged
    , isXAttrsChanged

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
    , isCloned
#endif

    -- ** Item Path info
    , isDir
    , isFile
    , isSymLink
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
    , isHardLink
    , isLastHardLink
#endif

    -- * Debugging
    , showEvent
    )
where

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.), (.&.), complement)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..), CDouble(..), CSize(..), CUChar(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.IO.Handle.FD (fdToHandle)
import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Cont (contListMap)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array(..))
import System.IO (Handle, hClose)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Internal/
--
data Config = Config
    { latency :: Double
    , createFlags   :: Word32
    }

-------------------------------------------------------------------------------
-- Batching Events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FileSystem/Event/Darwin.h FSEventStreamCreateFlagNoDefer"
    kFSEventStreamCreateFlagNoDefer :: Word32

data BatchInfo =
      Throttle Double -- ^ Deliver an event immediately but suppress the
                      -- following events upto the specified time.
    | Batch Double    -- ^ Collapse all events that occurred in the specified
                      -- time window (in seconds) and deliver them as a single
                      -- batch.

-- | Set how the events should be batched. See 'BatchInfo' for details.  A
-- negative value for time is treated as 0.
--
-- /default: Batch 0.0/
--
-- /macOS 10.5+/
--
-- /Internal/
--
setEventBatching :: BatchInfo -> Config -> Config
setEventBatching batchInfo cfg@Config{..} =
    let (t, status) =
            case batchInfo of
                Throttle sec | sec < 0 -> (0, On)
                Throttle sec -> (sec, On)
                Batch sec | sec < 0 -> (0, Off)
                Batch sec -> (sec, Off)
    in setFlag kFSEventStreamCreateFlagNoDefer status $ cfg {latency = t}

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

-- | Whether a setting is 'On' or 'Off'.
--
-- /Internal/
--
data Toggle = On | Off

setFlag :: Word32 -> Toggle -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            case status of
                On -> createFlags .|. mask
                Off -> createFlags .&. complement mask
    in cfg {createFlags = flags}

foreign import ccall safe
    "FSEventStreamCreateFlagWatchRoot"
    kFSEventStreamCreateFlagWatchRoot :: Word32

-- | Watch the changes to the path of the top level file system objects being
-- watched. If the root directory is deleted, moved or renamed you will receive
-- an 'isRootChanged' event with an eventId 0.
--
-- /default: Off/
--
-- /macOS 10.5+/
--
-- /Internal/
--
setRootMoved :: Toggle -> Config -> Config
setRootMoved = setFlag kFSEventStreamCreateFlagWatchRoot

foreign import ccall safe
    "FSEventStreamCreateFlagFileEvents"
    kFSEventStreamCreateFlagFileEvents :: Word32

-- | When this is 'Off' only events for the watched directories are reported.
-- For example, when a file is created inside a directory it is reported as an
-- event, the path name in the event is the path of the directory being
-- watched.  Events cannot be distinguished based on the type of event.
--
-- When it is 'On' the path reported in the event is the path of the item and
-- not the of the directory in which it is contained. We can use the event
-- accessor functions to determine the event type and the path type etc.
--
-- /default: On/
--
-- /macOS 10.7+/
--
-- /Internal/
--
setFileEvents :: Toggle -> Config -> Config
setFileEvents = setFlag kFSEventStreamCreateFlagFileEvents

foreign import ccall safe
    "FSEventStreamCreateFlagIgnoreSelf"
    kFSEventStreamCreateFlagIgnoreSelf :: Word32

-- | When this is 'On' events generated by the current process are not
-- reported.
--
-- /default: Off/
--
-- /macOS 10.6+/
--
-- /Internal/
--
setIgnoreSelf :: Toggle -> Config -> Config
setIgnoreSelf = setFlag kFSEventStreamCreateFlagIgnoreSelf

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
foreign import ccall safe
    "FSEventStreamCreateFlagFullHistory"
    kFSEventStreamCreateFlagFullHistory :: Word32

-- | When this is 'On' all events since the beginning of time are reported.
--
-- /default: Off/
--
-- /macOS 10.15+/
--
-- /Internal/
--
setFullHistory :: Toggle -> Config -> Config
setFullHistory = setFlag kFSEventStreamCreateFlagFullHistory
#endif

-------------------------------------------------------------------------------
-- Default config
-------------------------------------------------------------------------------

-- | The default settings are:
--
-- * 'setEventBatching' ('Batch' 0.0)
-- * 'setFileEvents' 'On'
-- * 'setRootMoved' 'Off'
-- * 'setIgnoreSelf' 'Off'
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
-- * 'setFullHistory' 'Off'
#endif
--
-- /Internal/
--
defaultConfig :: Config
defaultConfig = setFileEvents On $ Config
    { latency = 0.0
    , createFlags = 0
    }

-------------------------------------------------------------------------------
-- Open an event stream (C FFI)
-------------------------------------------------------------------------------

-- See FileSystem/Event/Darwin.h

data CWatch

-- Representation of "struct pathName" in "FileSystem/Event/Darwin.h"
data PathName = PathName
    { pathBytes :: Ptr CUChar
    , pathLen :: CSize
    }

instance Storable PathName where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: Ptr CUChar) + sizeOf (undefined :: CSize)
    peek ptr = do
        bptr <- peekByteOff ptr 0
        len <- peekByteOff ptr (sizeOf (undefined :: Ptr CUChar))
        return $ PathName bptr len
    poke ptr PathName{..} = do
        pokeByteOff ptr 0 pathBytes
        pokeByteOff ptr (sizeOf (undefined :: Ptr CUChar)) pathLen

foreign import ccall safe "FileSystem/Event/Darwin.h createWatch" createWatch
    :: Ptr PathName
    -> CInt
    -> Word32
    -> Word64
    -> CDouble
    -> Ptr (CInt)
    -> Ptr (Ptr CWatch)
    -> IO CInt

foreign import ccall safe "FileSystem/Event/Darwin.h destroyWatch" destroyWatch
    :: Ptr CWatch -> IO ()

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A handle for a watch.
data Watch = Watch Handle (Ptr CWatch) (MVar Bool)

-- | Given a 'Config' and @paths@ start monitoring the paths for file system
-- events. Returns a 'Watch' handle which can then be used to read the event
-- stream or to close the watch.
--
-- Implementation Note: This API creates an OS level thread where an event loop
-- runs and writes the events to the write side fd of a POSIX pipe. Event
-- stream APIs read the events from the read end of this pipe.  Since an OS
-- level thread is forked, creating a new watch stream is expensive. It is
-- advised to minimize the number of watches.
--
-- /Internal/
--
openWatch :: Config -> NonEmpty (Array Word8) -> IO Watch
openWatch Config{..} paths = do
    -- XXX check whether the path exists. If the path does not exist and
    -- is created later it cannot be properly monitored. Also a user may
    -- inadvertently provide a path for which the user may not have permission
    -- and then later wonder why events are not being reported.
    withPathNames (NonEmpty.toList paths) $ \arraysPtr ->
        alloca $ \fdPtr -> do
        alloca $ \watchPPtr -> do
            let nArrays = fromIntegral (NonEmpty.length paths)
                seconds = (realToFrac latency)
            r <- createWatch
                    arraysPtr nArrays createFlags 0 seconds fdPtr watchPPtr
            when (r /= 0) $
                ioError (userError "openWatch: failed to create watch.")
            fd <- peek fdPtr
            h <- fdToHandle fd
            watchPtr <- peek watchPPtr
            closeLock <- newMVar False
            return $ Watch h watchPtr closeLock

    where

    withPathName :: Array Word8 -> (PathName -> IO a) -> IO a
    withPathName arr act = do
        A.asPtr arr $ \ptr ->
            let pname = PathName (castPtr ptr) (fromIntegral (A.length arr))
            in act pname

    withPathNames = contListMap withPathName withArray

-- | Close a 'Watch' handle.
--
-- /Internal/
--
closeWatch :: Watch -> IO ()
closeWatch (Watch h watchPtr mvar) = do
    hClose h
    closed <- takeMVar mvar
    when (not closed) $ destroyWatch watchPtr
    putMVar mvar True

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
--
-- /Internal/
--
data Event = Event
   { eventId :: Word64
   , eventFlags :: Word32
   , eventAbsPath :: Array Word8
   } deriving (Show, Ord, Eq)

-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
--
-- XXX should we use a magic in the header to avoid any issues due to
-- misalignment of records? Can that ever happen?
--
readOneEvent :: Parser IO Word8 Event
readOneEvent = do
    arr <- PR.takeEQ 24 (A.writeN 24)
    let arr1 = A.unsafeCast arr :: Array Word64
        eid = A.unsafeIndex arr1 0
        eflags = A.unsafeIndex arr1 1
        pathLen = fromIntegral $ A.unsafeIndex arr1 2
    path <- PR.takeEQ pathLen (A.writeN pathLen)
    return $ Event
        { eventId = eid
        , eventFlags = fromIntegral eflags
        , eventAbsPath = path
        }

watchToStream :: Watch -> SerialT IO Event
watchToStream (Watch handle _ _) =
    S.parseMany readOneEvent $ S.unfold FH.read handle

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories.  When the path is a directory, the
-- whole directory tree under it is watched recursively. Monitoring starts from
-- the current time onwards. The paths are specified as UTF-8 encoded 'Array'
-- of 'Word8'.
--
-- If the path name to be watched is a symbolic link then the target of the
-- link is watched instead of the symbolic link itself. If the symbolic link is
-- removed later the target is still watched as usual.
--
-- When 'setFileEvents' is 'Off' then events occurring inside the watch root
-- cannot be distinguished from each other.  For example, we cannot determine
-- if an event is a "create" or "delete", we only know that some event ocurred
-- under the watched directory hierarchy.  Also, no path information for the
-- target item of the event is generated.
--
-- If the watched path is deleted and created again the events start coming
-- again for that path. A file type path being watched could be deleted and
-- recreated as a directory. If the watched path is moved to another path (or
-- is deleted and recreated as a symbolic link to another path) then no events
-- are reported for any changes under the new path.
--
-- BUGS: If a watch is started on a non-existing path then the path is not
-- watched even if it is created later.  The macOS API does not fail for a
-- non-existing path.  If a non-existing path is watched with 'setRootMoved'
-- then an 'isRootChanged' event is reported if the path is created later and
-- the "path" field in the event is set to the dirname of the path rather than
-- the full absolute path. This is the observed behavior on macOS 10.15.1.
--
-- @
-- {-\# LANGUAGE MagicHash #-}
-- watchTreesWith 
--      ('setIgnoreSelf' 'On' . 'setRootMoved' 'On')
--      [Array.fromCString\# "path"#]
-- @
--
-- /Internal/
--
watchTreesWith ::
    (Config -> Config) -> NonEmpty (Array Word8) -> SerialT IO Event
watchTreesWith f paths = S.bracket before after watchToStream

    where

    before = liftIO $ openWatch (f defaultConfig) paths
    after = (liftIO . closeWatch)

-- | Like 'watchTreesWith' but uses the 'defaultConfig' options.
--
-- @
-- watchTrees = watchTreesWith id
-- @
--
watchTrees :: NonEmpty (Array Word8) -> SerialT IO Event
watchTrees = watchTreesWith id

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration. The paths could be files or directories.
-- When the path is a directory, the whole directory tree under it is watched
-- recursively. Monitoring starts from the current time onwards. The paths are
-- specified as UTF-8 encoded 'Array' of 'Word8'.
--
-- @
-- {-\# LANGUAGE MagicHash #-}
-- watch True ('setIgnoreSelf' 'On' . 'setRootMoved' 'On') [Array.fromCString\# "path"#]
-- @
--
-- /Internal/
--
watch :: Bool -> Config -> NonEmpty (Array Word8) -> SerialT IO Event
watch _ cfg paths = watchTreesWith (\_ -> cfg) paths

-------------------------------------------------------------------------------
-- Examine the event stream
-------------------------------------------------------------------------------

-- | Get the event id of an 'Event'.  Event-id is a monotonically increasing
-- 64-bit integer identifying an event uniquely.
--
-- /Internal/
--
getEventId :: Event -> Word64
getEventId Event{..} = eventId

foreign import ccall safe
    "FSEventStreamEventFlagEventIdsWrapped"
    kFSEventStreamEventFlagEventIdsWrapped :: Word32

-- | Determine whether the event id has wrapped. This is impossible on any
-- traditional hardware in any reasonable amount of time unless the event-id
-- starts from a very high value, because the event-id is a 64-bit integer.
-- However, apple still recommends to check and handle this flag.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isEventIdWrapped :: Event -> Bool
isEventIdWrapped = getFlag kFSEventStreamEventFlagEventIdsWrapped

-- | Get the absolute path of the file system object for which the event is
-- generated. The path is a UTF-8 encoded array of bytes.
--
-- /Internal/
--
getAbsPath :: Event -> Array Word8
getAbsPath Event{..} = eventAbsPath

-------------------------------------------------------------------------------
-- Event types
-------------------------------------------------------------------------------

getFlag :: Word32 -> Event -> Bool
getFlag mask Event{..} = eventFlags .&. mask /= 0

-------------------------------------------------------------------------------
-- Error events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagMustScanSubDirs"
    kFSEventStreamEventFlagMustScanSubDirs :: Word32

-- | Apple documentation says that "If an event in a directory occurs at about
-- the same time as one or more events in a subdirectory of that directory, the
-- events may be coalesced into a single event." In that case you will recieve
-- 'isMustScanSubdirs' event. In that case the path listed in the event is
-- invalidated and you must rescan it to know the current state.
--
-- This event can also occur if a communication error (overflow) occurs in
-- sending the event and the event gets dropped. In that case 'isKernelDropped'
-- and/or 'isUserDropped' attributes will also be set.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isMustScanSubdirs :: Event -> Bool
isMustScanSubdirs = getFlag kFSEventStreamEventFlagMustScanSubDirs

foreign import ccall safe
    "FSEventStreamEventFlagKernelDropped"
    kFSEventStreamEventFlagKernelDropped :: Word32

-- | Did an event get dropped due to a kernel processing issue? Set only when
-- 'isMustScanSubdirs' is also true.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isKernelDropped :: Event -> Bool
isKernelDropped = getFlag kFSEventStreamEventFlagKernelDropped

foreign import ccall safe
    "FSEventStreamEventFlagUserDropped"
    kFSEventStreamEventFlagUserDropped :: Word32

-- | Did an event get dropped due to a user process issue? Set only when
-- 'isMustScanSubdirs' is also true.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isUserDropped :: Event -> Bool
isUserDropped = getFlag kFSEventStreamEventFlagUserDropped

-------------------------------------------------------------------------------
-- Global Sentinel Events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagHistoryDone"
    kFSEventStreamEventFlagHistoryDone :: Word32

-- | Determine whether the event is a history done marker event. A history done
-- event is generated when the historical events from before the current time
-- are done. Historical events are generated when the "since" parameter in the
-- watch config is set to before the current time.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isHistoryDone :: Event -> Bool
isHistoryDone = getFlag kFSEventStreamEventFlagHistoryDone

-------------------------------------------------------------------------------
-- Events affecting the watched path only
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagRootChanged"
    kFSEventStreamEventFlagRootChanged :: Word32

-- | Determine whether the event indicates a change of path of the monitored
-- object itself. Note that the object may become unreachable or deleted after
-- a change of path.
--
-- /Applicable only when 'setRootMoved' is 'On'/
--
-- /Occurs only for the watched path/
--
-- /macOS 10.5+/
--
-- /Internal/
--
isRootChanged :: Event -> Bool
isRootChanged = getFlag kFSEventStreamEventFlagRootChanged

-------------------------------------------------------------------------------
-- Global events under the watched path
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagMount"
    kFSEventStreamEventFlagMount :: Word32

-- | Determine whether the event is a mount event. A mount event is generated
-- if a volume is mounted under the path being watched.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isMount :: Event -> Bool
isMount = getFlag kFSEventStreamEventFlagMount

foreign import ccall safe
    "FSEventStreamEventFlagUnmount"
    kFSEventStreamEventFlagUnmount :: Word32

-- | Determine whether the event is an unmount event. An unmount event is
-- generated if a volume mounted under the path being watched is unmounted.
--
-- /macOS 10.5+/
--
-- /Internal/
--
isUnmount :: Event -> Bool
isUnmount = getFlag kFSEventStreamEventFlagUnmount

-------------------------------------------------------------------------------
-- Metadata change Events (applicable only when 'setFileEvents' is 'On')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemChangeOwner"
    kFSEventStreamEventFlagItemChangeOwner :: Word32

-- | Determine whether the event is ownership, group, permissions or ACL change
-- event of an object contained within the monitored path.
--
-- Note that this event may be generated even if the metadata is changed to the
-- same value again.
--
-- /Applicable only when 'setFileEvents' is 'On'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isOwnerGroupModeChanged :: Event -> Bool
isOwnerGroupModeChanged = getFlag kFSEventStreamEventFlagItemChangeOwner

foreign import ccall safe
    "FSEventStreamEventFlagItemInodeMetaMod"
    kFSEventStreamEventFlagItemInodeMetaMod :: Word32

-- | Determine whether the event indicates inode metadata change for an object
-- contained within the monitored path. This event is generated when inode
-- attributes other than the owner, group or permissions are changed e.g. file
-- modification time. It does not occur when the link count of a file changes
-- (as of macOS 10.15.1).
--
-- /Applicable only when 'setFileEvents' is 'On'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isInodeAttrsChanged :: Event -> Bool
isInodeAttrsChanged = getFlag kFSEventStreamEventFlagItemInodeMetaMod

foreign import ccall safe
    "FSEventStreamEventFlagItemFinderInfoMod"
    kFSEventStreamEventFlagItemFinderInfoMod :: Word32

-- | Determine whether the event indicates finder information metadata change
-- for an object contained within the monitored path.
--
-- /Applicable only when 'setFileEvents' is 'On'/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isFinderInfoChanged :: Event -> Bool
isFinderInfoChanged = getFlag kFSEventStreamEventFlagItemFinderInfoMod

foreign import ccall safe
    "FSEventStreamEventFlagItemXattrMod"
    kFSEventStreamEventFlagItemXattrMod :: Word32

-- | Determine whether the event indicates extended attributes metadata change
-- for an object contained within the monitored path.
--
-- /Applicable only when 'setFileEvents' is 'On'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isXAttrsChanged :: Event -> Bool
isXAttrsChanged = getFlag kFSEventStreamEventFlagItemXattrMod

-------------------------------------------------------------------------------
-- CRUD Events (applicable only when 'setFileEvents' is 'On')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemCreated"
    kFSEventStreamEventFlagItemCreated :: Word32

-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object other
-- than a hard link is created.  On hard linking only an 'isInodeAttrsChanged'
-- event on the directory is generated, it is not a create event. However, when
-- a hard link is deleted 'isDeleted' and 'isHardLink' both are true.
--
-- This event can occur for the watched path if the path was deleted/moved and
-- created again (tested on macOS 10.15.1). However, if a watch is started on a
-- non-existing path and the path is created later, then this event is not
-- generated, the path is not watched.
--
-- BUGS: On 10.15.1 when we use a "touch x" to create a file for the first time
-- only an 'isInodeAttrsChanged' event occurs and there is no 'isCreated'
-- event. However, this seems to have been fixed on 10.15.6.
--
-- /Applicable only when 'setFileEvents' is 'On'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isCreated :: Event -> Bool
isCreated = getFlag kFSEventStreamEventFlagItemCreated

foreign import ccall safe
    "FSEventStreamEventFlagItemRemoved"
    kFSEventStreamEventFlagItemRemoved :: Word32

-- | Determine whether the event indicates deletion of an object within the
-- monitored path. This is true when a file or a hardlink is deleted.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isDeleted :: Event -> Bool
isDeleted = getFlag kFSEventStreamEventFlagItemRemoved

foreign import ccall safe
    "FSEventStreamEventFlagItemRenamed"
    kFSEventStreamEventFlagItemRenamed :: Word32

-- | Determine whether the event indicates rename of an object within the
-- monitored path. This event is generated when a file is renamed within the
-- watched directory or if it is moved out of or in the watched directory.
--
-- /Applicable only when 'setFileEvents' is 'On'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Internal/
--
isMoved :: Event -> Bool
isMoved = getFlag kFSEventStreamEventFlagItemRenamed

foreign import ccall safe
    "FSEventStreamEventFlagItemModified"
    kFSEventStreamEventFlagItemModified :: Word32

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.7+/
--
-- /Internal/
--
isModified :: Event -> Bool
isModified = getFlag kFSEventStreamEventFlagItemModified

#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
foreign import ccall safe
    "FSEventStreamEventFlagItemCloned"
    kFSEventStreamEventFlagItemCloned :: Word32

-- | Determine whether the event indicates cloning of an object within the
-- monitored path. The "Duplicate" command in the "File" menu of the "Finder"
-- application generates a "clone" event.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.13+/
--
-- /Internal/
--
isCloned :: Event -> Bool
isCloned = getFlag kFSEventStreamEventFlagItemCloned
#endif

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'On')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemIsDir"
    kFSEventStreamEventFlagItemIsDir :: Word32

-- | Determine whether the event is for a directory path.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.7+/
--
-- /Internal/
--
isDir :: Event -> Bool
isDir = getFlag kFSEventStreamEventFlagItemIsDir

foreign import ccall safe
    "FSEventStreamEventFlagItemIsFile"
    kFSEventStreamEventFlagItemIsFile :: Word32

-- | Determine whether the event is for a file path.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.7+/
--
-- /Internal/
--
isFile :: Event -> Bool
isFile = getFlag kFSEventStreamEventFlagItemIsFile

foreign import ccall safe
    "FSEventStreamEventFlagItemIsSymlink"
    kFSEventStreamEventFlagItemIsSymlink :: Word32

-- | Determine whether the event is for a symbolic link path.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.7+/
--
-- /Internal/
--
isSymLink :: Event -> Bool
isSymLink = getFlag kFSEventStreamEventFlagItemIsSymlink

#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
foreign import ccall safe
    "FSEventStreamEventFlagItemIsHardlink"
    kFSEventStreamEventFlagItemIsHardlink :: Word32

-- | Determine whether the event is for a file with more than one hard link.
-- When 'isFile' is true we can check for 'isHardLink'. Note that 'isHardLink'
-- is not true when a hard link is created, however, it is true when a file
-- which has or had more than one link in the past is removed. This is not true
-- if a file never had more than one link.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.10+/
--
-- /Internal/
--
isHardLink :: Event -> Bool
isHardLink = getFlag kFSEventStreamEventFlagItemIsHardlink

foreign import ccall safe
    "FSEventStreamEventFlagItemIsLastHardlink"
     kFSEventStreamEventFlagItemIsLastHardlink :: Word32

-- | Determine whether the event is for a hard link path with only one hard
-- link.  If 'isHardLink' is true then we can check for 'isLastHardLink'.  This
-- is true when a file has had more than one link and now the last link is
-- removed. In that case both 'isHardLink' and 'isLastHardLink" would be true.
--
-- Applicable only when 'setFileEvents' is 'On'.
--
-- /macOS 10.10+/
--
-- /Internal/
--
isLastHardLink :: Event -> Bool
isLastHardLink = getFlag kFSEventStreamEventFlagItemIsLastHardlink
#endif

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
    let path = runIdentity $ S.toList $ U.decodeUtf8' $ A.toStream eventAbsPath
    in "--------------------------"
        ++ "\nId = " ++ show eventId
        ++ "\nPath = " ++ show path
        ++ "\nFlags " ++ show eventFlags

        ++ showev isMustScanSubdirs "MustScanSubdirs"
        ++ showev isKernelDropped "KernelDropped"
        ++ showev isUserDropped "UserDropped"

        ++ showev isRootChanged "RootChanged"
        ++ showev isMount "Mount"
        ++ showev isUnmount "Unmount"
        ++ showev isHistoryDone "HistoryDone"

        ++ showev isOwnerGroupModeChanged "OwnerGroupModeChanged"
        ++ showev isInodeAttrsChanged "InodeAttrsChanged"
        ++ showev isFinderInfoChanged "FinderInfoChanged"
        ++ showev isXAttrsChanged "XAttrsChanged"

        ++ showev isCreated "Created"
        ++ showev isDeleted "Deleted"
        ++ showev isModified "Modified"
        ++ showev isMoved "Moved"
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
        ++ showev isCloned "Cloned"
#endif

        ++ showev isDir "Dir"
        ++ showev isFile "File"
        ++ showev isSymLink "SymLink"
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
        ++ showev isHardLink "HardLink"
        ++ showev isLastHardLink "LastHardLink"
#endif
        ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""
#else
module Streamly.Internal.FileSystem.Event.Darwin () where
#warning "Autoconf did not find the definition \
kFSEventStreamCreateFlagFileEvents in Darwin header files.\
Do you have Cocoa framework header files installed?\
Not compiling the Streamly.Internal.FileSystem.Event.Darwin module. \
Programs depending on this module may not compile. \
Check if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS is defined in config.h \
generated from src/config.h.in"
#endif
