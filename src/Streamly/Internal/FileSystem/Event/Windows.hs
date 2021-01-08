-- Some code snippets are adapted from the fsnotify package.
-- http://hackage.haskell.org/package/fsnotify-0.3.0.1/
--
-- |
-- Module      : Streamly.Internal.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
--               (c) 2012, Mark Dittmer
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- =Overview
--
-- Use 'watchTrees'or 'watchPaths' with a list of file system paths you want to
-- watch as argument. It returns a stream of 'Event' representing the file
-- system events occurring under the watched paths.
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
-- For Windows reference documentation see:
--
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information file notify information>
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw read directory changes>
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =Availability
--
-- As per the Windows reference docs, the fs event notification API is
-- available in:
--
-- * Minimum supported client: Windows XP [desktop apps | UWP apps]
-- * Minimum supported server: Windows Server 2003 [desktop apps | UWP apps

module Streamly.Internal.FileSystem.Event.Windows
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config (..)
    , Event (..)
    , Toggle (..)
    , setFlag
    , defaultConfig
    , getConfigFlag
    , setAllEvents

    -- ** Watch Behavior
    , setRecursiveMode

    -- ** Events of Interest
    -- *** Root Level Events
    , setModifiedFileName
    , setRootMoved
    , setModifiedAttribute
    , setModifiedSize
    , setModifiedLastWrite
    , setModifiedSecurity

    -- ** Watch APIs
    , watch
    , watchTrees
    , watchTreesWith

    -- * Handling Events
    , getRelPath
    , getRoot
    , getAbsPath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMovedFrom
    , isMovedTo
    , isModified

    -- ** Exception Conditions
    , isOverflow

    -- * Debugging
    , showEvent
    )
where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bits ((.|.), (.&.), complement)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Foreign.C.String (peekCWStringLen)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peekByteOff)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, nullFunPtr, plusPtr)
import Streamly.Prelude (SerialT, parallel)
import System.FilePath ((</>))
import System.Win32.File
    ( FileNotificationFlag
    , LPOVERLAPPED
    , closeHandle
    , createFile
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_LIST_DIRECTORY
    , fILE_NOTIFY_CHANGE_FILE_NAME
    , fILE_NOTIFY_CHANGE_DIR_NAME
    , fILE_NOTIFY_CHANGE_ATTRIBUTES
    , fILE_NOTIFY_CHANGE_SIZE
    , fILE_NOTIFY_CHANGE_LAST_WRITE
    , fILE_NOTIFY_CHANGE_SECURITY
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , oPEN_EXISTING
    )
import System.Win32.Types (BOOL, DWORD, HANDLE, LPVOID, LPDWORD, failIfFalse_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import Streamly.Internal.Data.Array.Storable.Foreign (Array)

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Internal/
--
data Config = Config
    { watchRec :: BOOL
    , createFlags :: DWORD
    }

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

-- | Whether a setting is 'On' or 'Off'.
--
-- /Internal/
--
data Toggle = On | Off

setFlag :: DWORD -> Toggle -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            case status of
                On -> createFlags .|. mask
                Off -> createFlags .&. complement mask
    in cfg {createFlags = flags}

-- | Set watch event on directory recursively.
--
-- /default: On/
--
-- /Internal/
--
setRecursiveMode :: BOOL -> Config -> Config
setRecursiveMode rec cfg@Config{} = cfg {watchRec = rec}

-- | Report when a file name is modified.
--
-- /default: On/
--
-- /Internal/
--
setModifiedFileName :: Toggle -> Config -> Config
setModifiedFileName = setFlag fILE_NOTIFY_CHANGE_FILE_NAME

-- | Report when a directory name is modified.
--
-- /default: On/
--
-- /Internal/
--
setRootMoved :: Toggle -> Config -> Config
setRootMoved = setFlag fILE_NOTIFY_CHANGE_DIR_NAME

-- | Report when a file attribute is modified.
--
-- /default: On/
--
-- /Internal/
--
setModifiedAttribute :: Toggle -> Config -> Config
setModifiedAttribute = setFlag fILE_NOTIFY_CHANGE_ATTRIBUTES

-- | Report when a file size is changed.
--
-- /default: On/
--
-- /Internal/
--
setModifiedSize :: Toggle -> Config -> Config
setModifiedSize = setFlag fILE_NOTIFY_CHANGE_SIZE

-- | Report when a file last write time is changed.
--
-- /default: On/
--
-- /Internal/
--
setModifiedLastWrite :: Toggle -> Config -> Config
setModifiedLastWrite = setFlag fILE_NOTIFY_CHANGE_LAST_WRITE

-- | Report when a file Security attributes is changed.
--
-- /default: On/
--
-- /Internal/
--
setModifiedSecurity :: Toggle -> Config -> Config
setModifiedSecurity = setFlag fILE_NOTIFY_CHANGE_SECURITY

-- | Set all events 'On' or 'Off'.
--
-- /default: On/
--
-- /Internal/
--
setAllEvents :: Toggle -> Config -> Config
setAllEvents s =
     setModifiedFileName s
    . setRootMoved s
    . setModifiedAttribute s
    . setModifiedSize s
    . setModifiedLastWrite s
    . setModifiedSecurity s

defaultConfig :: Config
defaultConfig = setAllEvents On $ Config {watchRec = True, createFlags = 0}

getConfigFlag :: Config -> DWORD
getConfigFlag Config{..} = createFlags

getConfigRecMode :: Config -> BOOL
getConfigRecMode Config{..} = watchRec

data Event = Event
    { eventFlags :: DWORD
    , eventRelPath :: String
    , eventRootPath :: String
    , totalBytes :: DWORD
    } deriving (Show, Ord, Eq)

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information
data FILE_NOTIFY_INFORMATION = FILE_NOTIFY_INFORMATION
    { fniNextEntryOffset :: DWORD
    , fniAction :: DWORD
    , fniFileName :: String
    } deriving Show

type LPOVERLAPPED_COMPLETION_ROUTINE =
    FunPtr ((DWORD, DWORD, LPOVERLAPPED) -> IO ())

-- | A handle for a watch.
getWatchHandle :: FilePath -> IO (HANDLE, FilePath)
getWatchHandle dir = do
    h <- createFile dir
        -- Access mode
        fILE_LIST_DIRECTORY
        -- Share mode
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
        -- Security attributes
        Nothing
        -- Create mode, we want to look at an existing directory
        oPEN_EXISTING
        -- File attribute, NOT using OVERLAPPED since we work synchronously
        fILE_FLAG_BACKUP_SEMANTICS
        -- No template file
        Nothing
    return (h, dir)

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw
-- Note that this API uses UTF-16 for file system paths:
-- 1. https://docs.microsoft.com/en-us/windows/win32/intl/unicode-in-the-windows-api
-- 2. https://docs.microsoft.com/en-us/windows/win32/intl/unicode
foreign import ccall safe
    "windows.h ReadDirectoryChangesW" c_ReadDirectoryChangesW ::
           HANDLE
        -> LPVOID
        -> DWORD
        -> BOOL
        -> DWORD
        -> LPDWORD
        -> LPOVERLAPPED
        -> LPOVERLAPPED_COMPLETION_ROUTINE
        -> IO BOOL

readDirectoryChangesW ::
       HANDLE
    -> Ptr FILE_NOTIFY_INFORMATION
    -> DWORD
    -> BOOL
    -> FileNotificationFlag
    -> LPDWORD
    -> IO ()
readDirectoryChangesW h buf bufSize wst f br =
    let res = c_ReadDirectoryChangesW
                    h (castPtr buf) bufSize wst f br nullPtr nullFunPtr
     in failIfFalse_ "ReadDirectoryChangesW" res

peekFNI :: Ptr FILE_NOTIFY_INFORMATION -> IO FILE_NOTIFY_INFORMATION
peekFNI buf = do
    neof <- peekByteOff buf 0
    acti <- peekByteOff buf 4
    fnle <- peekByteOff buf 8
    -- Note: The path is UTF-16 encoded C WChars, peekCWStringLen converts
    -- UTF-16 to UTF-32 Char String
    fnam <- peekCWStringLen
        -- start of array
        (buf `plusPtr` 12,
        -- fnle is the length in *bytes*, and a WCHAR is 2 bytes
        fromEnum (fnle :: DWORD) `div` 2)
    return $ FILE_NOTIFY_INFORMATION neof acti fnam

readChangeEvents ::
    Ptr FILE_NOTIFY_INFORMATION -> String -> DWORD -> IO [Event]
readChangeEvents pfni root bytesRet = do
    fni <- peekFNI pfni
    let entry = Event
            { eventFlags = fniAction fni
            , eventRelPath = fniFileName fni
            , eventRootPath = root
            , totalBytes = bytesRet
            }
        nioff = fromEnum $ fniNextEntryOffset fni
    entries <-
        if nioff == 0
        then return []
        else readChangeEvents (pfni `plusPtr` nioff) root bytesRet
    return $ entry : entries

readDirectoryChanges ::
    String -> HANDLE -> Bool -> FileNotificationFlag -> IO [Event]
readDirectoryChanges root h wst mask = do
    let maxBuf = 63 * 1024
    allocaBytes maxBuf $ \buffer -> do
        alloca $ \bret -> do
            readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
            bytesRet <- peekByteOff bret 0
            readChangeEvents buffer root bytesRet

type FileAction = DWORD

fILE_ACTION_ADDED             :: FileAction
fILE_ACTION_ADDED             =  1

fILE_ACTION_REMOVED           :: FileAction
fILE_ACTION_REMOVED           =  2

fILE_ACTION_MODIFIED          :: FileAction
fILE_ACTION_MODIFIED          =  3

fILE_ACTION_RENAMED_OLD_NAME  :: FileAction
fILE_ACTION_RENAMED_OLD_NAME  =  4

fILE_ACTION_RENAMED_NEW_NAME  :: FileAction
fILE_ACTION_RENAMED_NEW_NAME  =  5

eventStreamAggr :: (HANDLE, FilePath, Config) -> SerialT IO Event
eventStreamAggr (handle, rootPath, cfg) =  do
    let recMode = getConfigRecMode cfg
        flagMasks = getConfigFlag cfg
    S.concatMap S.fromList $ S.repeatM
        $ readDirectoryChanges rootPath handle recMode flagMasks

pathsToHandles ::
    NonEmpty FilePath -> Config -> SerialT IO (HANDLE, FilePath, Config)
pathsToHandles paths cfg = do
    let pathStream = S.fromList (NonEmpty.toList paths)
        st2 = S.mapM getWatchHandle pathStream
    S.map (\(h, f) -> (h, f, cfg)) st2

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

utf8ToString :: Array Word8 -> FilePath
utf8ToString = runIdentity . S.toList . U.decodeUtf8 . A.toStream

utf8ToStringList :: NonEmpty (Array Word8) -> NonEmpty FilePath
utf8ToStringList = NonEmpty.map utf8ToString

-- | Close a Directory handle.
--
-- /Internal/
--
closePathHandleStream :: SerialT IO (HANDLE, FilePath, Config) -> IO ()
closePathHandleStream = S.mapM_ (\(h, _, _) -> closeHandle h)

-- XXX
-- Document the path treatment for Linux/Windows/macOS modules.
-- Remove the utf-8 encoding requirement of paths? It can be encoding agnostic
-- "\" separated bytes, the application can decide what encoding to use.
-- Instead of always using widechar (-W) APIs can we call the underlying APIs
-- based on the configured code page?
-- https://docs.microsoft.com/en-us/windows/uwp/design/globalizing/use-utf8-code-page
--
-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories.  When the path is a directory, the
-- whole directory tree under it is watched recursively. Monitoring starts from
-- the current time onwards.
--
-- /Internal/
--
watchTreesWith ::
       (Config -> Config)
    -> NonEmpty (Array Word8)
    -> SerialT IO Event
watchTreesWith f paths =
     S.bracket before after (S.concatMapWith parallel eventStreamAggr)

    where

    before = return $ pathsToHandles (utf8ToStringList paths) $ f defaultConfig
    after = liftIO . closePathHandleStream

-- | Like 'watchTreesWith' but uses the 'defaultConfig' options.
--
-- @
-- watchTrees = watchTreesWith id
-- @
--
watchTrees :: NonEmpty (Array Word8) -> SerialT IO Event
watchTrees = watchTreesWith id

-- | Start monitoring a list of file system paths for file system events with
-- the supplied recursive mode and configuration. The paths could be files or
-- directories. When recursive mode is True and the path is a directory, the
-- whole directory tree under it is watched recursively.
-- When recursive mode is False and the path is a directory, only the
-- files and directories directly under the watched directory are monitored,
-- contents of subdirectories are not monitored.  Monitoring starts from the
-- current time onwards. The paths are specified as UTF-8 encoded 'Array' of
-- 'Word8'.
--
-- @
-- watch True
--  ('setModifiedAttribute' On . 'setModifiedLastWrite' Off) defaultConfig
--  [Array.fromCString\# "dir"#]
-- @
--
-- /Internal/
--
watch :: Bool -> Config -> NonEmpty (Array Word8) -> SerialT IO Event
watch rec cfg paths =
    case rec of
        True -> watchTreesWith (\_ -> cfg) paths
        False -> watchTreesWith (\_ -> setRecursiveMode False cfg) paths

getFlag :: DWORD -> Event -> Bool
getFlag mask Event{..} = eventFlags == mask

-- XXX Change the type to Array Word8 to make it compatible with other APIs.
--
-- | Get the file system object path for which the event is generated, relative
-- to the watched root. The path is a UTF-8 encoded array of bytes.
--
-- /Internal/
--
getRelPath :: Event -> String
getRelPath Event{..} = eventRelPath

-- XXX Change the type to Array Word8 to make it compatible with other APIs.
--
-- | Get the watch root directory to which this event belongs.
--
-- /Internal/
--
getRoot :: Event -> String
getRoot Event{..} = eventRootPath

getAbsPath :: Event -> String
getAbsPath ev = getRoot ev </> getRelPath ev

-- XXX need to document the exact semantics of these.
--
-- | File/directory created in watched directory.
--
-- /Internal/
--
isCreated :: Event -> Bool
isCreated = getFlag fILE_ACTION_ADDED

-- | File/directory deleted from watched directory.
--
-- /Internal/
--
isDeleted :: Event -> Bool
isDeleted = getFlag fILE_ACTION_REMOVED

-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- /Internal/
--
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag fILE_ACTION_RENAMED_OLD_NAME

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Internal/
--
isMovedTo :: Event -> Bool
isMovedTo = getFlag fILE_ACTION_RENAMED_NEW_NAME

-- XXX This event is generated only for files and not directories?
--
-- | Determine whether the event indicates modification of an object within the
-- monitored path.
--
-- /Internal/
--
isModified :: Event -> Bool
isModified = getFlag fILE_ACTION_MODIFIED

-- |  If the buffer overflows, entire contents of the buffer are discarded,
-- therefore, events are lost.  The user application must scan everything under
-- the watched paths to know the current state.
--
-- /Internal/
--
isOverflow :: Event -> Bool
isOverflow Event{..} = totalBytes == 0

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
        "--------------------------"
    ++ "\nRoot = " ++ show (getRoot ev)
    ++ "\nPath = " ++ show (getRelPath ev)
    ++ "\nFlags " ++ show eventFlags
    ++ showev isOverflow "Overflow"
    ++ showev isCreated "Created"
    ++ showev isDeleted "Deleted"
    ++ showev isModified "Modified"
    ++ showev isMovedFrom "MovedFrom"
    ++ showev isMovedTo "MovedTo"
    ++ "\n"

    where showev f str = if f ev then "\n" ++ str else ""
