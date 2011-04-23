{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module: System.Directory
-- Copyright: 2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Simple 'FilePath'&#x2010;aware wrappers around standard "System.Directory"
-- computations. See the linked documentation for each computation for
-- details on exceptions and operating system interaction.
module System.Directory
	(
	
	-- * File and directory attributes
	  isFile
	, isDirectory
	, fileSize
	, rename
	, modified
	, listDirectory
	
	-- * Creating things
	, createDirectory
	, createTree
	
	-- * Removing things
	, removeFile
	, removeDirectory
	, removeTree
	
	-- * Current working directory
	, getWorkingDirectory
	, setWorkingDirectory
	
	-- * Commonly used paths
	, getHomeDirectory
	, getDesktopDirectory
	, getDocumentsDirectory
	, getAppDataDirectory
	, getAppCacheDirectory
	, getAppConfigDirectory
	) where

import           Prelude hiding (FilePath)

import qualified Control.Exception as Exc
import qualified Data.Text as T
import qualified System.Environment as SE
import           System.FilePath (FilePath, append)
import           System.FilePath.CurrentOS (currentOS)
import qualified System.FilePath.Rules as R
import           System.IO.Error (isDoesNotExistError)

#ifdef CABAL_OS_WINDOWS

import           Data.Bits ((.|.))
import           Data.Time ( UTCTime(..)
                           , fromGregorian
                           , secondsToDiffTime
                           , picosecondsToDiffTime)
import qualified System.Win32 as Win32

#else

import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified System.Posix as Posix

#endif

-- hopefully temporary
import qualified "directory" System.Directory as SD

import           System.FileIO.Internal (encode, decode)

-- | Check if a file exists at the given path.
--
-- See: 'SD.doesFileExist'
isFile :: FilePath -> IO Bool
isFile path = SD.doesFileExist (encode path)

-- | Check if a directory exists at the given path.
--
-- See: 'SD.doesDirectoryExist'
isDirectory :: FilePath -> IO Bool
isDirectory path = SD.doesDirectoryExist (encode path)

-- | Get when the object at a given path was last modified.
modified :: FilePath -> IO UTCTime
modified path = do
#ifdef CABAL_OS_WINDOWS
	info <- withHANDLE path Win32.getFileInformationByHandle
	let ftime = Win32.bhfiLastWriteTime info
	stime <- Win32.fileTimeToSystemTime ftime
	
	let date = fromGregorian
		(fromIntegral (Win32.wYear stime))
		(fromIntegral (Win32.wMonth stime))
		(fromIntegral (Win32.wDay stime))
	
	let seconds = secondsToDiffTime $
		(toInteger (Win32.wHour stime) * 3600) +
		(toInteger (Win32.wMinute stime) * 60) +
		(toInteger (Win32.wSecond stime))
	
	let msecs = picosecondsToDiffTime $
		(toInteger (Win32.wMilliseconds stime) * 1000000000)
	
	return (UTCTime date (seconds + msecs))
#else
	stat <- Posix.getFileStatus (encode path)
	let mtime = Posix.modificationTime stat
	return (posixSecondsToUTCTime (realToFrac mtime))
#endif

-- | Get the size of an object at a given path. For special objects like
-- links or directories, the size is filesystem&#x2010; and
-- platform&#x2010;dependent.
fileSize :: FilePath -> IO Integer
fileSize path = do
#ifdef CABAL_OS_WINDOWS
	info <- withHANDLE path Win32.getFileInformationByHandle
	return (toInteger (Win32.bhfiSize info))
#else
	stat <- Posix.getFileStatus (encode path)
	return (toInteger (Posix.fileSize stat))
#endif

-- | Rename a filesystem object. Some operating systems have restrictions
-- on what objects can be renamed; see linked documentation for details.
--
-- See: 'SD.renameFile' and 'SD.renameDirectory'
rename :: FilePath -> FilePath -> IO ()
rename old new =
	let old' = encode old in
	let new' = encode new in
#ifdef CABAL_OS_WINDOWS
	Win32.moveFileEx old' new' Win32.mOVEFILE_REPLACE_EXISTING
#else
	Posix.rename old' new'
#endif

-- | Create a directory at a given path. The user may choose whether it is
-- an error for a directory to already exist at that path.
--
-- See: 'SD.createDirectory'.
createDirectory :: Bool -- ^ Succeed if the directory already exists
                -> FilePath -> IO ()
createDirectory False path =
	let path' = encode path in
#ifdef CABAL_OS_WINDOWS
	Win32.createDirectory path' Nothing
#else
	Posix.createDirectory path' 0o777
#endif
createDirectory True path = SD.createDirectoryIfMissing False (encode path)

-- | Create a directory at a given path, including any parents which might
-- be missing.
--
-- See: 'SD.createDirectoryIfMissing'
createTree :: FilePath -> IO ()
createTree path = SD.createDirectoryIfMissing True (encode path)

-- | List contents of a directory, excluding @\".\"@ and @\"..\"@.
--
-- See: 'SD.getDirectoryContents'
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = fmap cleanup contents where
	contents = SD.getDirectoryContents (encode path)
	cleanup = map decode . filter (`notElem` [".", ".."])

-- | Remove a file.
--
-- See: 'SD.removeFile'
removeFile :: FilePath -> IO ()
removeFile path =
	let path' = encode path in
#ifdef CABAL_OS_WINDOWS
	Win32.deleteFile path'
#else
	Posix.removeLink path'
#endif

-- | Remove an empty directory.
--
-- See: 'SD.removeDirectory'
removeDirectory :: FilePath -> IO ()
removeDirectory path =
	let path' = encode path in
#ifdef CABAL_OS_WINDOWS
	Win32.removeDirectory path'
#else
	Posix.removeDirectory path'
#endif

-- | Recursively remove a directory tree rooted at the given path.
--
-- See: 'SD.removeDirectoryRecursive'
removeTree :: FilePath -> IO ()
removeTree path = SD.removeDirectoryRecursive (encode path)

-- | Get the current working directory.
--
-- See: 'SD.getCurrentDirectory'
getWorkingDirectory :: IO FilePath
getWorkingDirectory = do
#ifdef CABAL_OS_WINDOWS
	raw <- Win32.getCurrentDirectory
#else
	raw <- Posix.getWorkingDirectory
#endif
	return (decode raw)

-- | Set the current working directory.
--
-- See: 'SD.setCurrentDirectory'
setWorkingDirectory :: FilePath -> IO ()
setWorkingDirectory path =
	let path' = encode path in
#ifdef CABAL_OS_WINDOWS
	Win32.setCurrentDirectory path'
#else
	Posix.changeWorkingDirectory path'
#endif

-- TODO: expose all known exceptions as specific types, for users to catch
-- if need be

-- | Get the user&#x2019;s home directory. This is useful for building paths
-- to more specific directories.
--
-- For directing the user to open or safe a document, use
-- 'getDocumentsDirectory'.
--
-- For data files the user does not explicitly create, such as automatic
-- saves, use 'getAppDataDirectory'.
--
-- See: 'SD.getHomeDirectory'
getHomeDirectory :: IO FilePath
getHomeDirectory = fmap decode SD.getHomeDirectory

-- | Get the user&#x2019;s home directory. This is a good starting point for
-- file dialogs and other user queries. For data files the user does not
-- explicitly create, such as automatic saves, use 'getAppDataDirectory'.
getDesktopDirectory :: IO FilePath
getDesktopDirectory = xdg "XDG_DESKTOP_DIR" Nothing
	(homeSlash "Desktop")

-- | Get the user&#x2019;s documents directory. This is a good place to save
-- user-created files. For data files the user does not explicitly create,
-- such as automatic saves, use 'getAppDataDirectory'.
--
-- See: 'SD.getUserDocumentsDirectory'
getDocumentsDirectory :: IO FilePath
getDocumentsDirectory = xdg "XDG_DOCUMENTS_DIR" Nothing
#ifdef CABAL_OS_WINDOWS
	(fmap decode SD.getUserDocumentsDirectory)
#else
	(homeSlash "Documents")
#endif

-- | Get the user&#x2019;s application data directory, given an application
-- label. This directory is where applications should store data the user did
-- not explicitly create, such as databases and automatic saves.
--
-- See: 'SD.getAppUserDataDirectory'
getAppDataDirectory :: T.Text -> IO FilePath
getAppDataDirectory label = xdg "XDG_DATA_HOME" (Just label)
#ifdef CABAL_OS_WINDOWS
	(fmap decode (SD.getAppUserDataDirectory ""))
#else
	(homeSlash ".local/share")
#endif

-- | Get the user&#x2019;s application cache directory, given an application
-- label. This directory is where applications should store caches, which
-- might be large and can be safely deleted.
getAppCacheDirectory :: T.Text -> IO FilePath
getAppCacheDirectory label = xdg "XDG_CACHE_HOME" (Just label)
#ifdef CABAL_OS_WINDOWS
	(homeSlash "Local Settings\\Cache")
#else
	(homeSlash ".cache")
#endif

-- | Get the user&#x2019;s application configuration directory, given an
-- application label. This directory is where applications should store their
-- configurations and settings.
getAppConfigDirectory :: T.Text -> IO FilePath
getAppConfigDirectory label = xdg "XDG_CONFIG_HOME" (Just label)
#ifdef CABAL_OS_WINDOWS
	(homeSlash "Local Settings")
#else
	(homeSlash ".config")
#endif

homeSlash :: String -> IO FilePath
homeSlash path = do
	home <- getHomeDirectory
	return (append home (decode path))

getenv :: String -> IO (Maybe String)
getenv key = Exc.catch
	(fmap Just (SE.getEnv key))
	(\e -> if isDoesNotExistError e
		then return Nothing
		else Exc.throwIO e)

xdg :: String -> Maybe T.Text -> IO FilePath -> IO FilePath
xdg envkey label fallback = do
	env <- getenv envkey
	dir <- case env of
		Just var -> return (decode var)
		Nothing -> fallback
	return $ case label of
		Just text -> append dir (R.fromText currentOS text)
		Nothing -> dir

#ifdef CABAL_OS_WINDOWS
withHANDLE :: FilePath -> (Win32.HANDLE -> IO a) -> IO a
withHANDLE path = Exc.bracket open close where
	open = Win32.createFile
		(encode path)
		Win32.gENERIC_READ
		(Win32.fILE_SHARE_READ .|. Win32.fILE_SHARE_WRITE)
		Nothing
		Win32.oPEN_EXISTING
		0
		Nothing
	close = Win32.closeHandle
#endif
