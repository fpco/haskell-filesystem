{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Filesystem
-- Copyright: 2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Simple 'FilePath'&#x2010;aware wrappers around standard "System.IO"
-- computations. See the linked documentation for each computation for
-- details on exceptions and operating system interaction.
module Filesystem
	( IO.Handle
	, IO.IOMode(..)
	, rename
	
	-- * Files
	, isFile
	, getModified
	, getSize
	, copyFile
	, removeFile
	
	-- ** Binary files
	, openFile
	, withFile
	, readFile
	, writeFile
	, appendFile
	
	-- ** Text files
	, openTextFile
	, withTextFile
	, readTextFile
	, writeTextFile
	, appendTextFile
	
	-- * Directories
	, isDirectory
	, canonicalizePath
	, listDirectory
	
	-- ** Creating
	, createDirectory
	, createTree
	
	-- ** Removing
	, removeDirectory
	, removeTree
	
	-- ** Current working directory
	, getWorkingDirectory
	, setWorkingDirectory
	
	-- ** Commonly used paths
	, getHomeDirectory
	, getDesktopDirectory
	, getDocumentsDirectory
	, getAppDataDirectory
	, getAppCacheDirectory
	, getAppConfigDirectory
	) where

import           Prelude hiding (FilePath, readFile, writeFile, appendFile)

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.C (CString, withCString, peekCString)
import qualified System.Environment as SE

import           Filesystem.Path (FilePath, append)
import           Filesystem.Path.CurrentOS (currentOS, decodeString, encodeString)
import qualified Filesystem.Path.Rules as R

import qualified System.IO as IO
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
import qualified System.Posix.Error as Posix

#endif

import qualified System.Directory as SD

-- | Check if a file exists at the given path.
--
-- See: 'SD.doesFileExist'
isFile :: FilePath -> IO Bool
isFile path = SD.doesFileExist (encodeString path)

-- | Check if a directory exists at the given path.
--
-- See: 'SD.doesDirectoryExist'
isDirectory :: FilePath -> IO Bool
isDirectory path = SD.doesDirectoryExist (encodeString path)

-- | Rename a filesystem object. Some operating systems have restrictions
-- on what objects can be renamed; see linked documentation for details.
--
-- See: 'SD.renameFile' and 'SD.renameDirectory'
rename :: FilePath -> FilePath -> IO ()
rename old new =
	let old' = encodeString old in
	let new' = encodeString new in
#ifdef CABAL_OS_WINDOWS
	Win32.moveFileEx old' new' Win32.mOVEFILE_REPLACE_EXISTING
#else
	Posix.rename old' new'
#endif

-- Resolve symlinks and \"..\" path elements to return a canonical path.
-- It is intended that two paths referring to the same object will always
-- resolve to the same canonical path.
--
-- Note that on many operating systems, it is impossible to guarantee that
-- two paths to the same file will resolve to the same canonical path.
--
-- See: 'SD.canonicalizePath'
--
-- Since: 0.1.1
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = fmap decodeString $ do
	let path' = encodeString path
#ifdef CABAL_OS_WINDOWS
#if MIN_VERSION_Win32(2,2,1)
	Win32.getFullPathName path'
#else
	Win32.withTString path' $ \c_name -> do
		Win32.try "getFullPathName" (\buf len ->
			c_GetFullPathNameW c_name len buf nullPtr) 512
#endif
#else
	withCString path' $ \cPath -> do
		cOut <- Posix.throwErrnoPathIfNull "canonicalizePath" path' (c_realpath cPath nullPtr)
		peekCString cOut
#endif

#ifdef CABAL_OS_WINDOWS
#if MIN_VERSION_Win32(2,2,1)
#else
foreign import stdcall unsafe "GetFullPathNameW"
	c_GetFullPathNameW :: Win32.LPCTSTR -> Win32.DWORD -> Win32.LPTSTR -> Ptr Win32.LPTSTR -> IO Win32.DWORD
#endif
#endif

#ifndef CABAL_OS_WINDOWS
foreign import ccall unsafe "realpath"
	c_realpath :: CString -> CString -> IO CString
#endif

-- | Create a directory at a given path. The user may choose whether it is
-- an error for a directory to already exist at that path.
--
-- See: 'SD.createDirectory'.
createDirectory :: Bool -- ^ Succeed if the directory already exists
                -> FilePath -> IO ()
createDirectory False path =
	let path' = encodeString path in
#ifdef CABAL_OS_WINDOWS
	Win32.createDirectory path' Nothing
#else
	Posix.createDirectory path' 0o777
#endif
createDirectory True path = SD.createDirectoryIfMissing False (encodeString path)

-- | Create a directory at a given path, including any parents which might
-- be missing.
--
-- See: 'SD.createDirectoryIfMissing'
createTree :: FilePath -> IO ()
createTree path = SD.createDirectoryIfMissing True (encodeString path)

-- | List contents of a directory, excluding @\".\"@ and @\"..\"@. Each
-- returned 'FilePath' includes the path of the directory.
--
-- See: 'SD.getDirectoryContents'
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = fmap cleanup contents where
	contents = SD.getDirectoryContents (encodeString path)
	cleanup = map (append path) . map decodeString . filter (`notElem` [".", ".."])

-- | Remove a file.
--
-- See: 'SD.removeFile'
removeFile :: FilePath -> IO ()
removeFile path =
	let path' = encodeString path in
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
	let path' = encodeString path in
#ifdef CABAL_OS_WINDOWS
	Win32.removeDirectory path'
#else
	Posix.removeDirectory path'
#endif

-- | Recursively remove a directory tree rooted at the given path.
--
-- See: 'SD.removeDirectoryRecursive'
removeTree :: FilePath -> IO ()
removeTree path = SD.removeDirectoryRecursive (encodeString path)

-- | Get the current working directory.
--
-- See: 'SD.getCurrentDirectory'
getWorkingDirectory :: IO FilePath
getWorkingDirectory = fmap decodeString $ do
#ifdef CABAL_OS_WINDOWS
#if MIN_VERSION_Win32(2,2,1)
	Win32.getCurrentDirectory
#else
	Win32.try "getCurrentDirectory" (flip c_GetCurrentDirectoryW) 512
#endif
#else
	Posix.getWorkingDirectory
#endif

#ifdef CABAL_OS_WINDOWS
#if MIN_VERSION_Win32(2,2,1)
#else
foreign import stdcall unsafe "GetCurrentDirectoryW"
	c_GetCurrentDirectoryW :: Win32.DWORD -> Win32.LPTSTR -> IO Win32.UINT
#endif
#endif

-- | Set the current working directory.
--
-- See: 'SD.setCurrentDirectory'
setWorkingDirectory :: FilePath -> IO ()
setWorkingDirectory path =
	let path' = encodeString path in
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
getHomeDirectory = fmap decodeString SD.getHomeDirectory

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
	(fmap decodeString SD.getUserDocumentsDirectory)
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
	(fmap decodeString (SD.getAppUserDataDirectory ""))
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
	return (append home (decodeString path))

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
		Just var -> return (decodeString var)
		Nothing -> fallback
	return $ case label of
		Just text -> append dir (R.fromText currentOS text)
		Nothing -> dir

-- | Copy a file to a new entry in the filesystem. If a file already exists
-- at the new location, it will be replaced.
--
-- See: 'SD.copyFile'
--
-- Since: 0.1.1
copyFile :: FilePath -- ^ Old location
         -> FilePath -- ^ New location
         -> IO ()
copyFile old new = SD.copyFile (encodeString old) (encodeString new)

-- | Get when the object at a given path was last modified.
--
-- Since: 0.2
getModified :: FilePath -> IO UTCTime
getModified path = do
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
	stat <- Posix.getFileStatus (encodeString path)
	let mtime = Posix.modificationTime stat
	return (posixSecondsToUTCTime (realToFrac mtime))
#endif

-- | Get the size of an object at a given path. For special objects like
-- links or directories, the size is filesystem&#x2010; and
-- platform&#x2010;dependent.
--
-- Since: 0.2
getSize :: FilePath -> IO Integer
getSize path = do
#ifdef CABAL_OS_WINDOWS
	info <- withHANDLE path Win32.getFileInformationByHandle
	return (toInteger (Win32.bhfiSize info))
#else
	stat <- Posix.getFileStatus (encodeString path)
	return (toInteger (Posix.fileSize stat))
#endif

-- | Open a file in binary mode, and return an open 'Handle'. The 'Handle'
-- should be 'IO.hClose'd when it is no longer needed.
--
-- 'withFile' is easier to use, because it will handle the 'Handle'&#x2019;s
-- lifetime automatically.
--
-- See: 'IO.openBinaryFile'
openFile :: FilePath -> IO.IOMode -> IO IO.Handle
openFile path = IO.openBinaryFile (encodeString path)

-- | Open a file in binary mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- See: 'IO.withBinaryFile'
withFile :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFile path = IO.withBinaryFile (encodeString path)

-- | Read in the entire contents of a binary file.
--
-- See: 'B.readFile'
readFile :: FilePath -> IO B.ByteString
readFile path = B.readFile (encodeString path)

-- | Replace the entire contents of a binary file with the provided
-- 'B.ByteString'.
--
-- See: 'B.writeFile'
writeFile :: FilePath -> B.ByteString -> IO ()
writeFile path = B.writeFile (encodeString path)

-- | Append a 'B.ByteString' to a file. If the file does not exist, it will
-- be created.
--
-- See: 'B.appendFile'
appendFile :: FilePath -> B.ByteString -> IO ()
appendFile path = B.appendFile (encodeString path)

-- | Open a file in text mode, and return an open 'Handle'. The 'Handle'
-- should be 'IO.hClose'd when it is no longer needed.
--
-- 'withTextFile' is easier to use, because it will handle the
-- 'Handle'&#x2019;s lifetime automatically.
--
-- See: 'IO.openFile'
openTextFile :: FilePath -> IO.IOMode -> IO IO.Handle
openTextFile path = IO.openFile (encodeString path)

-- | Open a file in text mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- See: 'IO.withFile'
withTextFile :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withTextFile path = IO.withFile (encodeString path)

-- | Read in the entire contents of a text file.
--
-- See: 'T.readFile'
readTextFile :: FilePath -> IO T.Text
readTextFile path = T.readFile (encodeString path)

-- | Replace the entire contents of a text file with the provided
-- 'T.Text'.
--
-- See: 'T.writeFile'
writeTextFile :: FilePath -> T.Text -> IO ()
writeTextFile path = T.writeFile (encodeString path)

-- | Append 'T.Text' to a file. If the file does not exist, it will
-- be created.
--
-- See: 'T.appendFile'
appendTextFile :: FilePath -> T.Text -> IO ()
appendTextFile path = T.appendFile (encodeString path)

#ifdef CABAL_OS_WINDOWS
withHANDLE :: FilePath -> (Win32.HANDLE -> IO a) -> IO a
withHANDLE path = Exc.bracket open close where
	open = Win32.createFile
		(encodeString path)
		Win32.gENERIC_READ
		(Win32.fILE_SHARE_READ .|. Win32.fILE_SHARE_WRITE)
		Nothing
		Win32.oPEN_EXISTING
		0
		Nothing
	close = Win32.closeHandle
#endif
