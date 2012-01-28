{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}

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
import           Foreign.C (CInt, CString)
import qualified Foreign.C.Error as CError
import qualified System.Environment as SE

#if MIN_VERSION_system_filepath(0,4,0)
import           Filesystem.Path (FilePath, append)
import           Filesystem.Path.CurrentOS (currentOS, encodeString, decodeString)
import qualified Filesystem.Path.Rules as R
#else
import           System.FilePath (FilePath, append)
import           System.FilePath.CurrentOS (currentOS, encodeString, decodeString)
import qualified System.FilePath.Rules as R
#endif

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

-- hopefully temporary
import qualified "directory" System.Directory as SD

-- | Check if a file exists at the given path.
--
-- See: 'SD.doesFileExist'
isFile :: FilePath -> IO Bool
#ifdef CABAL_OS_WINDOWS
isFile path = SD.doesFileExist (encodeString path)
#else
isFile path = Exc.catch
	(do
		stat <- withFd "isFile" path Posix.getFdStatus
		return (not (Posix.isDirectory stat)))
	((\_ -> return False) :: Exc.IOException -> IO Bool)
#endif

-- | Check if a directory exists at the given path.
--
-- See: 'SD.doesDirectoryExist'
isDirectory :: FilePath -> IO Bool
#ifdef CABAL_OS_WINDOWS
isDirectory path = SD.doesDirectoryExist (encodeString path)
#else
isDirectory path = Exc.catch
	(do
		stat <- withFd "isFile" path Posix.getFdStatus
		return (Posix.isDirectory stat))
	((\_ -> return False) :: Exc.IOException -> IO Bool)
#endif

-- | Rename a filesystem object. Some operating systems have restrictions
-- on what objects can be renamed; see linked documentation for details.
--
-- See: 'SD.renameFile' and 'SD.renameDirectory'
rename :: FilePath -> FilePath -> IO ()
rename old new =
#ifdef CABAL_OS_WINDOWS
	let old' = encodeString old in
	let new' = encodeString new in
	Win32.moveFileEx old' new' Win32.mOVEFILE_REPLACE_EXISTING
#else
	withFilePath old $ \old' ->
	withFilePath new $ \new' ->
	throwErrnoPathIfMinus1_ "rename" old (c_rename old' new')

foreign import ccall unsafe "rename"
	c_rename :: CString -> CString -> IO CInt

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
canonicalizePath path =
	let path' = encodeString path in
#ifdef CABAL_OS_WINDOWS
	fmap decodeString $
#if MIN_VERSION_Win32(2,2,1)
	Win32.getFullPathName path'
#else
	Win32.withTString path' $ \c_name -> do
		Win32.try "getFullPathName" (\buf len ->
			c_GetFullPathNameW c_name len buf nullPtr) 512
#endif
#else
	withFilePath path $ \cPath -> do
		cOut <- Posix.throwErrnoPathIfNull "canonicalizePath" path' (c_realpath cPath nullPtr)
		bytes <- B.packCString cOut
		c_free cOut
		return (R.decode R.posix bytes)
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
createDirectory succeedIfExists path =
#ifdef CABAL_OS_WINDOWS
	let path' = encodeString path in
	if succeedIfExists
		then SD.createDirectoryIfMissing False path'
		else Win32.createDirectory path' Nothing
#else
	withFilePath path $ \cPath ->
	throwErrnoPathIfMinus1Retry_ "createDirectory" path $
	c_mkdir cPath 0o777 (if succeedIfExists then 1 else 0)

foreign import ccall unsafe "hssystemfileio_mkdir"
	c_mkdir :: CString -> CInt -> CInt -> IO CInt
#endif

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
#ifdef CABAL_OS_WINDOWS
listDirectory root = fmap cleanup contents where
	contents = SD.getDirectoryContents (encodeString root)
	cleanup = map (append root) . map decodeString . filter (`notElem` [".", ".."])
#else
listDirectory root = Exc.bracket alloc free list where
	alloc = do
		dirent <- c_alloc_dirent
		dir <- openDir root
		return (dirent, dir)
	free (dirent, dir) = do
		c_free_dirent dirent
		closeDir dir
	list (dirent, dir) = loop where
		loop = do
			next <- readDir dir dirent
			case next of
				Nothing -> return []
				Just bytes | ignore bytes -> loop
				Just bytes -> do
					let name = append root (R.decode R.posix bytes)
					names <- loop
					return (name:names)

ignore :: B.ByteString -> Bool
ignore = ignore' where
	dot = B.pack [46]
	dotdot = B.pack [46, 46]
	ignore' b = b == dot || b == dotdot

data Dir = Dir FilePath (Ptr ())

openDir :: FilePath -> IO Dir
openDir root = withFilePath root $ \cRoot -> do
	p <- throwErrnoPathIfNullRetry "listDirectory" root (c_opendir cRoot)
	return (Dir root p)

closeDir :: Dir -> IO ()
closeDir (Dir _ p) = CError.throwErrnoIfMinus1Retry_ "listDirectory" (c_closedir p)

readDir :: Dir -> Ptr () -> IO (Maybe B.ByteString)
readDir (Dir _ p) dirent = do
	rc <- CError.throwErrnoIfMinus1 "listDirectory" (c_readdir p dirent)
	if rc == 0
		then do
			bytes <- c_dirent_name dirent >>= B.packCString
			return (Just bytes)
		else return Nothing

foreign import ccall unsafe "opendir"
	c_opendir :: CString -> IO (Ptr ())

foreign import ccall unsafe "opendir"
	c_closedir :: Ptr () -> IO CInt

foreign import ccall unsafe "hssystemfileio_alloc_dirent"
	c_alloc_dirent :: IO (Ptr ())

foreign import ccall unsafe "hssystemfileio_free_dirent"
	c_free_dirent :: Ptr () -> IO ()

foreign import ccall unsafe "hssystemfileio_readdir"
	c_readdir :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "hssystemfileio_dirent_name"
	c_dirent_name :: Ptr () -> IO CString

#endif

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
withFile path mode = Exc.bracket (openFile path mode) IO.hClose

-- | Read in the entire contents of a binary file.
--
-- See: 'B.readFile'
readFile :: FilePath -> IO B.ByteString
readFile path = withFile path IO.ReadMode
	(\h -> IO.hFileSize h >>= B.hGet h . fromIntegral)

-- | Replace the entire contents of a binary file with the provided
-- 'B.ByteString'.
--
-- See: 'B.writeFile'
writeFile :: FilePath -> B.ByteString -> IO ()
writeFile path bytes = withFile path IO.WriteMode
	(\h -> B.hPut h bytes)

-- | Append a 'B.ByteString' to a file. If the file does not exist, it will
-- be created.
--
-- See: 'B.appendFile'
appendFile :: FilePath -> B.ByteString -> IO ()
appendFile path bytes = withFile path IO.AppendMode
	(\h -> B.hPut h bytes)

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
withTextFile path mode = Exc.bracket (openTextFile path mode) IO.hClose

-- | Read in the entire contents of a text file.
--
-- See: 'T.readFile'
readTextFile :: FilePath -> IO T.Text
readTextFile path = openTextFile path IO.ReadMode >>= T.hGetContents

-- | Replace the entire contents of a text file with the provided
-- 'T.Text'.
--
-- See: 'T.writeFile'
writeTextFile :: FilePath -> T.Text -> IO ()
writeTextFile path text = withTextFile path IO.WriteMode
	(\h -> T.hPutStr h text)

-- | Append 'T.Text' to a file. If the file does not exist, it will
-- be created.
--
-- See: 'T.appendFile'
appendTextFile :: FilePath -> T.Text -> IO ()
appendTextFile path text = withTextFile path IO.AppendMode
	(\h -> T.hPutStr h text)

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

#else

withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath path = B.useAsCString (R.encode R.posix path)

throwErrnoPathIfMinus1 :: String -> FilePath -> IO CInt -> IO CInt
throwErrnoPathIfMinus1 loc path = CError.throwErrnoPathIfMinus1 loc (encodeString path)

throwErrnoPathIfMinus1_ :: String -> FilePath -> IO CInt -> IO ()
throwErrnoPathIfMinus1_ loc path = CError.throwErrnoPathIfMinus1_ loc (encodeString path)

throwErrnoPathIfNullRetry :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry = throwErrnoPathIfRetry (== nullPtr)

throwErrnoPathIfMinus1Retry_ :: String -> FilePath -> IO CInt -> IO ()
throwErrnoPathIfMinus1Retry_ = throwErrnoPathIfRetry_ (== -1)

throwErrnoPathIfRetry :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIfRetry failed loc path io = loop where
	loop = do
		a <- io
		if failed a
			then do
				errno <- CError.getErrno
				if errno == CError.eINTR
					then loop
					else CError.throwErrnoPath loc (encodeString path)
			else return a

throwErrnoPathIfRetry_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIfRetry_ failed loc path io = do
	_ <- throwErrnoPathIfRetry failed loc path io
	return ()

withFd :: String -> FilePath -> (Posix.Fd -> IO a) -> IO a
withFd fnName path = Exc.bracket open close where
	open = withFilePath path $ \cpath -> do
		fd <- throwErrnoPathIfMinus1 fnName path (c_open cpath 0)
		return (Posix.Fd fd)
	close = Posix.closeFd

foreign import ccall unsafe "open"
	c_open :: CString -> CInt -> IO CInt

foreign import ccall unsafe "free"
	c_free :: Ptr a -> IO ()

#endif
