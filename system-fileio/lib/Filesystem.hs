{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Filesystem
-- Copyright: 2011-2012 John Millikin <jmillikin@gmail.com>
-- License: MIT
--
-- Maintainer: John Millikin <jmillikin@gmail.com>
-- Portability: portable
--
-- Simple 'FilePath'&#8208;aware wrappers around standard "System.IO"
-- computations. These wrappers are designed to work as similarly as
-- possible across various versions of GHC.
--
-- In particular, they do not require POSIX file paths to be valid strings,
-- and can therefore open paths regardless of the current locale encoding.
module Filesystem
  (
  -- * Exports from System.IO
    IO.Handle
  , IO.IOMode(..)

  -- * Files
  , isFile
  , getModified
  , getSize
  , copyFile
  , copyFileContent
  , copyPermissions
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

  -- ** Creating directories
  , createDirectory
  , createTree

  -- ** Removing directories
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

  -- * Other
  , rename
  ) where

import           Prelude hiding (FilePath, readFile, writeFile, appendFile)

import qualified Control.Exception as Exc
import           Control.Monad (forM_, unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.C (CInt(..), CString, withCAString)
import qualified Foreign.C.Error as CError
import qualified System.Environment as SE

import           Filesystem.Path (FilePath, append)
import qualified Filesystem.Path as Path
import           Filesystem.Path.CurrentOS (currentOS, encodeString, decodeString)
import qualified Filesystem.Path.Rules as R

import qualified System.IO as IO
import           System.IO.Error (IOError)

#ifdef CABAL_OS_WINDOWS

import           Data.Bits ((.|.))
import           Data.Time ( UTCTime(..)
                           , fromGregorian
                           , secondsToDiffTime
                           , picosecondsToDiffTime)
import           Foreign.C (CWString, withCWString)
import qualified System.Win32 as Win32
import           System.IO.Error (isDoesNotExistError)
import qualified System.Directory as SD

#else

import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified System.Posix as Posix
import qualified System.Posix.Error as Posix
#if MIN_VERSION_unix(2,5,1)
import qualified System.Posix.Files.ByteString
#endif

#endif

#ifdef SYSTEMFILEIO_LOCAL_OPEN_FILE
import           Data.Bits ((.|.))
import           GHC.IO.Handle.FD (mkHandleFromFD)
import           GHC.IO.FD (mkFD)
import qualified GHC.IO.Device
import qualified System.Posix.Internals
#endif

-- | Check if a file exists at the given path.
--
-- Any non&#8208;directory object, including devices and pipes, are
-- considered to be files. Symbolic links are resolved to their targets
-- before checking their type.
--
-- This computation does not throw exceptions.
isFile :: FilePath -> IO Bool
#ifdef CABAL_OS_WINDOWS
isFile path = SD.doesFileExist (encodeString path)
#else
isFile path = Exc.catch
  (do
    stat <- posixStat "isFile" path
    return (not (Posix.isDirectory stat)))
  ((\_ -> return False) :: IOError -> IO Bool)
#endif

-- | Check if a directory exists at the given path.
--
-- Symbolic links are resolved to their targets before checking their type.
--
-- This computation does not throw exceptions.
isDirectory :: FilePath -> IO Bool
#ifdef CABAL_OS_WINDOWS
isDirectory path = SD.doesDirectoryExist (encodeString path)
#else
isDirectory path = Exc.catch
  (do
    stat <- posixStat "isDirectory" path
    return (Posix.isDirectory stat))
  ((\_ -> return False) :: IOError -> IO Bool)
#endif

-- | Rename a filesystem object.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
rename :: FilePath -> FilePath -> IO ()
rename old new =
#ifdef CABAL_OS_WINDOWS
  let old' = encodeString old in
  let new' = encodeString new in
#if MIN_VERSION_Win32(2,6,0)
  Win32.moveFileEx old' (Just new') Win32.mOVEFILE_REPLACE_EXISTING
#else
  Win32.moveFileEx old' new' Win32.mOVEFILE_REPLACE_EXISTING
#endif
#else
  withFilePath old $ \old' ->
  withFilePath new $ \new' ->
  throwErrnoPathIfMinus1_ "rename" old (c_rename old' new')

foreign import ccall unsafe "rename"
  c_rename :: CString -> CString -> IO CInt

#endif

-- | Resolve symlinks and \"..\" path elements to return a canonical path.
-- It is intended that two paths referring to the same object will always
-- resolve to the same canonical path.
--
-- Note that on many operating systems, it is impossible to guarantee that
-- two paths to the same file will resolve to the same canonical path.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
--
-- Since: 0.1.1
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path =
  fmap (preserveFinalSlash path) $
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

preserveFinalSlash :: FilePath -> FilePath -> FilePath
preserveFinalSlash orig out = if Path.null (Path.filename orig)
  then Path.append out Path.empty
  else out

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
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
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
  throwErrnoPathIfMinus1Retry_ "createDirectory" path $ if succeedIfExists
    then mkdirIfMissing path cPath 0o777
    else c_mkdir cPath 0o777

mkdirIfMissing :: FilePath -> CString -> CInt -> IO CInt
mkdirIfMissing path cPath mode = do
  rc <- c_mkdir cPath mode
  if rc == -1
    then do
      errno <- CError.getErrno
      if errno == CError.eEXIST
        then do
          dirExists <- isDirectory path
          if dirExists
            then return 0
            else return rc
        else return rc
    else return rc

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CInt -> IO CInt
#endif

-- | Create a directory at a given path, including any parents which might
-- be missing.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
createTree :: FilePath -> IO ()
#ifdef CABAL_OS_WINDOWS
createTree path = SD.createDirectoryIfMissing True (encodeString path)
#else
createTree path = do
  let parent = Path.parent path
  parentExists <- isDirectory parent
  unless parentExists (createTree parent)
  withFilePath path $ \cPath ->
    throwErrnoPathIfMinus1Retry_ "createTree" path (mkdirIfMissing path cPath 0o777)
#endif

-- | List objects in a directory, excluding @\".\"@ and @\"..\"@. Each
-- returned 'FilePath' includes the path of the directory. Entries are not
-- sorted.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
listDirectory :: FilePath -> IO [FilePath]
#ifdef CABAL_OS_WINDOWS
listDirectory root = fmap cleanup contents where
  contents = SD.getDirectoryContents (encodeString root)
  cleanup = map (append root) . map decodeString . filter (`notElem` [".", ".."])
#else
listDirectory root = Exc.bracket alloc free list where
  alloc = do
    dir <- openDir root
    let Dir _ dirp = dir
    dirent <- c_alloc_dirent dirp
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
  rc <- CError.throwErrnoIfMinus1Retry "listDirectory" (c_readdir p dirent)
  if rc == 0
    then do
      bytes <- c_dirent_name dirent >>= B.packCString
      return (Just bytes)
    else return Nothing

foreign import ccall unsafe "opendir"
  c_opendir :: CString -> IO (Ptr ())

foreign import ccall unsafe "closedir"
  c_closedir :: Ptr () -> IO CInt

foreign import ccall unsafe "hssystemfileio_alloc_dirent"
  c_alloc_dirent :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "hssystemfileio_free_dirent"
  c_free_dirent :: Ptr () -> IO ()

foreign import ccall unsafe "hssystemfileio_readdir"
  c_readdir :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "hssystemfileio_dirent_name"
  c_dirent_name :: Ptr () -> IO CString

#endif

-- | Remove a file. This will fail if the file does not exist.
--
-- This computation cannot remove directories. For that, use 'removeDirectory'
-- or 'removeTree'.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
removeFile :: FilePath -> IO ()
removeFile path =
#ifdef CABAL_OS_WINDOWS
  Win32.deleteFile (encodeString path)
#else
  withFilePath path $ \cPath ->
  throwErrnoPathIfMinus1_ "removeFile" path (c_unlink cPath)

foreign import ccall unsafe "unlink"
  c_unlink :: CString -> IO CInt
#endif

-- | Remove an empty directory.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
removeDirectory :: FilePath -> IO ()
removeDirectory path =
#ifdef CABAL_OS_WINDOWS
  Win32.removeDirectory (encodeString path)
#else
  withFilePath path $ \cPath ->
  throwErrnoPathIfMinus1Retry_ "removeDirectory" path (c_rmdir cPath)

foreign import ccall unsafe "rmdir"
  c_rmdir :: CString -> IO CInt
#endif

-- | Recursively remove a directory tree rooted at the given path.
--
-- This computation does not follow symlinks. If the tree contains symlinks,
-- the links themselves will be removed, but not the objects they point to.
--
-- If the root path is a symlink, then it will be treated as if it were a
-- regular directory.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
removeTree :: FilePath -> IO ()
#ifdef CABAL_OS_WINDOWS
removeTree root = SD.removeDirectoryRecursive (encodeString root)
#else
removeTree root = do
  items <- listDirectory root
  forM_ items $ \item -> Exc.catch
    (removeFile item)
    (\exc -> do
      isDir <- isRealDir item
      if isDir
        then removeTree item
        else Exc.throwIO (exc :: IOError))
  removeDirectory root

-- Check whether a path is a directory, and not just a symlink to a directory.
--
-- This is used in 'removeTree' to prevent recursing into symlinks if the link
-- itself cannot be deleted.
isRealDir :: FilePath -> IO Bool
isRealDir path = withFilePath path $ \cPath -> do
  rc <- throwErrnoPathIfMinus1Retry "removeTree" path (c_isrealdir cPath)
  return (rc == 1)

foreign import ccall unsafe "hssystemfileio_isrealdir"
  c_isrealdir :: CString -> IO CInt

#endif

-- | Get the current working directory.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
getWorkingDirectory :: IO FilePath
getWorkingDirectory = do
#ifdef CABAL_OS_WINDOWS
#if MIN_VERSION_Win32(2,2,1)
  fmap decodeString Win32.getCurrentDirectory
#else
  fmap decodeString (Win32.try "getWorkingDirectory" (flip c_GetCurrentDirectoryW) 512)
#endif
#else
  buf <- CError.throwErrnoIfNull "getWorkingDirectory" c_getcwd
  bytes <- B.packCString buf
  c_free buf
  return (R.decode R.posix bytes)

foreign import ccall unsafe "hssystemfileio_getcwd"
  c_getcwd :: IO CString

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
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
setWorkingDirectory :: FilePath -> IO ()
setWorkingDirectory path =
#ifdef CABAL_OS_WINDOWS
  Win32.setCurrentDirectory (encodeString path)
#else
  withFilePath path $ \cPath ->
  throwErrnoPathIfMinus1Retry_ "setWorkingDirectory" path (c_chdir cPath)

foreign import ccall unsafe "chdir"
  c_chdir :: CString -> IO CInt

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
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
getHomeDirectory :: IO FilePath
#ifdef CABAL_OS_WINDOWS
getHomeDirectory = fmap decodeString SD.getHomeDirectory
#else
getHomeDirectory = do
  path <- getenv "HOME"
  case path of
    Just p -> return p
    Nothing -> do
      -- use getEnv to throw the right exception type
      fmap decodeString (SE.getEnv "HOME")
#endif

-- | Get the user&#x2019;s desktop directory. This is a good starting point for
-- file dialogs and other user queries. For data files the user does not
-- explicitly create, such as automatic saves, use 'getAppDataDirectory'.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
getDesktopDirectory :: IO FilePath
getDesktopDirectory = xdg "XDG_DESKTOP_DIR" Nothing
  (homeSlash "Desktop")

-- | Get the user&#x2019;s documents directory. This is a good place to save
-- user&#8208;created files. For data files the user does not explicitly
-- create, such as automatic saves, use 'getAppDataDirectory'.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
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
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
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
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
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
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
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

getenv :: String -> IO (Maybe FilePath)
#ifdef CABAL_OS_WINDOWS
getenv key = Exc.catch
  (fmap (Just . decodeString) (SE.getEnv key))
  (\e -> if isDoesNotExistError e
    then return Nothing
    else Exc.throwIO e)
#else
getenv key = withCAString key $ \cKey -> do
  ret <- c_getenv cKey
  if ret == nullPtr
    then return Nothing
    else do
      bytes <- B.packCString ret
      return (Just (R.decode R.posix bytes))

foreign import ccall unsafe "getenv"
  c_getenv :: CString -> IO CString

#endif

xdg :: String -> Maybe T.Text -> IO FilePath -> IO FilePath
xdg envkey label fallback = do
  env <- getenv envkey
  dir <- case env of
    Just var -> return var
    Nothing -> fallback
  return $ case label of
    Just text -> append dir (R.fromText currentOS text)
    Nothing -> dir

-- | Copy the content of a file to a new entry in the filesystem. If a
-- file already exists at the new location, it will be replaced. Copying
-- a file is not atomic.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
--
-- Since: 0.2.4 / 0.3.4
copyFileContent :: FilePath -- ^ Old location
                -> FilePath -- ^ New location
                -> IO ()
copyFileContent oldPath newPath =
  withFile oldPath IO.ReadMode $ \old ->
  withFile newPath IO.WriteMode $ \new ->
  BL.hGetContents old >>= BL.hPut new

-- | Copy the permissions from one path to another. Both paths must already
-- exist.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
--
-- Since: 0.2.4 / 0.3.4
copyPermissions :: FilePath -- ^ Old location
                -> FilePath -- ^ New location
                -> IO ()
copyPermissions oldPath newPath =
  withFilePath oldPath $ \cOldPath ->
  withFilePath newPath $ \cNewPath ->
  CError.throwErrnoIfMinus1Retry_ "copyPermissions" $
  c_copy_permissions cOldPath cNewPath

#ifdef CABAL_OS_WINDOWS

foreign import ccall unsafe "hssystemfileio_copy_permissions"
  c_copy_permissions :: CWString -> CWString -> IO CInt

#else

foreign import ccall unsafe "hssystemfileio_copy_permissions"
  c_copy_permissions :: CString -> CString -> IO CInt

#endif

-- | Copy the content and permissions of a file to a new entry in the
-- filesystem. If a file already exists at the new location, it will be
-- replaced. Copying a file is not atomic.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
--
-- Since: 0.1.1
copyFile :: FilePath -- ^ Old location
         -> FilePath -- ^ New location
         -> IO ()
copyFile oldPath newPath = do
  copyFileContent oldPath newPath
  Exc.catch
    (copyPermissions oldPath newPath)
    ((\_ -> return ()) :: IOError -> IO ())

-- | Get when the object at a given path was last modified.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
--
-- Since: 0.2
getModified :: FilePath -> IO UTCTime
getModified path = do
#ifdef CABAL_OS_WINDOWS
  info <- withHANDLE path Win32.getFileInformationByHandle
  let ftime = Win32.bhfiLastWriteTime info
  stime <- Win32.fileTimeToSystemTime ftime
  let
    date = fromGregorian
      (fromIntegral (Win32.wYear stime))
      (fromIntegral (Win32.wMonth stime))
      (fromIntegral (Win32.wDay stime))
    seconds = secondsToDiffTime $
      (toInteger (Win32.wHour stime) * 3600) +
      (toInteger (Win32.wMinute stime) * 60) +
      (toInteger (Win32.wSecond stime))
    msecs = picosecondsToDiffTime $
      (toInteger (Win32.wMilliseconds stime) * 1000000000)
  return (UTCTime date (seconds + msecs))
#else
  stat <- posixStat "getModified" path
  let mtime = Posix.modificationTime stat
  return (posixSecondsToUTCTime (realToFrac mtime))
#endif

-- | Get the size of an object at a given path. For special objects like
-- links or directories, the size is filesystem&#8208; and
-- platform&#8208;dependent.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
--
-- Since: 0.2
getSize :: FilePath -> IO Integer
getSize path = do
#ifdef CABAL_OS_WINDOWS
  info <- withHANDLE path Win32.getFileInformationByHandle
  return (toInteger (Win32.bhfiSize info))
#else
  stat <- posixStat "getSize" path
  return (toInteger (Posix.fileSize stat))
#endif

-- | Open a file in binary mode, and return an open 'Handle'. The 'Handle'
-- should be closed with 'IO.hClose' when it is no longer needed.
--
-- 'withFile' is easier to use, because it will handle the 'Handle'&#x2019;s
-- lifetime automatically.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
openFile :: FilePath -> IO.IOMode -> IO IO.Handle
#ifdef SYSTEMFILEIO_LOCAL_OPEN_FILE
openFile path mode = openFile' "openFile" path mode Nothing
#else
openFile path = IO.openBinaryFile (encodeString path)
#endif

-- | Open a file in binary mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
withFile :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFile path mode = Exc.bracket (openFile path mode) IO.hClose

-- | Read in the entire content of a binary file.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
readFile :: FilePath -> IO B.ByteString
readFile path = withFile path IO.ReadMode
  (\h -> IO.hFileSize h >>= B.hGet h . fromIntegral)

-- | Replace the entire content of a binary file with the provided
-- 'B.ByteString'.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
writeFile :: FilePath -> B.ByteString -> IO ()
writeFile path bytes = withFile path IO.WriteMode
  (\h -> B.hPut h bytes)

-- | Append a 'B.ByteString' to a file. If the file does not exist, it will
-- be created.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
appendFile :: FilePath -> B.ByteString -> IO ()
appendFile path bytes = withFile path IO.AppendMode
  (\h -> B.hPut h bytes)

-- | Open a file in text mode, and return an open 'Handle'. The 'Handle'
-- should be closed with 'IO.hClose' when it is no longer needed.
--
-- 'withTextFile' is easier to use, because it will handle the
-- 'Handle'&#x2019;s lifetime automatically.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
openTextFile :: FilePath -> IO.IOMode -> IO IO.Handle
#ifdef SYSTEMFILEIO_LOCAL_OPEN_FILE
openTextFile path mode = openFile' "openTextFile" path mode (Just IO.localeEncoding)
#else
openTextFile path = IO.openFile (encodeString path)
#endif

-- | Open a file in text mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
withTextFile :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withTextFile path mode = Exc.bracket (openTextFile path mode) IO.hClose

-- | Read in the entire content of a text file.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
readTextFile :: FilePath -> IO T.Text
readTextFile path = openTextFile path IO.ReadMode >>= T.hGetContents

-- | Replace the entire content of a text file with the provided
-- 'T.Text'.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
writeTextFile :: FilePath -> T.Text -> IO ()
writeTextFile path text = withTextFile path IO.WriteMode
  (\h -> T.hPutStr h text)

-- | Append 'T.Text' to a file. If the file does not exist, it will
-- be created.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
appendTextFile :: FilePath -> T.Text -> IO ()
appendTextFile path text = withTextFile path IO.AppendMode
  (\h -> T.hPutStr h text)

#ifdef SYSTEMFILEIO_LOCAL_OPEN_FILE
-- | Copied from GHC.IO.FD.openFile
openFile' :: String -> FilePath -> IO.IOMode -> (Maybe IO.TextEncoding) -> IO IO.Handle
openFile' loc path mode codec = open where
  sys_c_open = System.Posix.Internals.c_open
  sys_c_close = System.Posix.Internals.c_close
  flags = iomodeFlags mode
  open = withFilePath path $ \cPath -> do
    c_fd <- throwErrnoPathIfMinus1Retry loc path (sys_c_open cPath flags 0o666)
    (fd, fd_type) <- Exc.onException
      (mkFD c_fd mode Nothing False True)
      (sys_c_close c_fd)
    when (mode == IO.WriteMode && fd_type == GHC.IO.Device.RegularFile) $ do
      GHC.IO.Device.setSize fd 0
    Exc.onException
      (mkHandleFromFD fd fd_type (encodeString path) mode False codec)
      (GHC.IO.Device.close fd)

iomodeFlags :: IO.IOMode -> CInt
iomodeFlags mode = cased .|. commonFlags where
  cased = case mode of
    IO.ReadMode -> flagsR
#ifdef mingw32_HOST_OS
    IO.WriteMode -> flagsW .|. System.Posix.Internals.o_TRUNC
#else
    IO.WriteMode -> flagsW
#endif
    IO.ReadWriteMode -> flagsRW
    IO.AppendMode -> flagsA

  flagsR  = System.Posix.Internals.o_RDONLY
  flagsW  = outputFlags .|. System.Posix.Internals.o_WRONLY
  flagsRW = outputFlags .|. System.Posix.Internals.o_RDWR
  flagsA  = flagsW      .|. System.Posix.Internals.o_APPEND

  commonFlags = System.Posix.Internals.o_NOCTTY .|.
                System.Posix.Internals.o_NONBLOCK
  outputFlags = System.Posix.Internals.o_CREAT

#endif

#ifdef CABAL_OS_WINDOWS

-- Only for accessing file or directory metadata.
-- See issue #8.
withHANDLE :: FilePath -> (Win32.HANDLE -> IO a) -> IO a
withHANDLE path = Exc.bracket open close where
  open = Win32.createFile
    (encodeString path)
    0
    (Win32.fILE_SHARE_READ .|. Win32.fILE_SHARE_WRITE)
    Nothing
    Win32.oPEN_EXISTING
    Win32.fILE_FLAG_BACKUP_SEMANTICS
    Nothing
  close = Win32.closeHandle

withFilePath :: FilePath -> (CWString -> IO a) -> IO a
withFilePath path = withCWString (encodeString path)

#else

withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath path = B.useAsCString (R.encode R.posix path)

throwErrnoPathIfMinus1 :: String -> FilePath -> IO CInt -> IO CInt
throwErrnoPathIfMinus1 loc path = CError.throwErrnoPathIfMinus1 loc (encodeString path)

throwErrnoPathIfMinus1_ :: String -> FilePath -> IO CInt -> IO ()
throwErrnoPathIfMinus1_ loc path = CError.throwErrnoPathIfMinus1_ loc (encodeString path)

throwErrnoPathIfNullRetry :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry = throwErrnoPathIfRetry (== nullPtr)

throwErrnoPathIfMinus1Retry :: String -> FilePath -> IO CInt -> IO CInt
throwErrnoPathIfMinus1Retry = throwErrnoPathIfRetry (== -1)

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

posixStat :: String -> FilePath -> IO Posix.FileStatus
#if MIN_VERSION_unix(2,5,1)
posixStat _ path = System.Posix.Files.ByteString.getFileStatus (R.encode R.posix path)
#else
posixStat loc path = withFd loc path Posix.getFdStatus

withFd :: String -> FilePath -> (Posix.Fd -> IO a) -> IO a
withFd fnName path = Exc.bracket open close where
  open = withFilePath path $ \cpath -> do
    fd <- throwErrnoPathIfMinus1 fnName path (c_open_nonblocking cpath 0)
    return (Posix.Fd fd)
  close = Posix.closeFd

foreign import ccall unsafe "hssystemfileio_open_nonblocking"
  c_open_nonblocking :: CString -> CInt -> IO CInt

#endif

foreign import ccall unsafe "free"
  c_free :: Ptr a -> IO ()

#endif
