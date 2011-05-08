{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module: System.File
-- Copyright: 2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Simple 'FilePath'&#x2010;aware wrappers around standard "System.IO"
-- computations. See the linked documentation for each computation for
-- details on exceptions and operating system interaction.
module System.File
	( IO.Handle
	, IO.IOMode(..)
	
	-- * File operations
	, copyFile
	
	-- * File information
	, getModified
	, getSize
	
	-- * Binary files
	, openFile
	, withFile
	, readFile
	, writeFile
	, appendFile
	
	-- * Text files
	, openTextFile
	, withTextFile
	, readTextFile
	, writeTextFile
	, appendTextFile
	) where

import           Prelude hiding (FilePath, readFile, writeFile, appendFile)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import           System.FilePath (FilePath)
import           System.FileIO.Internal (encode)

#ifdef CABAL_OS_WINDOWS

import qualified Control.Exception as Exc
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

import qualified "directory" System.Directory as SD

-- | Copy a file to a new entry in the filesystem. If a file already exists
-- at the new location, it will be replaced.
--
-- See: 'SD.copyFile'
--
-- Since: 0.1.1
copyFile :: FilePath -- ^ Old location
         -> FilePath -- ^ New location
         -> IO ()
copyFile old new = SD.copyFile (encode old) (encode new)

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
	stat <- Posix.getFileStatus (encode path)
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
	stat <- Posix.getFileStatus (encode path)
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
openFile path = IO.openBinaryFile (encode path)

-- | Open a file in binary mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- See: 'IO.withBinaryFile'
withFile :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFile path = IO.withBinaryFile (encode path)

-- | Read in the entire contents of a binary file.
--
-- See: 'B.readFile'
readFile :: FilePath -> IO B.ByteString
readFile path = B.readFile (encode path)

-- | Replace the entire contents of a binary file with the provided
-- 'B.ByteString'.
--
-- See: 'B.writeFile'
writeFile :: FilePath -> B.ByteString -> IO ()
writeFile path = B.writeFile (encode path)

-- | Append a 'B.ByteString' to a file. If the file does not exist, it will
-- be created.
--
-- See: 'B.appendFile'
appendFile :: FilePath -> B.ByteString -> IO ()
appendFile path = B.appendFile (encode path)

-- | Open a file in text mode, and return an open 'Handle'. The 'Handle'
-- should be 'IO.hClose'd when it is no longer needed.
--
-- 'withTextFile' is easier to use, because it will handle the
-- 'Handle'&#x2019;s lifetime automatically.
--
-- See: 'IO.openFile'
openTextFile :: FilePath -> IO.IOMode -> IO IO.Handle
openTextFile path = IO.openFile (encode path)

-- | Open a file in text mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- See: 'IO.withFile'
withTextFile :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withTextFile path = IO.withFile (encode path)

-- | Read in the entire contents of a text file.
--
-- See: 'T.readFile'
readTextFile :: FilePath -> IO T.Text
readTextFile path = T.readFile (encode path)

-- | Replace the entire contents of a text file with the provided
-- 'T.Text'.
--
-- See: 'T.writeFile'
writeTextFile :: FilePath -> T.Text -> IO ()
writeTextFile path = T.writeFile (encode path)

-- | Append 'T.Text' to a file. If the file does not exist, it will
-- be created.
--
-- See: 'T.appendFile'
appendTextFile :: FilePath -> T.Text -> IO ()
appendTextFile path = T.appendFile (encode path)

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
