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
