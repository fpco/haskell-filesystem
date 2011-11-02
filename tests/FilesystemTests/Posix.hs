{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module FilesystemTests.Posix
	( test_Posix
	) where

import           Prelude hiding (FilePath)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString
import           Foreign
import           Foreign.C
import           Test.Chell

import           Filesystem
import           Filesystem.Path.CurrentOS

import           FilesystemTests.Util (assertionsWithTemp)

test_Posix :: Suite
test_Posix = suite "posix"
	[ suite "isFile"
		[ test_IsFileAscii
		, test_IsFileUtf8
		, test_IsFileIso8859
		]
	, suite "isDirectory"
		[ test_IsDirectoryAscii
		, test_IsDirectoryUtf8
		, test_IsDirectoryIso8859
		]
	, suite "rename"
		[
		]
	, suite "canonicalizePath"
		[
		]
	, suite "createDirectory"
		[
		]
	, suite "createTree"
		[
		]
	, test_ListDirectory
	, suite "removeFile"
		[
		]
	, suite "removeDirectory"
		[
		]
	, suite "removeTree"
		[
		]
	, suite "getWorkingDirectory"
		[
		]
	, suite "setWorkingDirectory"
		[
		]
	, suite "getHomeDirectory"
		[
		]
	, suite "getDesktopDirectory"
		[
		]
	, suite "getDocumentsDirectory"
		[
		]
	, suite "getAppDataDirectory"
		[
		]
	, suite "getAppCacheDirectory"
		[
		]
	, suite "getAppConfigDirectory"
		[
		]
	, suite "copyFile"
		[
		]
	, suite "getModified"
		[
		]
	, suite "getSize"
		[
		]
	, suite "openFile"
		[
		]
	, suite "withFile"
		[
		]
	, suite "readFile"
		[
		]
	, suite "writeFile"
		[
		]
	, suite "appendFile"
		[
		]
	, suite "openTextFile"
		[
		]
	, suite "withTextFile"
		[
		]
	, suite "readTextFile"
		[
		]
	, suite "writeTextFile"
		[
		]
	, suite "appendTextFile"
		[
		]
	]

test_IsFileAscii :: Suite
test_IsFileAscii = assertionsWithTemp "ascii" $ \dir -> do
	let path = dir </> decode "test.txt"
	touch_ffi path "contents\n"
	
	x <- liftIO $ Filesystem.isFile path
	$expect x

test_IsFileUtf8 :: Suite
test_IsFileUtf8 = assertionsWithTemp "utf8" $ \dir -> do
	let path = dir </> decode "\xC2\xA1\xC2\xA2.txt"
	touch_ffi path "contents\n"
	
	x <- liftIO $ Filesystem.isFile path
	$expect x

test_IsFileIso8859 :: Suite
test_IsFileIso8859 = assertionsWithTemp "iso8859" $ \dir -> do
	let path = dir </> decode "\xA1\xA2\xA3.txt"
	touch_ffi path "contents\n"
	
	x <- liftIO $ Filesystem.isFile path
	$expect x

test_IsDirectoryAscii :: Suite
test_IsDirectoryAscii = assertionsWithTemp "ascii" $ \dir -> do
	let path = dir </> decode "test.d"
	mkdir_ffi path
	
	x <- liftIO $ Filesystem.isDirectory path
	$expect x

test_IsDirectoryUtf8 :: Suite
test_IsDirectoryUtf8 = assertionsWithTemp "utf8" $ \dir -> do
	let path = dir </> decode "\xC2\xA1\xC2\xA2.d"
	mkdir_ffi path
	
	x <- liftIO $ Filesystem.isDirectory path
	$expect x

test_IsDirectoryIso8859 :: Suite
test_IsDirectoryIso8859 = assertionsWithTemp "iso8859" $ \dir -> do
	let path = dir </> decode "\xA1\xA2\xA3.d"
	mkdir_ffi path
	
	x <- liftIO $ Filesystem.isDirectory path
	$expect x

test_ListDirectory :: Suite
test_ListDirectory = assertionsWithTemp "listDirectory" $ \dir -> do
	let paths =
		[ dir </> decode "test.txt"
		, dir </> decode "\xC2\xA1\xC2\xA2.txt"
		, dir </> decode "\xA1\xA2\xA3.txt"
		]
	forM_ paths (\path -> touch_ffi path "")
	
	names <- liftIO $ Filesystem.listDirectory dir
	$expect $ sameItems paths names

-- | Create a file using the raw POSIX API, via FFI
touch_ffi :: FilePath -> Data.ByteString.ByteString -> Assertions ()
touch_ffi path contents = do
	let pathBytes = encode path
	fp <- liftIO $ Data.ByteString.useAsCString pathBytes $ \path_cstr ->
		Foreign.C.withCString "wb" $ \mode_cstr ->
		c_fopen path_cstr mode_cstr
	
	$assert (fp /= nullPtr)
	
	_ <- liftIO $ Data.ByteString.useAsCStringLen contents $ \(buf, len) ->
		c_fwrite buf 1 (fromIntegral len) fp
	
	_ <- liftIO $ c_fclose fp
	return ()

-- | Create a directory using the raw POSIX API, via FFI
mkdir_ffi :: FilePath -> Assertions ()
mkdir_ffi path = do
	let pathBytes = encode path
	ret <- liftIO $ Data.ByteString.useAsCString pathBytes $ \path_cstr ->
		c_mkdir path_cstr 0o700
	
	$assert (ret == 0)

foreign import ccall unsafe "fopen"
	c_fopen :: CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "fclose"
	c_fclose :: Ptr () -> IO CInt

foreign import ccall unsafe "fwrite"
	c_fwrite :: CString -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall unsafe "mkdir"
	c_mkdir :: CString -> CInt -> IO CInt
