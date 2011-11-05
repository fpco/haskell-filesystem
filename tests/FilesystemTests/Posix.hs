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
import           Data.Text (Text)
import           Foreign
import           Foreign.C
import           Test.Chell

import           Filesystem
import           Filesystem.Path.CurrentOS

import           FilesystemTests.Util (assertionsWithTemp, todo)

test_Posix :: Suite
test_Posix = suite "posix"
	[ suite "isFile"
		[ test_IsFile "ascii"
			(decode "test.txt")
		, test_IsFile "utf8"
			(decode "\xC2\xA1\xC2\xA2.txt")
		, test_IsFile "iso8859"
			(decode "\xA1\xA2\xA3.txt")
		]
	, suite "isDirectory"
		[ test_IsDirectory "ascii"
			(decode "test.d")
		, test_IsDirectory "utf8"
			(decode "\xC2\xA1\xC2\xA2.d")
		, test_IsDirectory "iso8859"
			(decode "\xA1\xA2\xA3.d")
		]
	, suite "rename"
		[ test_Rename "ascii"
			(decode "old_test.txt")
			(decode "new_test.txt")
		, test_Rename "utf8"
			(decode "old_\xC2\xA1\xC2\xA2.txt")
			(decode "new_\xC2\xA1\xC2\xA2.txt")
		, test_Rename "iso8859"
			(decode "old_\xA1\xA2\xA3.txt")
			(decode "new_\xA1\xA2\xA3.txt")
		]
	, suite "canonicalizePath"
		[ test_CanonicalizePath "ascii"
			(decode "test-a.txt")
			(decode "test-b.txt")
		, test_CanonicalizePath "utf8"
			(decode "\xC2\xA1\xC2\xA2-a.txt")
			(decode "\xC2\xA1\xC2\xA2-b.txt")
		, test_CanonicalizePath "iso8859"
			(decode "\xA1\xA2\xA3-a.txt")
			(decode "\xA1\xA2\xA3-b.txt")
		]
	, todo "createDirectory"
	, todo "createTree"
	, test_ListDirectory
	, todo "removeFile"
	, todo "removeDirectory"
	, todo "removeTree"
	, todo "getWorkingDirectory"
	, todo "setWorkingDirectory"
	, todo "getHomeDirectory"
	, todo "getDesktopDirectory"
	, todo "getDocumentsDirectory"
	, todo "getAppDataDirectory"
	, todo "getAppCacheDirectory"
	, todo "getAppConfigDirectory"
	, todo "copyFile"
	, todo "getModified"
	, todo "getSize"
	, todo "openFile"
	, todo "withFile"
	, todo "readFile"
	, todo "writeFile"
	, todo "appendFile"
	, todo "openTextFile"
	, todo "withTextFile"
	, todo "readTextFile"
	, todo "writeTextFile"
	, todo "appendTextFile"
	]

test_IsFile :: Text -> FilePath -> Suite
test_IsFile test_name file_name = assertionsWithTemp test_name $ \dir -> do
	let path = dir </> file_name
	
	before <- liftIO $ Filesystem.isFile path
	$expect (not before)
	
	touch_ffi path "contents\n"
	
	after <- liftIO $ Filesystem.isFile path
	$expect after

test_IsDirectory :: Text -> FilePath -> Suite
test_IsDirectory test_name dir_name = assertionsWithTemp test_name $ \dir -> do
	let path = dir </> dir_name
	
	before <- liftIO $ Filesystem.isDirectory path
	$expect (not before)
	
	mkdir_ffi path
	
	after <- liftIO $ Filesystem.isDirectory path
	$expect after

test_Rename :: Text -> FilePath -> FilePath -> Suite
test_Rename test_name old_name new_name = assertionsWithTemp test_name $ \dir -> do
	let old_path = dir </> old_name
	let new_path = dir </> new_name
	
	touch_ffi old_path ""
	
	old_before <- liftIO $ Filesystem.isFile old_path
	new_before <- liftIO $ Filesystem.isFile new_path
	$expect old_before
	$expect (not new_before)
	
	liftIO $ Filesystem.rename old_path new_path
	
	old_after <- liftIO $ Filesystem.isFile old_path
	new_after <- liftIO $ Filesystem.isFile new_path
	$expect (not old_after)
	$expect new_after

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

test_CanonicalizePath :: Text -> FilePath -> FilePath -> Suite
test_CanonicalizePath test_name src_name dst_name = assertionsWithTemp test_name $ \dir -> do
	let src_path = dir </> src_name
	let subdir = dir </> "subdir"
	let dst_path = subdir </> dst_name
	
	mkdir_ffi subdir
	touch_ffi dst_path ""
	symlink_ffi dst_path src_path
	
	canonicalized <- liftIO $ Filesystem.canonicalizePath src_path
	$expect $ equal canonicalized dst_path

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

-- | Create a symlink using the raw POSIX API, via FFI
symlink_ffi :: FilePath -> FilePath -> Assertions ()
symlink_ffi dst src  = do
	ret <- liftIO $
		Data.ByteString.useAsCString (encode dst) $ \dst_p ->
		Data.ByteString.useAsCString (encode src) $ \src_p ->
		c_symlink dst_p src_p
	
	$assert (ret == 0)

foreign import ccall unsafe "fopen"
	c_fopen :: CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "fclose"
	c_fclose :: Ptr () -> IO CInt

foreign import ccall unsafe "fwrite"
	c_fwrite :: CString -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall unsafe "mkdir"
	c_mkdir :: CString -> CInt -> IO CInt

foreign import ccall unsafe "symlink"
	c_symlink :: CString -> CString -> IO CInt
