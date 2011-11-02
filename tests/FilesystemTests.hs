{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main
	( tests
	, main
	) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8
import qualified Data.ByteString
import qualified Data.Text
import           Foreign
import           Foreign.C
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Chell

import           Filesystem
import           Filesystem.Path.CurrentOS

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
tests = [test_Unicode]

test_Unicode :: Suite
test_Unicode = suite "unicode"
	[ suite "posix"
		[ test_UnicodePosixValid
		, test_UnicodePosixInvalid
		]
	, test_UnicodeWindows
	]

#ifdef CABAL_OS_WINDOWS

test_UnicodePosixValid :: Suite
test_UnicodePosixValid = skipIf True (assertions "valid" (return ()))

test_UnicodePosixInvalid :: Suite
test_UnicodePosixInvalid = skipIf True (assertions "invalid" (return ()))

test_UnicodeWindows :: Suite
test_UnicodeWindows = assertions "windows" $ do
	let text = "\12354\946\1076\119070.txt"
	(tempDir, paths, contents) <- liftIO $ withSystemTempDirectory "tests." $ \dir -> do
		let dirPath = decodeString dir
		let filePath = dirPath </> fromText text
		writeTextFile filePath "contents"
		
		paths <- listDirectory dirPath
		contents <- readTextFile filePath
		return (dir, paths, contents)
	
	$assert (not (Prelude.null paths))
	$expect $ equal (length paths) 1
	let path = paths !! 0
	
	let textPath = Data.Text.concat
		[ Data.Text.pack tempDir, "\\", text]
	$expect $ equal (toText path) (Right textPath)
	
	$expect $ equal contents "contents"

#else

test_UnicodePosixValid :: Suite
test_UnicodePosixValid = assertions "valid" $ do
	let text = "\12354\946\1076\119070.txt"
	(tempDir, paths, contents, fp) <- liftIO $ withSystemTempDirectory "tests." $ \dir -> do
		let dirPath = decodeString dir
		let filePath = dirPath </> fromText text
		writeTextFile filePath "contents"
		
		-- check that it was saved in the correct encoding
		fp <- Data.ByteString.useAsCString (encode filePath) $ \path_cstr ->
			Foreign.C.withCString "rb" $ \mode_cstr ->
			c_fopen path_cstr mode_cstr
		when (fp /= nullPtr) $ do
			_ <- c_fclose fp
			return ()
		
		paths <- listDirectory dirPath
		contents <- readTextFile filePath
		return (dir, paths, contents, fp)
	
	$assert (not (Prelude.null paths))
	$expect $ equal (length paths) 1
	let path = paths !! 0
	
	let textPath = Data.Text.concat
		[ Data.Text.pack tempDir, "/", text]
	$expect $ equal (toText path) (Right textPath)
	
	$expect $ equal contents "contents"
	$expect $ notEqual fp nullPtr

test_UnicodePosixInvalid :: Suite
test_UnicodePosixInvalid = assertions "invalid" $ do
	let bytes = "\xA1\xA2.txt"
	(tempDir, paths, contents) <- liftIO $ withSystemTempDirectory "tests." $ \dir -> do
		let dirPath = decodeString dir
		let filePath = dirPath </> decode bytes
		
		-- use posix API to make sure GHC doesn't corrupt the path.
		fp <- Data.ByteString.useAsCString (encode filePath) $ \path_cstr ->
			Foreign.C.withCString "wb" $ \mode_cstr ->
			c_fopen path_cstr mode_cstr
		
		_ <- Foreign.C.withCStringLen "contents" $ \(buf, len) ->
			c_fwrite buf 1 (fromIntegral len) fp
		
		_ <- c_fclose fp
		
		paths <- listDirectory dirPath
		contents <- readTextFile filePath
		return (dir, paths, contents)
	
	$assert (not (Prelude.null paths))
	$expect $ equal (length paths) 1
	let path = paths !! 0
	
	let text = Data.Text.pack (Data.ByteString.Char8.unpack bytes)
	let textPath = Data.Text.concat
		[ Data.Text.pack tempDir, "/", text]
	$expect $ equal (toText path) (Left textPath)
	
	$expect $ equal contents "contents"

test_UnicodeWindows :: Suite
test_UnicodeWindows = skipIf True (assertions "windows" (return ()))

foreign import ccall unsafe "fopen"
	c_fopen :: CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "fclose"
	c_fclose :: Ptr () -> IO CInt

foreign import ccall unsafe "fwrite"
	c_fwrite :: CString -> CSize -> CSize -> Ptr () -> IO CSize

#endif
