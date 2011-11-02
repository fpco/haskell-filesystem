{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module FilesystemTests.Windows
	( test_Windows
	) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Chell

import           Filesystem
import           Filesystem.Path.CurrentOS

test_Windows :: Suite
test_Windows = suite "windows" []

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
