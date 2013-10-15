{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module FilesystemTests.Util
	( assertionsWithTemp
	, todo
	) where

import           Prelude hiding (FilePath)

import           Control.Exception (finally)
import           Data.Text (Text, unpack)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Chell

import           Filesystem (removeTree)
import           Filesystem.Path.CurrentOS (FilePath, decodeString)

assertionsWithTemp :: Text -> (FilePath -> Assertions a) -> Suite
assertionsWithTemp name io = test (Test name impl) where
	impl options = withTempDir name $ \dir -> do
		case suiteTests (assertions name (io dir)) of
			[(Test _ io')] -> io' options
			_ -> error "assertionsWithTemp: use in place of 'assertions' only."

withTempDir :: Text -> (FilePath -> IO a) -> IO a
withTempDir name io = withSystemTempDirectory
	("tests." ++ unpack name ++ ".")
	(\dir ->
		let dir' = decodeString dir in
		finally (io dir') (removeTree dir'))

todo :: Text -> Suite
todo name = skipIf True (assertions name (return ()))
