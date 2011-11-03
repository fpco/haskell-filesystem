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
import           Data.Text (Text, unpack)
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Chell

import           Filesystem.Path.CurrentOS (FilePath, decodeString)

assertionsWithTemp :: Text -> (FilePath -> Assertions a) -> Suite
assertionsWithTemp name io = test (Test name impl) where
	impl options = withSystemTempDirectory ("tests." ++ unpack name ++ ".") $ \dir -> do
		let dirPath = decodeString dir
		case suiteTests (assertions name (io dirPath)) of
			[(Test _ io')] -> io' options
			_ -> error "assertionsWithTemp: use in place of 'assertions' only."

todo :: Text -> Suite
todo name = skipIf True (assertions name (return ()))
