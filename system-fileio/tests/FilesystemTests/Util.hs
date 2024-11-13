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
import           System.IO.Temp (withSystemTempDirectory)
import           Test.Chell

import           Filesystem (removeTree)
import           Filesystem.Path.CurrentOS (FilePath, decodeString)

assertionsWithTemp :: String -> (FilePath -> Assertions a) -> Test
assertionsWithTemp name io = test name impl where
  impl options = withTempDir name $ \dir -> do
    runTest (assertions name (io dir)) options

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name io = withSystemTempDirectory
  ("tests." ++ name ++ ".")
  (\dir ->
    let dir' = decodeString dir in
    finally (io dir') (removeTree dir'))

todo :: String -> Test
todo name = skipIf True (assertions name (return ()))
