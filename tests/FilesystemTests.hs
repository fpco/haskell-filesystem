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

import           Test.Chell

#ifdef CABAL_OS_WINDOWS
import           FilesystemTests.Windows (test_Windows)
#else
import           FilesystemTests.Posix (test_Posix)
#endif

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
#ifdef CABAL_OS_WINDOWS
tests = [test_Windows]
#else
tests = [test_Posix]
#endif
