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
import           FilesystemTests.Windows (suite_Windows)
#else
import           FilesystemTests.Posix (suite_Posix)
#endif

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
#ifdef CABAL_OS_WINDOWS
tests = [suite_Windows]
#else
tests = [suite_Posix]
#endif
