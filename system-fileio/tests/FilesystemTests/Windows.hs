{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module FilesystemTests.Windows
  ( suite_Windows
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Test.Chell

import           Filesystem
import           Filesystem.Path.CurrentOS

import           FilesystemTests.Util (assertionsWithTemp, todo)

suite_Windows :: Suite
suite_Windows = suite "windows"
  [ todo "isFile"
  , todo "isDirectory"
  , todo "rename"
  , todo "canonicalizePath"
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

test_ListDirectory :: Test
test_ListDirectory = assertionsWithTemp "listDirectory" $ \dir -> do
  let
    paths =
      [ dir </> decode "test.txt"
      , dir </> decode "\12354\946\1076\119070.txt"
      , dir </> decode "\xA1\xA2\xA3.txt"
      ]

  liftIO $ forM_ paths (\path -> writeTextFile path "")

  names <- liftIO $ Filesystem.listDirectory dir
  $expect $ sameItems paths names
