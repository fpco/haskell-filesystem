-- |
-- Module: System.Directory
-- Copyright: 2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Deprecated alias for "Filesystem".
module System.Directory
	(
	
	-- * Generic operations
	  isFile
	, isDirectory
	, rename
	, canonicalizePath
	, listDirectory
	
	-- * Creating things
	, createDirectory
	, createTree
	
	-- * Removing things
	, removeFile
	, removeDirectory
	, removeTree
	
	-- * Current working directory
	, getWorkingDirectory
	, setWorkingDirectory
	
	-- * Commonly used paths
	, getHomeDirectory
	, getDesktopDirectory
	, getDocumentsDirectory
	, getAppDataDirectory
	, getAppCacheDirectory
	, getAppConfigDirectory
	) where

import           Filesystem
