-- |
-- Module: System.File
-- Copyright: 2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Deprecated alias for "Filesystem".
module System.File
	( Filesystem.Handle
	, Filesystem.IOMode(..)
	
	-- * File operations
	, copyFile
	
	-- * File information
	, getModified
	, getSize
	
	-- * Binary files
	, openFile
	, withFile
	, Filesystem.readFile
	, Filesystem.writeFile
	, Filesystem.appendFile
	
	-- * Text files
	, openTextFile
	, withTextFile
	, readTextFile
	, writeTextFile
	, appendTextFile
	) where

import           Filesystem
