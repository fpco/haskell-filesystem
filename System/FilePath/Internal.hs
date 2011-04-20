{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: System.FilePath.Internal
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
module System.FilePath.Internal where

import Prelude hiding (FilePath)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Data (Data)
import           Data.Typeable (Typeable)

-------------------------------------------------------------------------------
-- File Paths
-------------------------------------------------------------------------------

type Directory = B.ByteString
type Basename = B.ByteString
type Extension = B.ByteString

data Root
	= RootPosix
	| RootWindowsVolume Char
	| RootWindowsCurrentVolume
	deriving (Eq, Ord, Data, Typeable)

data FilePath = FilePath
	{ pathRoot :: Maybe Root
	, pathDirectories :: [Directory]
	, pathBasename :: Maybe Basename
	, pathExtensions :: [Extension]
	}
	deriving (Eq, Ord, Data, Typeable)

-- | A file path with no root, directory, or filename
empty :: FilePath
empty = FilePath Nothing [] Nothing []

dot :: B.ByteString
dot = B8.pack "."

dots :: B.ByteString
dots = B8.pack ".."

filenameBytes :: FilePath -> B.ByteString
filenameBytes p = B.append name ext where
	name = maybe B.empty id (pathBasename p)
	ext = case pathExtensions p of
		[] -> B.empty
		exts -> B.intercalate dot (B.empty:exts)

-------------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------------

data Rules = Rules
	{ rulesName :: String
	, toByteChunks :: FilePath -> [B.ByteString]
	
	-- | Parse a strict 'B.ByteString', such as  those received from
	-- OS libraries, into a 'FilePath'.
	, fromBytes :: B.ByteString -> FilePath
	
	-- | Check if a 'FilePath' is valid; it must not contain
	-- any illegal characters, and must have a root appropriate to the
	-- current 'Rules'.
	, valid :: FilePath -> Bool
	
	-- | Split a search path, such as @$PATH@ or @$PYTHONPATH@, into
	-- a list of 'FilePath's.
	, splitSearchPath :: B.ByteString -> [FilePath]
	}

instance Show Rules where
	showsPrec d r = showParen (d > 10) $
		showString "Rules " . shows (rulesName r)
