-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath.Internal
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module System.FilePath.Internal where

import Prelude hiding (FilePath)
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-------------------------------------------------------------------------------
-- File Paths
-------------------------------------------------------------------------------

type Component = B.ByteString
type Basename = B.ByteString
type Extension = B.ByteString

data Root
	= RootPosix
	| RootWindowsVolume Char
	| RootWindowsCurrentVolume
	deriving (Eq, Data, Typeable)

data FilePath = FilePath
	{ pathRoot :: (Maybe Root)
	, pathComponents :: [Component]
	, pathBasename :: (Maybe Basename)
	, pathExtensions :: [Extension]
	}
	deriving (Eq, Data, Typeable)

-- | A file path with no root, components, or filename
empty :: FilePath
empty = FilePath Nothing [] Nothing []

filenameBytes :: FilePath -> B.ByteString
filenameBytes p = B.append name ext where
	name = case pathBasename p of
		Nothing -> B.empty
		Just name' -> name'
	ext = case pathExtensions p of
		[] -> B.empty
		exts -> B.intercalate (B8.pack ".") (B.empty:exts)

-------------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------------

data Rules = Rules
	{ rulesName :: String
	, toByteChunks :: FilePath -> [B.ByteString]
	
	-- | Parse a strict 'B.ByteString', such as  those received from
	-- OS libraries, into a 'FilePath'.
	, fromBytes :: B.ByteString -> FilePath
	, caseSensitive :: Bool
	
	-- | Check if a 'FilePath' is valid; that is, it must not contain
	-- any illegal characters, and must have a root appropriate to the
	-- current 'Rules'.
	, valid :: FilePath -> Bool
	
	-- | Split a search path, such as @$PATH@ or @$PYTHONPATH@, into
	-- a list of 'FilePath's.
	, splitSearchPath :: B.ByteString -> [FilePath]
	
	-- | Remove redundant characters. On case-insensitive platforms,
	-- also lowercases any ASCII uppercase characters.
	, normalise :: FilePath -> FilePath
	}

instance Show Rules where
	showsPrec d r = showParen (d > 10) $
		showString "Rules " . shows (rulesName r)
