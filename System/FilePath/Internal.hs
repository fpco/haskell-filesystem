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
	, fromBytes :: B.ByteString -> FilePath
	, caseSensitive :: Bool
	, valid :: FilePath -> Bool
	, splitSearchPath :: B.ByteString -> [FilePath]
	, normalise :: FilePath -> FilePath
	}

instance Show Rules where
	showsPrec d r = showParen (d > 10) $
		showString "Rules " . shows (rulesName r)
