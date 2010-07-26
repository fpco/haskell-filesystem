-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath.Internal
-- Copyright: 2010 John Millikin
-- License: X11
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module System.FilePath.Internal where

import Prelude hiding (FilePath)
import Data.Word (Word8)
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B

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
	}

instance Show Rules where
	showsPrec d r = showParen (d > 10) $
		showString "Rules " . shows (rulesName r)
