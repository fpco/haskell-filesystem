{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Filesystem.Path.Internal
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
module Filesystem.Path.Internal where

import           Prelude hiding (FilePath)

import qualified Data.ByteString.Char8 as B8
import           Data.Data (Data)
import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable (Typeable)

-------------------------------------------------------------------------------
-- File Paths
-------------------------------------------------------------------------------

data Chunk = Chunk
	{ chunkText :: T.Text
	, chunkGood :: Bool
	}
	deriving (Ord, Data, Typeable)

instance Eq Chunk where
	(Chunk x _) == (Chunk y _) = x == y

type Directory = Chunk
type Basename = Chunk
type Extension = Chunk

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

dot :: Chunk
dot = Chunk (T.pack ".") True

dots :: Chunk
dots = Chunk (T.pack "..") True

filenameChunk :: Bool -> FilePath -> Chunk
filenameChunk strict p = Chunk (T.concat texts) allGood where
	name = maybe (Chunk T.empty True) id (pathBasename p)
	exts = case pathExtensions p of
		[] -> []
		exts' -> intersperse dot ((Chunk T.empty True):exts')
	chunks = name:exts
	
	texts = map chunkText' chunks
	allGood = and (map chunkGood chunks)
	
	chunkText' c = if chunkGood c
		then if allGood || not strict
			then chunkText c
			else T.pack (B8.unpack (TE.encodeUtf8 (chunkText c)))
		else chunkText c

-------------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------------

data Rules platformFormat = Rules
	{ rulesName :: T.Text
	
	-- | Check if a 'FilePath' is valid; it must not contain any illegal
	-- characters, and must have a root appropriate to the current
	-- 'Rules'.
	, valid :: FilePath -> Bool
	
	-- | Split a search path, such as @$PATH@ or @$PYTHONPATH@, into
	-- a list of 'FilePath's.
	, splitSearchPath :: platformFormat -> [FilePath]
	
	-- | Attempt to convert a 'FilePath' to human&#x2010;readable text.
	--
	-- If the path is decoded successfully, the result is a 'Right'
	-- containing the decoded text. Successfully decoded text can be
	-- converted back to the original path using 'fromText'.
	--
	-- If the path cannot be decoded, the result is a 'Left' containing an
	-- approximation of the original path. If displayed to the user, this
	-- value should be accompanied by some warning that the path has an
	-- invalid encoding. Approximated text cannot be converted back to the
	-- original path.
	--
	-- This function ignores the user&#x2019;s locale, and assumes all
	-- file paths are encoded in UTF8. If you need to display file paths
	-- with an unusual or obscure encoding, use 'encode' and then decode
	-- them manually.
	--
	-- Since: 0.2
	, toText :: FilePath -> Either T.Text T.Text
	
	-- | Convert human&#x2010;readable text into a 'FilePath'.
	--
	-- This function ignores the user&#x2019;s locale, and assumes all
	-- file paths are encoded in UTF8. If you need to create file paths
	-- with an unusual or obscure encoding, encode them manually and then
	-- use 'decode'.
	--
	-- Since: 0.2
	, fromText :: T.Text -> FilePath
	
	-- | Convert a 'FilePath' to a platform&#x2010;specific format,
	-- suitable for use with external OS functions.
	--
	-- Since: 0.3
	, encode :: FilePath -> platformFormat
	
	-- | Convert a 'FilePath' from a platform&#x2010;specific format,
	-- suitable for use with external OS functions.
	--
	-- Since: 0.3
	, decode :: platformFormat -> FilePath
	
	-- | Attempt to convert a 'FilePath' to a string suitable for use with
	-- functions in @System.IO@. The contents of this string are
	-- platform&#x2010;dependent, and are not guaranteed to be
	-- human&#x2010;readable. For converting 'FilePath's to a
	-- human&#x2010;readable format, use 'toText'.
	--
	-- Since: 0.3.1
	, encodeString :: FilePath -> String
	
	-- | Attempt to parse a 'FilePath' from a string suitable for use
	-- with functions in @System.IO@. Do not use this function for parsing
	-- human&#x2010;readable paths, as the character set decoding is
	-- platform&#x2010;dependent. For converting human&#x2010;readable
	-- text to a 'FilePath', use 'fromText'.
	--
	-- Since: 0.3.1
	, decodeString :: String -> FilePath
	}

instance Show (Rules a) where
	showsPrec d r = showParen (d > 10)
		(showString "Rules " . shows (rulesName r))

textSplitBy :: (Char -> Bool) -> T.Text -> [T.Text]
#if MIN_VERSION_text(0,11,0)
textSplitBy = T.split
#else
textSplitBy = T.splitBy
#endif

