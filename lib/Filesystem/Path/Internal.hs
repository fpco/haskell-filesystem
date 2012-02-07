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

import           Control.DeepSeq (NFData, rnf)
import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (chr, ord)
import           Data.Data (Data)
import           Data.List (intersperse)
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (UnicodeException)
import           Data.Typeable (Typeable)
import           System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- File Paths
-------------------------------------------------------------------------------

type Chunk = String
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
	deriving (Data, Typeable)

instance Eq FilePath where
	x == y = compare x y == EQ

instance Ord FilePath where
	compare = comparing (\p ->
		(pathRoot p
		, fmap unescape' (pathDirectories p)
		, fmap unescape' (pathBasename p)
		, fmap unescape' (pathExtensions p)
		))

instance NFData Root where
	rnf (RootWindowsVolume c) = rnf c
	rnf _ = ()

instance NFData FilePath where
	rnf p = rnf (pathRoot p) `seq` rnf (pathDirectories p) `seq` rnf (pathBasename p) `seq` rnf (pathExtensions p)

-- | A file path with no root, directory, or filename
empty :: FilePath
empty = FilePath Nothing [] Nothing []

dot :: Chunk
dot = "."

dots :: Chunk
dots = ".."

filenameChunk :: FilePath -> Chunk
filenameChunk p = concat (name:exts) where
	name = maybe "" id (pathBasename p)
	exts = case pathExtensions p of
		[] -> []
		exts' -> intersperse dot ("":exts')

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

escape :: T.Text -> Chunk
escape t = T.unpack t

unescape :: Chunk -> (T.Text, Bool)
unescape cs = if any (\c -> ord c >= 0xDC80 && ord c <= 0xDCFF) cs
	then (T.pack (map (\c -> if ord c >= 0xDC80 && ord c <= 0xDCFF
		then chr (ord c - 0xDC00)
		else c) cs), False)
	else (T.pack cs, True)

unescape' :: Chunk -> T.Text
unescape' = fst . unescape

unescapeBytes' :: Chunk -> B.ByteString
unescapeBytes' cs = if any (\c -> ord c >= 0xDC80 && ord c <= 0xDCFF) cs
	then B8.concat (map (\c -> if ord c >= 0xDC80 && ord c <= 0xDCFF
		then B8.singleton (chr (ord c - 0xDC00))
		else TE.encodeUtf8 (T.singleton c)) cs)
	else TE.encodeUtf8 (T.pack cs)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = loop where
	loop xs = let
		(chunk, rest) = break p xs
		cont = chunk : loop (tail rest)
		in if null rest then [chunk] else cont

textSplitBy :: (Char -> Bool) -> T.Text -> [T.Text]
#if MIN_VERSION_text(0,11,0)
textSplitBy = T.split
#else
textSplitBy = T.splitBy
#endif

parseFilename :: Chunk -> (Maybe Basename, [Extension])
parseFilename filename = parsed where
	parsed = if null filename
		then (Nothing, [])
		else case splitBy (== '.') filename of
			[] -> (Nothing, [])
			(name':exts') -> (Just name', exts')

maybeDecodeUtf8 :: B.ByteString -> Maybe T.Text
maybeDecodeUtf8 = excToMaybe . TE.decodeUtf8 where
	excToMaybe :: a -> Maybe a
	excToMaybe x = unsafePerformIO $ Exc.catch
		(fmap Just (Exc.evaluate x))
		unicodeError
	
	unicodeError :: UnicodeException -> IO (Maybe a)
	unicodeError _ = return Nothing