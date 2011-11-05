-- |
-- Module: Filesystem.Path.Rules
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
module Filesystem.Path.Rules
	( Rules
	, posix
	, posix_ghc702
	, windows
	
	-- * Type conversions
	, toText
	, fromText
	, encode
	, decode
	, encodeString
	, decodeString
	
	-- * Rule&#x2010;specific path properties
	, valid
	, splitSearchPath
	) where

import           Prelude hiding (FilePath, null)
import qualified Prelude as P

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (toUpper, chr, ord)
import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.IO ()

import           Filesystem.Path hiding (root, filename, basename)
import           Filesystem.Path.Internal

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

rootText :: Maybe Root -> T.Text
rootText r = T.pack $ flip (maybe "") r $ \r' -> case r' of
	RootPosix -> "/"
	RootWindowsVolume c -> c : ":\\"
	RootWindowsCurrentVolume -> "\\"

directoryChunks :: FilePath -> [T.Text]
directoryChunks path = pathDirectories path ++ [filenameText path]

-------------------------------------------------------------------------------
-- POSIX
-------------------------------------------------------------------------------

-- | Linux, BSD, OS X, and other UNIX or UNIX-like operating systems.
posix :: Rules B.ByteString
posix = Rules
	{ rulesName = T.pack "POSIX"
	, valid = posixValid
	, splitSearchPath = posixSplitSearch
	, toText = posixToText
	, fromText = posixFromText
	, encode = posixToBytes
	, decode = posixFromBytes
	, encodeString = B8.unpack . posixToBytes
	, decodeString = posixFromBytes . B8.pack
	}

-- | Linux, BSD, OS X, and other UNIX or UNIX-like operating systems.
--
-- This variant is for use with GHC 7.2 or later, which tries to decode
-- file paths in its IO computations.
posix_ghc702 :: Rules B.ByteString
posix_ghc702 = posix
	{ rulesName = T.pack "POSIX (GHC 7.2)"
	, encodeString = posixToGhcString
	, decodeString = posixFromGhcString
	}

posixToText :: FilePath -> Either T.Text T.Text
posixToText p = if good then Right text else Left text where
	good = and (map snd chunks)
	text = T.concat (root : map fst chunks)
	
	root = rootText (pathRoot p)
	chunks = intersperse (T.pack "/", True) (map unescape (directoryChunks p))

posixFromChunks :: [T.Text] -> FilePath
posixFromChunks chunks = FilePath root directories basename exts where
	(root, pastRoot) = if T.null (head chunks)
		then (Just RootPosix, tail chunks)
		else (Nothing, chunks)
	
	(directories, filename)
		| P.null pastRoot = ([], T.empty)
		| otherwise = case last pastRoot of
			fn | fn == dot -> (goodDirs pastRoot, T.empty)
			fn | fn == dots -> (goodDirs pastRoot, T.empty)
			fn -> (goodDirs (init pastRoot), fn)
	
	goodDirs = filter (not . T.null)
	
	(basename, exts) = parseFilename filename

posixFromText :: T.Text -> FilePath
posixFromText text = if T.null text
	then empty
	else posixFromChunks (textSplitBy (== '/') text)

posixToBytes :: FilePath -> B.ByteString
posixToBytes p = B.concat (root : chunks) where
	root = TE.encodeUtf8 (rootText (pathRoot p))
	chunks = intersperse (B8.pack "/") (map chunkBytes (directoryChunks p))
	chunkBytes t = if T.any (\c -> ord c >= 0xEF00 && ord c <= 0xEFFF) t
		then unescapeBytes' t
		else TE.encodeUtf8 t

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes = if B.null bytes
	then empty
	else posixFromChunks $ flip map (B.split 0x2F bytes) $ \b -> case maybeDecodeUtf8 b of
		Just text -> text
		Nothing -> T.pack (map (\c -> if ord c >= 0x80
			then chr (ord c + 0xEF00)
			else c) (B8.unpack b))

posixToGhcString :: FilePath -> String
posixToGhcString p = P.concat (root : chunks) where
	root = T.unpack (rootText (pathRoot p))
	chunks = intersperse "/" (map T.unpack (directoryChunks p))

posixFromGhcString :: String -> FilePath
posixFromGhcString = posixFromText . T.pack

posixValid :: FilePath -> Bool
posixValid p = validRoot && validDirectories where
	validDirectories = all validChunk (directoryChunks p)
	validChunk ch = not (T.any (\c -> c == '\0' || c == '/') ch)
	validRoot = case pathRoot p of
		Nothing -> True
		Just RootPosix -> True
		_ -> False

posixSplitSearch :: B.ByteString -> [FilePath]
posixSplitSearch = map (posixFromBytes . normSearch) . B.split 0x3A where
	normSearch bytes = if B.null bytes then B8.pack "." else bytes

-------------------------------------------------------------------------------
-- Windows
-------------------------------------------------------------------------------

-- | Windows and DOS
windows :: Rules T.Text
windows = Rules
	{ rulesName = T.pack "Windows"
	, valid = winValid
	, splitSearchPath = winSplit
	, toText = Right . winToText
	, fromText = winFromText
	, encode = winToText
	, decode = winFromText
	, encodeString = T.unpack . winToText
	, decodeString = winFromText . T.pack
	}

winToText :: FilePath -> T.Text
winToText p = T.concat (root : chunks) where
	root = rootText (pathRoot p)
	chunks = intersperse (T.pack "\\") (directoryChunks p)

winFromText :: T.Text -> FilePath
winFromText text = if T.null text then empty else path where
	path = FilePath root directories basename exts
	
	split = textSplitBy (\c -> c == '/' || c == '\\') text
	
	(root, pastRoot) = let
		head' = head split
		tail' = tail split
		in if T.null head'
			then (Just RootWindowsCurrentVolume, tail')
			else if T.any (== ':') head'
				then (Just (parseDrive head'), tail')
				else (Nothing, split)
	
	parseDrive = RootWindowsVolume . toUpper . T.head
	
	(directories, filename)
		| P.null pastRoot = ([], T.empty)
		| otherwise = case last pastRoot of
			fn | fn == dot -> (goodDirs pastRoot, T.empty)
			fn | fn == dots -> (goodDirs pastRoot, T.empty)
			fn -> (goodDirs (init pastRoot), fn)
	
	goodDirs = filter (not . T.null)
	
	(basename, exts) = parseFilename filename

winValid :: FilePath -> Bool
winValid p = validRoot && noReserved && validCharacters where
	reservedChars = map chr [0..0x1F] ++ "/\\?*:|\"<>"
	reservedNames = map T.pack
		[ "AUX", "CLOCK$", "COM1", "COM2", "COM3", "COM4"
		, "COM5", "COM6", "COM7", "COM8", "COM9", "CON"
		, "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6"
		, "LPT7", "LPT8", "LPT9", "NUL", "PRN"
		]
	
	validRoot = case pathRoot p of
		Nothing -> True
		Just RootWindowsCurrentVolume -> True
		Just (RootWindowsVolume v) -> elem v ['A'..'Z']
		_ -> False
	
	noExt = p { pathExtensions = [] }
	noReserved = flip all (directoryChunks noExt)
		$ \fn -> notElem (T.toUpper fn) reservedNames
	
	validCharacters = flip all (directoryChunks p)
		$ not . T.any (`elem` reservedChars)

winSplit :: T.Text -> [FilePath]
winSplit = map winFromText . filter (not . T.null) . textSplitBy (== ';')
