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

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (toUpper, chr)
import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (UnicodeException)
import           System.IO ()
import           System.IO.Unsafe (unsafePerformIO)

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

directoryChunks :: Bool -> FilePath -> [Chunk]
directoryChunks strict path = pathDirectories path ++ [filenameChunk strict path]

maybeDecodeUtf8 :: B.ByteString -> Maybe T.Text
maybeDecodeUtf8 = excToMaybe . TE.decodeUtf8 where
	excToMaybe :: a -> Maybe a
	excToMaybe x = unsafePerformIO $ Exc.catch
		(fmap Just (Exc.evaluate x))
		unicodeError
	
	unicodeError :: UnicodeException -> IO (Maybe a)
	unicodeError _ = return Nothing

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
	{ encodeString = T.unpack . either id id . posixToText
	, decodeString = posixFromText . T.pack
	}

posixToText :: FilePath -> Either T.Text T.Text
posixToText p = if good then Right text else Left text where
	good = and (map chunkGood chunks)
	text = T.concat (root : map chunkText chunks)
	
	root = rootText (pathRoot p)
	chunks = intersperse (Chunk (T.pack "/") True) (directoryChunks False p)

posixFromChunks :: [Chunk] -> FilePath
posixFromChunks chunks = FilePath root directories basename exts where
	(root, pastRoot) = if T.null (chunkText (head chunks))
		then (Just RootPosix, tail chunks)
		else (Nothing, chunks)
	
	(directories, filename)
		| P.null pastRoot = ([], Chunk T.empty True)
		| otherwise = case last pastRoot of
			fn | fn == dot -> (goodDirs pastRoot, Chunk T.empty True)
			fn | fn == dots -> (goodDirs pastRoot, Chunk T.empty True)
			fn -> (goodDirs (init pastRoot), fn)
	
	goodDirs = filter (not . T.null . chunkText)
	
	(basename, exts) = if T.null (chunkText filename)
		then (Nothing, [])
		else case T.split (== '.') (chunkText filename) of
			[] -> (Nothing, [])
			(name':exts') -> if chunkGood filename
				then (Just (Chunk name' True), map (\e -> Chunk e True) exts')
				else (Just (checkChunk name'), map checkChunk exts')
	
	checkChunk raw = case maybeDecodeUtf8 (B8.pack (T.unpack raw)) of
		Just text -> Chunk text True
		Nothing -> Chunk raw False

posixFromText :: T.Text -> FilePath
posixFromText text = if T.null text
	then empty
	else posixFromChunks (map (\t -> Chunk t True) (T.split (== '/') text))

posixToBytes :: FilePath -> B.ByteString
posixToBytes p = B.concat (root : chunks) where
	root = TE.encodeUtf8 (rootText (pathRoot p))
	chunks = intersperse (B8.pack "/") (map chunkBytes (directoryChunks True p))
	chunkBytes c = if chunkGood c
		then TE.encodeUtf8 (chunkText c)
		else B8.pack (T.unpack (chunkText c))

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes = if B.null bytes
	then empty
	else posixFromChunks $ flip map (B.split 0x2F bytes) $ \b -> case maybeDecodeUtf8 b of
		Just text -> Chunk text True
		Nothing -> Chunk (T.pack (B8.unpack b)) False

posixValid :: FilePath -> Bool
posixValid p = validRoot && validDirectories where
	validDirectories = all validChunk (directoryChunks True p)
	validChunk ch = not (T.any (\c -> c == '\0' || c == '/') (chunkText ch))
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
	chunks = intersperse (T.pack "\\") (map chunkText (directoryChunks False p))

winFromText :: T.Text -> FilePath
winFromText text = if T.null text then empty else path where
	path = FilePath root directories basename exts
	
	split = T.split (\c -> c == '/' || c == '\\') text
	
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
			fn | fn == chunkText dot -> (goodDirs pastRoot, T.empty)
			fn | fn == chunkText dots -> (goodDirs pastRoot, T.empty)
			fn -> (goodDirs (init pastRoot), fn)
	
	goodDirs = map (\t -> Chunk t True) . filter (not . T.null)
	
	(basename, exts) = if T.null filename
		then (Nothing, [])
		else case T.split (== '.') filename of
			[] -> (Nothing, [])
			(name':exts') -> (Just (Chunk name' True), map (\e -> Chunk e True) exts')

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
	noReserved = flip all (directoryChunks False noExt)
		$ \fn -> notElem (T.toUpper (chunkText fn)) reservedNames
	
	validCharacters = flip all (directoryChunks False p)
		$ not . T.any (`elem` reservedChars) . chunkText

winSplit :: T.Text -> [FilePath]
winSplit = map winFromText . filter (not . T.null) . T.split (== ';')
