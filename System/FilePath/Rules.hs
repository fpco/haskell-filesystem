-- |
-- Module: System.FilePath.Rules
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
module System.FilePath.Rules
	( Rules
	, posix
	, windows
	
	-- * Type conversions
	, toBytes
	, fromBytes
	, toText
	, fromText
	
	-- * Rule-specific path properties
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
import           System.IO.Unsafe (unsafePerformIO)

import           System.FilePath hiding (root, filename)
import           System.FilePath.Internal

-------------------------------------------------------------------------------
-- Public helpers
-------------------------------------------------------------------------------

-- | Convert a 'FilePath' into a strict 'B.ByteString', suitable for passing
-- to OS libraries.
toBytes :: Rules -> FilePath -> B.ByteString
toBytes r = B.concat . toByteChunks r

-- | Attempt to convert a 'FilePath' to human-readable text.
--
-- If the path is decoded successfully, the result is a 'Right' containing
-- the decoded text. Successfully decoded text can be converted back to a
-- path using 'fromText'.
--
-- If the path cannot be decoded, the result is a 'Left' containing an
-- approximation of the original path. If displayed to the user, this value
-- should be accompanied by some warning that the path has an invalid
-- encoding. Approximated text cannot be converted back to the original path.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to display file paths with an unusual or
-- obscure encoding, use 'toBytes' and then decode them manually.
--
-- Since: 0.2
toText :: Rules -> FilePath -> Either T.Text T.Text
toText r path = encoded where
	bytes = toBytes r path
	encoded = case maybeDecodeUtf8 bytes of
		Just text -> Right text
		Nothing -> Left (T.pack (B8.unpack bytes))

-- | Convert human-readable text into a 'FilePath'.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to create file paths with an unusual or
-- obscure encoding, encode them manually and then use 'fromBytes'.
--
-- Since: 0.2
fromText :: Rules -> T.Text -> FilePath
fromText r text = fromBytes r (TE.encodeUtf8 text)

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

rootBytes :: Maybe Root -> B.ByteString
rootBytes r = B8.pack $ flip (maybe "") r $ \r' -> case r' of
	RootPosix -> "/"
	RootWindowsVolume c -> c : ":\\"
	RootWindowsCurrentVolume -> "\\"

byteDirectories :: FilePath -> [B.ByteString]
byteDirectories path = pathDirectories path ++ [filenameBytes path]

upperBytes :: B.ByteString -> B.ByteString
upperBytes bytes = (`B.map` bytes) $ \b -> if b >= 0x41 && b <= 0x5A
	then b + 0x20
	else b

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
posix :: Rules
posix = Rules
	{ rulesName = "POSIX"
	, toByteChunks = posixToByteChunks
	, fromBytes = posixFromBytes
	, valid = posixValid
	, splitSearchPath = posixSplitSearch
	}

posixToByteChunks :: FilePath -> [B.ByteString]
posixToByteChunks p = root : chunks where
	root = rootBytes $ pathRoot p
	chunks = intersperse (B8.pack "/") $ byteDirectories p

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes = if B.null bytes then empty else path where
	path = FilePath root directories basename exts
	
	split = B.split 0x2F bytes
	
	(root, pastRoot) = if B.null (head split)
		then (Just RootPosix, tail split)
		else (Nothing, split)
	
	(directories, filename)
		| P.null pastRoot = ([], B.empty)
		| otherwise = case last pastRoot of
			fn | fn == B8.pack "." -> (goodDirs pastRoot ++ [fn], B.empty)
			fn | fn == B8.pack ".." -> (goodDirs pastRoot, B.empty)
			fn -> (goodDirs (init pastRoot), fn)
	
	goodDirs = filter (\x -> not (x == B8.pack "." || B.null x))
	
	(basename, exts) = if B.null filename
		then (Nothing, [])
		else case B.split 0x2E filename of
			[] -> (Nothing, [])
			(name':exts') -> (Just name', exts')

posixValid :: FilePath -> Bool
posixValid p = validRoot && validDirectories where
	validDirectories = flip all (byteDirectories p)
		$ not . B.any (\b -> b == 0 || b == 0x2F)
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
windows :: Rules
windows = Rules
	{ rulesName = "Windows"
	, toByteChunks = winToByteChunks
	, fromBytes = winFromBytes
	, valid = winValid
	, splitSearchPath = map winFromBytes . filter (not . B.null) . B.split 0x3B
	}

winToByteChunks :: FilePath -> [B.ByteString]
winToByteChunks p = root : chunks where
	root = rootBytes $ pathRoot p
	chunks = intersperse (B8.pack "\\") $ byteDirectories p

winFromBytes :: B.ByteString -> FilePath
winFromBytes bytes = if B.null bytes then empty else path where
	path = FilePath root directories basename exts
	
	split = B.splitWith (\b -> b == 0x2F || b == 0x5C) bytes
	
	(root, pastRoot) = let
		head' = head split
		tail' = tail split
		in if B.null head'
			then (Just RootWindowsCurrentVolume, tail')
			else if B.elem 0x3A head'
				then (Just (parseDrive head'), tail')
				else (Nothing, split)
	
	parseDrive bytes' = RootWindowsVolume c where
		c = (toUpper . chr . fromIntegral . B.head) bytes'
	
	(directories, filename)
		| P.null pastRoot = ([], B.empty)
		| otherwise = case last pastRoot of
			fn | fn == B8.pack "." -> (goodDirs pastRoot ++ [fn], B.empty)
			fn | fn == B8.pack ".." -> (goodDirs pastRoot, B.empty)
			fn -> (goodDirs (init pastRoot), fn)
	
	goodDirs = filter (\x -> not (x == B8.pack "." || B.null x))
	
	(basename, exts) = if B.null filename
		then (Nothing, [])
		else case B.split 0x2E filename of
			[] -> (Nothing, [])
			(name':exts') -> (Just name', exts')

winValid :: FilePath -> Bool
winValid p = validRoot && noReserved && validCharacters where
	reservedChars = [0..0x1F] ++ [0x2F, 0x5C, 0x3F, 0x2A, 0x3A, 0x7C, 0x22, 0x3C, 0x3E]
	reservedNames = map B8.pack
		[ "AUX", "CLOCK$", "COM1", "COM2", "COM3", "COM4"
		, "COM5", "COM6", "COM7", "COM8", "COM9", "CON"
		, "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6"
		, "LPT7", "LPT8", "LPT9", "NUL", "PRN"
		]
	
	validRoot = case pathRoot p of
		Nothing -> True
		Just RootWindowsCurrentVolume -> True
		Just (RootWindowsVolume v) -> elem (toUpper v) ['A'..'Z']
		_ -> False
	
	noExt = p { pathExtensions = [] }
	noReserved = flip all (byteDirectories noExt)
		$ \c -> notElem (upperBytes c) reservedNames
	
	validCharacters = flip all (byteDirectories p)
		$ not . B.any (`elem` reservedChars)
