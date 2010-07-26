-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath.Rules
-- Copyright: 2010 John Millikin
-- License: X11
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-----------------------------------------------------------------------------

module System.FilePath.Rules
	( Rules
	, posix
	, windows
	
	-- * Rule-specific path properties
	, valid
	, normalise
	, equivalent
	
	-- * Parsing file paths
	, toBytes
	, toLazyBytes
	, toString
	, fromBytes
	, fromLazyBytes
	, fromString
	
	-- * Parsing search paths
	, splitSearchPath
	) where

import Prelude hiding (FilePath)
import Data.Char (toUpper, chr)
import Data.List (intersperse)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import System.FilePath.Internal

-------------------------------------------------------------------------------
-- Rule-specific path properties
-------------------------------------------------------------------------------
equivalent :: Rules -> FilePath -> FilePath -> Bool
equivalent r x y = n x == n y where
	n p = if caseSensitive r
		then normalise r p
		else casefold (normalise r p)
	
	-- TODO: use proper unicode case folding here? I'm not sure there's
	-- any way to do correct case-insensitive comparison without knowing
	-- the filename's encoding.
	casefold p = p
		{ pathComponents = map upperBytes $ pathComponents p
		, pathBasename = fmap upperBytes $ pathBasename p
		, pathExtensions = map upperBytes $ pathExtensions p
		}

-------------------------------------------------------------------------------
-- Public helpers
-------------------------------------------------------------------------------
toBytes :: Rules -> FilePath -> B.ByteString
toBytes r = B.concat . toByteChunks r

toLazyBytes :: Rules -> FilePath -> BL.ByteString
toLazyBytes r = BL.fromChunks . toByteChunks r

fromLazyBytes :: Rules -> BL.ByteString -> FilePath
fromLazyBytes r = fromBytes r . B.concat . BL.toChunks

toString :: Rules -> FilePath -> String
toString r = BL8.unpack . toLazyBytes r

fromString :: Rules -> String -> FilePath
fromString r = fromBytes r . B8.pack

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

rootBytes :: Maybe Root -> B.ByteString
rootBytes r = B8.pack $ flip (maybe "") r $ \r' -> case r' of
	RootPosix -> "/"
	RootWindowsVolume c -> c : ":\\"
	RootWindowsCurrentVolume -> "\\"

byteComponents :: FilePath -> [B.ByteString]
byteComponents path = pathComponents path ++ [name] where
	name = (`B.append` ext) $ case pathBasename path of
		Nothing -> B.empty
		Just name' -> name'
	ext = case pathExtensions path of
		[] -> B.empty
		exts -> B.intercalate (B8.pack ".") (B.empty:exts)

upperBytes :: B.ByteString -> B.ByteString
upperBytes bytes = (`B.map` bytes) $ \b -> if b >= 0x41 && b <= 0x5A
	then b + 0x20
	else b

-------------------------------------------------------------------------------
-- POSIX
-------------------------------------------------------------------------------

posix :: Rules
posix = Rules
	{ rulesName = "POSIX"
	, toByteChunks = posixToByteChunks
	, fromBytes = posixFromBytes
	, caseSensitive = True
	, valid = posixValid
	, splitSearchPath = posixSplitSearch
	, normalise = posixNormalise
	}

posixToByteChunks :: FilePath -> [B.ByteString]
posixToByteChunks p = [root] ++ chunks where
	root = rootBytes $ pathRoot p
	chunks = intersperse (B8.pack "/") $ byteComponents p

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes = if B.null bytes then empty else path where
	path = FilePath root cs name exts
	split' = B.split 0x2F bytes
	
	(root, pastRoot) = if B.null (head split')
		then (Just RootPosix, tail split')
		else (Nothing, split')
	
	cs = if null pastRoot
		then []
		else filter (not . B.null) $ if B.null (last pastRoot)
			then pastRoot
			else init pastRoot
	
	(name, exts) = case B.split 0x2E (last split') of
		[] -> (Nothing, [])
		(name':exts') -> (Just name', exts')

posixValid :: FilePath -> Bool
posixValid p = validRoot && validComponents where
	validComponents = flip all (byteComponents p)
		$ not . B.any (\b -> b == 0 || b == 0x2F)
	validRoot = case pathRoot p of
		Nothing -> True
		Just RootPosix -> True
		_ -> False

posixSplitSearch :: B.ByteString -> [FilePath]
posixSplitSearch = map (posixFromBytes . normSearch) . B.split 0x3A where
	normSearch bytes = if B.null bytes then (B8.pack ".") else bytes

posixNormalise :: FilePath -> FilePath
posixNormalise p = p { pathComponents = components } where
	components = filter (\c -> c /= (B8.pack ".")) $ pathComponents p

-------------------------------------------------------------------------------
-- Windows
-------------------------------------------------------------------------------

windows :: Rules
windows = Rules
	{ rulesName = "Windows"
	, toByteChunks = winToByteChunks
	, fromBytes = winFromBytes
	, caseSensitive = False
	, valid = winValid
	, splitSearchPath = map winFromBytes . filter (not . B.null) . B.split 0x3B
	, normalise = winNormalise
	}

winToByteChunks :: FilePath -> [B.ByteString]
winToByteChunks p = [root] ++ chunks where
	root = rootBytes $ pathRoot p
	chunks = intersperse (B8.pack "\\") $ byteComponents p

winFromBytes :: B.ByteString -> FilePath
winFromBytes bytes = if B.null bytes then empty else path where
	path = FilePath root cs name exts
	split' = B.splitWith isSep bytes
	isSep b = b == 0x2F || b == 0x5C
	
	(root, pastRoot) = let
		head' = head split'
		tail' = tail split'
		in if B.null head'
			then (Just RootWindowsCurrentVolume, tail')
			else if B.elem 0x3A head'
				then (Just (parseDrive head'), tail')
				else (Nothing, split')
	
	parseDrive bytes' = RootWindowsVolume c where
		c = chr . fromIntegral . B.head $ bytes'
	
	cs = if null pastRoot
		then []
		else filter (not . B.null) $ if B.null (last pastRoot)
			then pastRoot
			else init pastRoot
	
	(name, exts) = case B.split 0x2E (last split') of
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
	noReserved = flip all (byteComponents noExt)
		$ \c -> not (elem (upperBytes c) reservedNames)
	
	validCharacters = flip all (byteComponents p)
		$ not . B.any (\b -> elem b reservedChars)

winNormalise :: FilePath -> FilePath
winNormalise p = p' where
	p' = p
		{ pathComponents = components
		, pathRoot = root
		}
	components = filter (\c -> c /= (B8.pack ".")) $ pathComponents p
	root = case pathRoot p of
		Just (RootWindowsVolume c) -> Just (RootWindowsVolume (toUpper c))
		r -> r
