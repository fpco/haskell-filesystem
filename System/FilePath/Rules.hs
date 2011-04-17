-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath.Rules
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-----------------------------------------------------------------------------

module System.FilePath.Rules
	( Rules
	, posix
	, windows
	
	-- * Type conversions
	, toBytes
	, fromBytes
	
	-- * Rule-specific path properties
	, valid
	, splitSearchPath
	) where

import Prelude hiding (FilePath, null)
import qualified Prelude as P
import Data.Char (toUpper, chr)
import Data.List (intersperse)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import System.FilePath hiding (root, filename)
import System.FilePath.Internal

-------------------------------------------------------------------------------
-- Public helpers
-------------------------------------------------------------------------------

-- | Convert a 'FilePath' into a strict 'B.ByteString', suitable for passing
-- to OS libraries.
toBytes :: Rules -> FilePath -> B.ByteString
toBytes r = B.concat . toByteChunks r

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

rootBytes :: Maybe Root -> B.ByteString
rootBytes r = B8.pack $ flip (maybe "") r $ \r' -> case r' of
	RootPosix -> "/"
	RootWindowsVolume c -> c : ":\\"
	RootWindowsCurrentVolume -> "\\"

byteComponents :: FilePath -> [B.ByteString]
byteComponents path = pathComponents path ++ [filenameBytes path]

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
	, valid = posixValid
	, splitSearchPath = posixSplitSearch
	}

posixToByteChunks :: FilePath -> [B.ByteString]
posixToByteChunks p = root : chunks where
	root = rootBytes $ pathRoot p
	chunks = intersperse (B8.pack "/") $ byteComponents p

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes = if B.null bytes then empty else path where
	path = FilePath root cs name exts
	split' = B.split 0x2F bytes
	
	(root, pastRoot) = if B.null (head split')
		then (Just RootPosix, tail split')
		else (Nothing, split')
	
	cs = if P.null pastRoot
		then []
		else filter (not . B.null) $ if B.null (last pastRoot)
			then pastRoot
			else init pastRoot
	
	filename = last split'
	(name, exts) = if elem filename [B8.pack ".", B8.pack ".."]
		then (Just filename, [])
		else case B.split 0x2E (last split') of
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
	normSearch bytes = if B.null bytes then B8.pack "." else bytes

-------------------------------------------------------------------------------
-- Windows
-------------------------------------------------------------------------------

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
	
	cs = if P.null pastRoot
		then []
		else filter (not . B.null) $ if B.null (last pastRoot)
			then pastRoot
			else init pastRoot
	
	filename = last split'
	(name, exts) = if elem filename [B8.pack ".", B8.pack ".."]
		then (Just filename, [])
		else case B.split 0x2E (last split') of
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
		$ \c -> notElem (upperBytes c) reservedNames
	
	validCharacters = flip all (byteComponents p)
		$ not . B.any (`elem` reservedChars)
