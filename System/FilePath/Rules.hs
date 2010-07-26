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
	, valid
	
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
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

import System.FilePath.Internal

-------------------------------------------------------------------------------
-- Generic
-------------------------------------------------------------------------------

rootBytes :: Root -> B.ByteString
rootBytes r = case r of
	RootPosix -> B8.pack ""

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
-- POSIX
-------------------------------------------------------------------------------

posix :: Rules
posix = Rules
	{ rulesName = T.pack "POSIX"
	, toByteChunks = posixToByteChunks
	, fromBytes = posixFromBytes
	, caseSensitive = True
	, valid = posixValid
	, splitSearchPath = posixSplitSearch
	}

posixComponents :: FilePath -> [B.ByteString]
posixComponents path =  dirs ++ [name] where
	cs = pathComponents path
	dirs = case pathRoot path of
		Nothing -> cs
		Just root' -> (rootBytes root'):cs
	name = (`B.append` ext) $ case pathBasename path of
		Nothing -> B.empty
		Just name' -> name'
	ext = case pathExtensions path of
		[] -> B.empty
		exts -> B.intercalate (B8.pack ".") (B.empty:exts)

posixToByteChunks :: FilePath -> [B.ByteString]
posixToByteChunks = intersperse (B8.pack "/") . posixComponents

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
	validComponents = flip all (posixComponents p)
		$ not . B.any (\b -> b == 0 || b == 0x2F)
	validRoot = case pathRoot p of
		Nothing -> True
		Just RootPosix -> True
		_ -> False

posixSplitSearch :: B.ByteString -> [FilePath]
posixSplitSearch = map (posixFromBytes . normSearch) . B.split 0x3A where
	normSearch bytes = if B.null bytes then (B8.pack ".") else bytes
