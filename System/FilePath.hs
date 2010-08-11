-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-----------------------------------------------------------------------------

module System.FilePath
	( FilePath
	, empty
	
	-- * Basic properties
	, null
	, root
	, directory
	, parent
	, filename
	, basename
	, absolute
	, relative
	
	-- * Basic operations
	, append
	, (</>)
	, concat
	, commonPrefix
	
	-- * Extensions
	, extension
	, extensions
	, hasExtension
	
	, addExtension
	, (<.>)
	, dropExtension
	, replaceExtension
	
	, addExtensions
	, dropExtensions
	, replaceExtensions
	
	, splitExtension
	, splitExtensions
	) where

import Prelude hiding (FilePath, concat, null)
import qualified Prelude as P
import Data.Maybe (isNothing)
import qualified Data.Monoid as M
import System.FilePath.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

instance M.Monoid FilePath where
	mempty = empty
	mappend = append
	mconcat = concat

-------------------------------------------------------------------------------
-- Basic properties
-------------------------------------------------------------------------------

null :: FilePath -> Bool
null = (== empty)

root :: FilePath -> FilePath
root p = empty { pathRoot = pathRoot p }

directory :: FilePath -> FilePath
directory p = empty
	{ pathRoot = pathRoot p
	, pathComponents = let
		starts = map (Just . B8.pack) [".", ".."]
		dot = if safeHead (pathComponents p) `elem` starts
			then []
			else if isNothing (pathRoot p)
				then [B8.pack "."]
				else []
		in dot ++ pathComponents p
	}

parent :: FilePath -> FilePath
parent p = empty
	{ pathRoot = pathRoot p
	, pathComponents = let
		starts = map (Just . B8.pack) [".", ".."]
		components = if null (filename p)
			then safeInit (pathComponents p)
			else pathComponents p
		
		dot = if safeHead components `elem` starts
			then []
			else if isNothing (pathRoot p)
				then [B8.pack "."]
				else []
		
		in dot ++ components
	}

filename :: FilePath -> FilePath
filename p = empty
	{ pathBasename = pathBasename p
	, pathExtensions = pathExtensions p
	}

basename :: FilePath -> FilePath
basename p = empty
	{ pathBasename = pathBasename p
	}

absolute :: FilePath -> Bool
absolute p = case pathRoot p of
	Just RootPosix -> True
	Just (RootWindowsVolume _) -> True
	_ -> False

relative :: FilePath -> Bool
relative p = case pathRoot p of
	Just _ -> False
	_ -> True

-------------------------------------------------------------------------------
-- Basic operations
-------------------------------------------------------------------------------

append :: FilePath -> FilePath -> FilePath
append x y = if absolute y then y else xy where
	xy = y
		{ pathRoot = pathRoot x
		, pathComponents = components
		}
	components = xComponents ++ pathComponents y
	xComponents = (pathComponents x ++) $ if null (filename x)
		then []
		else [filenameBytes x]

(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

concat :: [FilePath] -> FilePath
concat [] = empty
concat ps = foldr1 append ps

commonPrefix :: [FilePath] -> FilePath
commonPrefix [] = empty
commonPrefix ps = foldr1 step ps where
	step x y = if pathRoot x /= pathRoot y
		then empty
		else let cs = commonComponents x y in
			if cs /= pathComponents x
				then empty { pathRoot = pathRoot x, pathComponents = cs }
				else if pathBasename x /= pathBasename y
					then empty { pathRoot = pathRoot x, pathComponents = cs }
					else let exts = commonExtensions x y in
						x { pathExtensions = exts }
	
	commonComponents x y = common (pathComponents x) (pathComponents y)
	commonExtensions x y = common (pathExtensions x) (pathExtensions y)
	
	common [] _ = []
	common _ [] = []
	common (x:xs) (y:ys) = if x == y
		then x : common xs ys
		else []

-------------------------------------------------------------------------------
-- Basenames
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Extensions
-------------------------------------------------------------------------------

extension :: FilePath -> Maybe B.ByteString
extension p = case extensions p of
	[] -> Nothing
	es -> Just (last es)

extensions :: FilePath -> [B.ByteString]
extensions = pathExtensions

hasExtension :: FilePath -> B.ByteString -> Bool
hasExtension p e = extension p == Just e

addExtension :: FilePath -> B.ByteString -> FilePath
addExtension p ext = addExtensions p [ext]

addExtensions :: FilePath -> [B.ByteString] -> FilePath
addExtensions p exts = p { pathExtensions = pathExtensions p ++ exts }

(<.>) :: FilePath -> B.ByteString -> FilePath
(<.>) = addExtension

dropExtension :: FilePath -> FilePath
dropExtension p = p { pathExtensions = safeInit (pathExtensions p) }

dropExtensions :: FilePath -> FilePath
dropExtensions p = p { pathExtensions = [] }

replaceExtension :: FilePath -> B.ByteString -> FilePath
replaceExtension = addExtension . dropExtension

replaceExtensions :: FilePath -> [B.ByteString] -> FilePath
replaceExtensions = addExtensions . dropExtensions

splitExtension :: FilePath -> (FilePath, Maybe B.ByteString)
splitExtension p = (dropExtension p, extension p)

splitExtensions :: FilePath -> (FilePath, [B.ByteString])
splitExtensions p = (dropExtensions p, extensions p)

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

safeInit :: [a] -> [a]
safeInit xs = case xs of
	[] -> []
	_ -> init xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
