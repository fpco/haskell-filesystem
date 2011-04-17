-- |
-- Module: System.FilePath
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-- High-level, byte-based file and directory path manipulations. You probably
-- want to import "System.FilePath.CurrentOS" instead, since it handles
-- detecting which rules to use in the current compilation.
--
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

import           Prelude hiding (FilePath, concat, null)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (isNothing)
import qualified Data.Monoid as M

import           System.FilePath.Internal

instance M.Monoid FilePath where
	mempty = empty
	mappend = append
	mconcat = concat

-------------------------------------------------------------------------------
-- Basic properties
-------------------------------------------------------------------------------

-- | @null p = (p == 'empty')@
null :: FilePath -> Bool
null = (== empty)

-- | Retrieves the 'FilePath'&#x2019;s root.
root :: FilePath -> FilePath
root p = empty { pathRoot = pathRoot p }

-- | Retrieves the 'FilePath'&#x2019;s directory. If the path is already a
-- directory, it is returned unchanged.
directory :: FilePath -> FilePath
directory p = empty
	{ pathRoot = pathRoot p
	, pathComponents = let
		starts = map (Just . B8.pack) [".", ".."]
		dot | safeHead (pathComponents p) `elem` starts = []
		    | isNothing (pathRoot p) = [B8.pack "."]
		    | otherwise = []
		in dot ++ pathComponents p
	}

-- | Retrieves the 'FilePath'&#x2019;s parent directory.
parent :: FilePath -> FilePath
parent p = empty
	{ pathRoot = pathRoot p
	, pathComponents = let
		starts = map (Just . B8.pack) [".", ".."]
		components = if null (filename p)
			then safeInit (pathComponents p)
			else pathComponents p
		
		dot | safeHead components `elem` starts = []
		    | isNothing (pathRoot p) = [B8.pack "."]
		    | otherwise = []
		in dot ++ components
	}

-- | Retrieve a 'FilePath'&#x2019;s filename component.
--
-- @filename \"foo/bar.txt\" == \"bar.txt\"@.
filename :: FilePath -> FilePath
filename p = empty
	{ pathBasename = pathBasename p
	, pathExtensions = pathExtensions p
	}

-- | Retrieve a 'FilePath'&#x2019;s basename component.
--
-- @
-- filename \"foo/bar.txt\" == \"bar\"
-- @
basename :: FilePath -> FilePath
basename p = empty
	{ pathBasename = pathBasename p
	}

-- | Test whether a path is absolute.
absolute :: FilePath -> Bool
absolute p = case pathRoot p of
	Just RootPosix -> True
	Just (RootWindowsVolume _) -> True
	_ -> False

-- | Test whether a path is relative.
relative :: FilePath -> Bool
relative p = case pathRoot p of
	Just _ -> False
	_ -> True

-------------------------------------------------------------------------------
-- Basic operations
-------------------------------------------------------------------------------

-- | Appends two 'FilePath's. If the second path is absolute, it is returned
-- unchanged.
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

-- | An alias for 'append'.
(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

-- | A fold over 'append'.
concat :: [FilePath] -> FilePath
concat [] = empty
concat ps = foldr1 append ps

-- | Find the greatest common prefix between two 'FilePath's.
commonPrefix :: [FilePath] -> FilePath
commonPrefix [] = empty
commonPrefix ps = foldr1 step ps where
	step x y = if pathRoot x /= pathRoot y
		then empty
		else let cs = commonComponents x y in
			if cs /= pathComponents x || pathBasename x /= pathBasename y
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
-- Extensions
-------------------------------------------------------------------------------

-- | Get a 'FilePath'&#x2019;s last extension, or 'Nothing' if it has no
-- extensions.
extension :: FilePath -> Maybe B.ByteString
extension p = case extensions p of
	[] -> Nothing
	es -> Just (last es)

-- | Get a 'FilePath'&#x2019;s full extension list.
extensions :: FilePath -> [B.ByteString]
extensions = pathExtensions

-- | Get whether a 'FilePath'&#x2019;s last extension is the predicate.
hasExtension :: FilePath -> B.ByteString -> Bool
hasExtension p e = extension p == Just e

-- | Append an extension to the end of a 'FilePath'.
addExtension :: FilePath -> B.ByteString -> FilePath
addExtension p ext = addExtensions p [ext]

-- | Append many extensions to the end of a 'FilePath'.
addExtensions :: FilePath -> [B.ByteString] -> FilePath
addExtensions p exts = p { pathExtensions = pathExtensions p ++ exts }

-- | An alias for 'addExtension'.
(<.>) :: FilePath -> B.ByteString -> FilePath
(<.>) = addExtension

-- | Remove a 'FilePath'&#x2019;s last extension.
dropExtension :: FilePath -> FilePath
dropExtension p = p { pathExtensions = safeInit (pathExtensions p) }

-- | Remove all extensions from a 'FilePath'.
dropExtensions :: FilePath -> FilePath
dropExtensions p = p { pathExtensions = [] }

-- | Replace a 'FilePath'&#x2019;s last extension.
replaceExtension :: FilePath -> B.ByteString -> FilePath
replaceExtension = addExtension . dropExtension

-- | Remove all extensions from a 'FilePath', and replace them with a new
-- list.
replaceExtensions :: FilePath -> [B.ByteString] -> FilePath
replaceExtensions = addExtensions . dropExtensions

-- | @splitExtension p = ('dropExtension' p, 'extension' p)@
splitExtension :: FilePath -> (FilePath, Maybe B.ByteString)
splitExtension p = (dropExtension p, extension p)

-- | @splitExtensions p = ('dropExtensions' p, 'extensions' p)@
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
