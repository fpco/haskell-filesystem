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
	(
	-- * Types
	  FilePath
	, Extension
	
	-- * Basic properties
	, empty
	, null
	, absolute
	, relative
	, root
	, basename
	, dirname
	
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
import qualified Data.Monoid as M
import System.FilePath.Internal

instance M.Monoid FilePath where
	mempty = empty
	mappend = append
	mconcat = concat

-------------------------------------------------------------------------------
-- Basic properties
-------------------------------------------------------------------------------

null :: FilePath -> Bool
null = (== empty)

absolute :: FilePath -> Bool
absolute p = case pathRoot p of
	Just RootPosix -> True
	Just (RootWindowsVolume _) -> True
	_ -> False

relative :: FilePath -> Bool
relative p = case pathRoot p of
	Just _ -> False
	_ -> True

root :: FilePath -> FilePath
root p = empty { pathRoot = pathRoot p }

basename :: FilePath -> FilePath
basename p = p
	{ pathRoot = Nothing
	, pathComponents = []
	}

dirname :: FilePath -> FilePath
dirname p = p
	{ pathBasename = Nothing
	, pathExtensions = []
	}

-------------------------------------------------------------------------------
-- Basic operations
-------------------------------------------------------------------------------

append :: FilePath -> FilePath -> FilePath
append x y = if absolute y then y else xy where
	xy = y
		{ pathRoot = pathRoot x
		, pathComponents = components
		}
	components = pathComponents x ++ pathComponents y

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
-- Extensions
-------------------------------------------------------------------------------

extension :: FilePath -> Maybe Extension
extension p = case pathExtensions p of
	[] -> Nothing
	es -> Just (last es)

extensions :: FilePath -> [Extension]
extensions = pathExtensions

hasExtension :: FilePath -> Extension -> Bool
hasExtension p e = case pathExtensions p of
	[] -> False
	es -> last es == e

addExtension :: FilePath -> Extension -> FilePath
addExtension p ext = addExtensions p [ext]

(<.>) :: FilePath -> Extension -> FilePath
(<.>) = addExtension

dropExtension :: FilePath -> FilePath
dropExtension p = case pathExtensions p of
	[] -> p
	es -> p { pathExtensions = init es }

replaceExtension :: FilePath -> Extension -> FilePath
replaceExtension = addExtension . dropExtension

addExtensions :: FilePath -> [Extension] -> FilePath
addExtensions p exts = p { pathExtensions = pathExtensions p ++ exts }

dropExtensions :: FilePath -> FilePath
dropExtensions p = p { pathExtensions = [] }

replaceExtensions :: FilePath -> [Extension] -> FilePath
replaceExtensions p exts = p { pathExtensions = exts }

splitExtension :: FilePath -> (FilePath, Maybe Extension)
splitExtension p = case pathExtensions p of
	[] -> (p, Nothing)
	exts -> (p { pathExtensions = init exts }, Just (last exts))

splitExtensions :: FilePath -> (FilePath, [Extension])
splitExtensions p = (dropExtensions p, pathExtensions p)
