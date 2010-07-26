-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath
-- Copyright: 2010 John Millikin
-- License: X11
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-----------------------------------------------------------------------------

module System.FilePath
	( FilePath
	, Extension
	
	-- * Basic path operations
	-- ** Predicates
	, absolute
	, relative
	
	-- ** ??
	, empty
	, append
	, (</>)
	, concat
	, splitName
	
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

import Prelude hiding (FilePath, concat)
import qualified Data.Monoid as M
import System.FilePath.Internal

instance M.Monoid FilePath where
	mempty = empty
	mappend = append
	mconcat = concat

-------------------------------------------------------------------------------
-- Basic path operations
-------------------------------------------------------------------------------

absolute :: FilePath -> Bool
absolute p = case pathRoot p of
	Just RootPosix -> True
	Just (RootWindowsVolume _) -> True
	_ -> False

relative :: FilePath -> Bool
relative p = case pathRoot p of
	Just _ -> False
	_ -> True

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

splitName :: FilePath -> (FilePath, FilePath)
splitName p = (withPath, withName) where
	withPath = p
		{ pathBasename = Nothing
		, pathExtensions = []
		}
	withName = p
		{ pathRoot = Nothing
		, pathComponents = []
		}

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
