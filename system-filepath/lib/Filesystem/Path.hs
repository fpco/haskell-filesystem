{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Filesystem.Path
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-- High&#x2010;level, byte&#x2010;based file and directory path
-- manipulations. You probably want to import "Filesystem.Path.CurrentOS"
-- instead, since it handles detecting which rules to use in the current
-- compilation.
--
module Filesystem.Path
  ( FilePath
  , empty

  -- * Basic properties
  , null
  , root
  , directory
  , parent
  , filename
  , dirname
  , basename
  , absolute
  , relative

  -- * Basic operations
  , append
  , (</>)
  , concat
  , commonPrefix
  , stripPrefix
  , collapse
  , splitDirectories

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
import qualified Prelude as Prelude

import           Data.List (foldl')
import           Data.Maybe (isJust, isNothing)
import qualified Data.Semigroup as Sem
import qualified Data.Monoid as M
import qualified Data.Text as T

import           Filesystem.Path.Internal

instance Sem.Semigroup FilePath where
  (<>) = append

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
  , pathDirectories = let
    dot' | isJust (pathRoot p) = []
         | Prelude.null (pathDirectories p) = [dot]
         | otherwise = []
    in dot' ++ pathDirectories p
  }

-- | Retrieves the 'FilePath'&#x2019;s parent directory.
parent :: FilePath -> FilePath
parent p = empty
  { pathRoot = pathRoot p
  , pathDirectories = let
    starts = map Just [dot, dots]
    directories = if null (filename p)
      then safeInit (pathDirectories p)
      else pathDirectories p

    dot' | safeHead directories `elem` starts = []
         | isNothing (pathRoot p) = [dot]
         | otherwise = []
    in dot' ++ directories
  }

-- | Retrieve a 'FilePath'&#x2019;s filename component.
--
-- @
-- filename \"foo\/bar.txt\" == \"bar.txt\"
-- @
filename :: FilePath -> FilePath
filename p = empty
  { pathBasename = pathBasename p
  , pathExtensions = pathExtensions p
  }

-- | Retrieve a 'FilePath'&#x2019;s directory name. This is only the
-- /file name/ of the directory, not its full path.
--
-- @
-- dirname \"foo\/bar\/baz.txt\" == \"bar\"
-- dirname \"/\" == \"\"
-- @
--
-- Since: 0.4.1
dirname :: FilePath -> FilePath
dirname p = case reverse (pathDirectories p) of
  [] -> FilePath Nothing [] Nothing []
  (d:_) -> case parseFilename d of
    (base, exts) -> FilePath Nothing [] base exts

-- | Retrieve a 'FilePath'&#x2019;s basename component.
--
-- @
-- basename \"foo/bar.txt\" == \"bar\"
-- @
basename :: FilePath -> FilePath
basename p = empty
  { pathBasename = pathBasename p
  }

-- | Test whether a path is absolute.
absolute :: FilePath -> Bool
absolute p = case pathRoot p of
  Just RootPosix -> True
  Just RootWindowsVolume{} -> True
  Just RootWindowsCurrentVolume -> False
  Just RootWindowsUnc{} -> True
  Just RootWindowsDoubleQMark -> True
  Nothing -> False

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
append x y = cased where
  cased = case pathRoot y of
    Just RootPosix -> y
    Just RootWindowsVolume{} -> y
    Just RootWindowsCurrentVolume -> case pathRoot x of
      Just RootWindowsVolume{} -> y { pathRoot = pathRoot x }
      _ -> y
    Just RootWindowsUnc{} -> y
    Just RootWindowsDoubleQMark -> y
    Nothing -> xy
  xy = y
    { pathRoot = pathRoot x
    , pathDirectories = directories
    }
  directories = xDirectories ++ pathDirectories y
  xDirectories = (pathDirectories x ++) $ if null (filename x)
    then []
    else [filenameChunk x]

-- | An alias for 'append'.
(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

-- | A fold over 'append'.
concat :: [FilePath] -> FilePath
concat [] = empty
concat ps = foldr1 append ps

-- | Find the greatest common prefix between a list of 'FilePath's.
commonPrefix :: [FilePath] -> FilePath
commonPrefix [] = empty
commonPrefix ps = foldr1 step ps where
  step x y = if pathRoot x /= pathRoot y
    then empty
    else let cs = commonDirectories x y in
      if cs /= pathDirectories x || pathBasename x /= pathBasename y
        then empty { pathRoot = pathRoot x, pathDirectories = cs }
        else let exts = commonExtensions x y in
          x { pathExtensions = exts }

  commonDirectories x y = common (pathDirectories x) (pathDirectories y)
  commonExtensions x y = common (pathExtensions x) (pathExtensions y)

  common [] _ = []
  common _ [] = []
  common (x:xs) (y:ys) = if x == y
    then x : common xs ys
    else []

-- | Remove a prefix from a path.
--
-- @
-- 'stripPrefix' \"\/foo\/\" \"\/foo\/bar\/baz.txt\" == Just \"bar\/baz.txt\"
-- 'stripPrefix' \"\/foo\/\" \"\/bar\/baz.txt\" == Nothing
-- @
--
-- This function operates on logical prefixes, rather than by counting
-- characters. The prefix @\"\/foo\/bar\/baz\"@ is interpreted the path
-- @(\"\/foo\/bar\/\", \"baz\")@, and will be stripped accordingly:
--
-- @
-- 'stripPrefix' \"\/foo\/bar\/baz\" \"\/foo\/bar\/baz\/qux\" == Nothing
-- 'stripPrefix' \"\/foo\/bar\/baz\" \"\/foo\/bar\/baz.txt\" == Just \".txt\"
-- @
--
-- Since: 0.4.1
stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix x y = if pathRoot x /= pathRoot y
  then case pathRoot x of
    Nothing -> Just y
    Just _ -> Nothing
  else do
    dirs <- strip (pathDirectories x) (pathDirectories y)
    case dirs of
      [] -> case (pathBasename x, pathBasename y) of
        (Nothing, Nothing) -> do
          exts <- strip (pathExtensions x) (pathExtensions y)
          return (y { pathRoot = Nothing, pathDirectories = dirs, pathExtensions = exts })
        (Nothing, Just _) -> case pathExtensions x of
          [] -> Just (y { pathRoot = Nothing, pathDirectories = dirs })
          _ -> Nothing
        (Just x_b, Just y_b) | x_b == y_b -> do
          exts <- strip (pathExtensions x) (pathExtensions y)
          return (empty { pathExtensions = exts })
        _ -> Nothing
      _ -> case (pathBasename x, pathExtensions x) of
        (Nothing, []) -> Just (y { pathRoot = Nothing, pathDirectories = dirs })
        _ -> Nothing

strip :: Eq a => [a] -> [a] -> Maybe [a]
strip [] ys = Just ys
strip _ [] = Nothing
strip (x:xs) (y:ys) = if x == y
  then strip xs ys
  else Nothing

-- | Remove intermediate @\".\"@ and @\"..\"@ directories from a path.
--
-- @
-- 'collapse' \"\/foo\/.\/bar\" == \"\/foo\/bar\"
-- 'collapse' \"\/foo\/bar\/..\/baz\" == \"\/foo\/baz\"
-- 'collapse' \"\/foo\/..\/..\/bar\" == \"\/bar\"
-- 'collapse' \".\/foo\/bar\" == \".\/foo\/baz\"
-- @
--
-- Note that if any of the elements are symbolic links, 'collapse' may change
-- which file the path resolves to.
--
-- Since: 0.2
collapse :: FilePath -> FilePath
collapse p = p { pathDirectories = newDirs } where
  newDirs = case pathRoot p of
    Nothing -> reverse revNewDirs
    Just _ -> dropWhile (\x -> x == dot || x == dots) (reverse revNewDirs)
  (_, revNewDirs) = foldl' step (True, []) (pathDirectories p)

  step (True, acc) c = (False, c:acc)
  step (_, acc) c | c == dot = (False, acc)
  step (_, acc) c | c == dots = case acc of
    [] -> (False, c:acc)
    (h:ts) | h == dot -> (False, c:ts)
           | h == dots -> (False, c:acc)
           | otherwise -> (False, ts)
  step (_, acc) c = (False, c:acc)

-- | expand a FilePath into a list of the root name, directories, and file name
--
-- Since: 0.4.7
splitDirectories :: FilePath -> [FilePath]
splitDirectories p = rootName ++ dirNames ++ fileName where
  rootName = case pathRoot p of
    Nothing -> []
    r -> [empty { pathRoot = r }]
  dirNames = map (\d -> empty { pathDirectories = [d] }) (pathDirectories p)
  fileName = case (pathBasename p, pathExtensions p) of
    (Nothing, []) -> []
    _ -> [filename p]

-------------------------------------------------------------------------------
-- Extensions
-------------------------------------------------------------------------------

-- | Get a 'FilePath'&#x2019;s last extension, or 'Nothing' if it has no
-- extensions.
extension :: FilePath -> Maybe T.Text
extension p = case extensions p of
  [] -> Nothing
  es -> Just (last es)

-- | Get a 'FilePath'&#x2019;s full extension list.
extensions :: FilePath -> [T.Text]
extensions = map unescape' . pathExtensions


-- | Get whether a 'FilePath'&#x2019;s last extension is the predicate.
hasExtension :: FilePath -> T.Text -> Bool
hasExtension p e = extension p == Just e

-- | Append an extension to the end of a 'FilePath'.
addExtension :: FilePath -> T.Text -> FilePath
addExtension p ext = addExtensions p [ext]

-- | Append many extensions to the end of a 'FilePath'.
addExtensions :: FilePath -> [T.Text] -> FilePath
addExtensions p exts = p { pathExtensions = newExtensions } where
  newExtensions = pathExtensions p ++ map escape exts

-- | An alias for 'addExtension'.
(<.>) :: FilePath -> T.Text -> FilePath
(<.>) = addExtension

-- | Remove a 'FilePath'&#x2019;s last extension.
dropExtension :: FilePath -> FilePath
dropExtension p = p { pathExtensions = safeInit (pathExtensions p) }

-- | Remove all extensions from a 'FilePath'.
dropExtensions :: FilePath -> FilePath
dropExtensions p = p { pathExtensions = [] }

-- | Replace a 'FilePath'&#x2019;s last extension.
replaceExtension :: FilePath -> T.Text -> FilePath
replaceExtension = addExtension . dropExtension

-- | Remove all extensions from a 'FilePath', and replace them with a new
-- list.
replaceExtensions :: FilePath -> [T.Text] -> FilePath
replaceExtensions = addExtensions . dropExtensions

-- | @splitExtension p = ('dropExtension' p, 'extension' p)@
splitExtension :: FilePath -> (FilePath, Maybe T.Text)
splitExtension p = (dropExtension p, extension p)

-- | @splitExtensions p = ('dropExtensions' p, 'extensions' p)@
splitExtensions :: FilePath -> (FilePath, [T.Text])
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
