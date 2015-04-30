{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Filesystem.Path
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
module Filesystem.Path
       ( Rules
       , currentOS
       , darwin
       , darwin_ghc702
       , decode
       , decodeOn
       , decodeString
       , decodeStringOn
       , encode
       , encodeOn
       , encodeString
       , encodeStringOn
       , fromText
       , fromTextOn
       , posix
       , posix_ghc702
       , posix_ghc704
       , splitSearchPath
       , splitSearchPathOn
       , splitSearchPathString
       , splitSearchPathStringOn
       , toText
       , toTextOn
       , valid
       , validOn
       , windows
       , FilePath
       , (<.>)
       , (</>)
       , absolute
       , addExtension
       , addExtensions
       , append
       , basename
       , collapse
       , commonPrefix
       , concat
       , directory
       , dirname
       , dropExtension
       , dropExtensions
       , empty
       , extension
       , extensions
       , filename
       , hasExtension
       , null
       , parent
       , relative
       , replaceExtension
       , replaceExtensions
       , root
       , splitDirectories
       , splitExtension
       , splitExtensions
       , stripPrefix
       ) where

import Control.DeepSeq ( NFData, rnf )
import qualified Data.ByteString as B
    ( ByteString, split, null, concat )
import qualified Data.ByteString as B ()
import qualified Data.ByteString.Char8 as B8 ( concat, singleton )
import qualified Data.ByteString.Char8 as B8 ( unpack, pack )
import Data.Char ( chr, ord )
import Data.Char ( toUpper )
import Data.Data ( Data )
import Data.List ( foldl' )
import Data.List ( intercalate )
import Data.List ( intersperse )
import Data.Maybe ( isJust, isNothing )
import Data.Monoid ( Monoid(..) )
import Data.Ord ( comparing )
import Data.String ( IsString(..) )
import qualified Data.Text as T
    ( split, unpack, Text, singleton, pack )
import qualified Data.Text as T
    ( concat,
      toCaseFold,
      take,
      stripPrefix,
      null,
      length,
      isPrefixOf,
      head,
      drop,
      break,
      append,
      any )
import qualified Data.Text as T ()
import qualified Data.Text.Encoding as TE
    ( encodeUtf8, decodeUtf8' )
import qualified Data.Text.Encoding as TE ( decodeUtf8 )
import Data.Typeable ( Typeable )
import qualified Prelude as P
import Prelude hiding (FilePath,null,concat)
import System.IO ()

#if defined(__HADDOCK__)
#  define PLATFORM_PATH_FORMAT platformTextFormat
#elif defined(CABAL_OS_WINDOWS) || defined(CABAL_OS_DARWIN)
#  define PLATFORM_PATH_FORMAT T.Text
#else
#  define PLATFORM_PATH_FORMAT B.ByteString
#endif

currentOS :: Rules PLATFORM_PATH_FORMAT
#if defined(__HADDOCK__)
currentOS = undefined
#elif defined(CABAL_OS_WINDOWS)
currentOS = windows
#elif defined(CABAL_OS_DARWIN)
#if __GLASGOW_HASKELL__ >= 702
currentOS = darwin_ghc702
#else
currentOS = darwin
#endif
#else
#if __GLASGOW_HASKELL__ >= 704
currentOS = posix_ghc704
#elif __GLASGOW_HASKELL__ >= 702
currentOS = posix_ghc702
#else
currentOS = posix
#endif
#endif

-- | Attempt to convert a 'FilePath' to human&#x2010;readable text.
--
-- If the path is decoded successfully, the result is a 'Right' containing
-- the decoded text. Successfully decoded text can be converted back to the
-- original path using 'fromText'.
--
-- If the path cannot be decoded, the result is a 'Left' containing an
-- approximation of the original path. If displayed to the user, this value
-- should be accompanied by some warning that the path has an invalid
-- encoding. Approximated text cannot be converted back to the original path.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to display file paths with an unusual or
-- obscure encoding, use 'encode' and then decode them manually.
--
-- Since: 0.2
toText :: FilePath -> Either T.Text T.Text
toText = toTextOn currentOS

-- | Convert human&#x2010;readable text into a 'FilePath'.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to create file paths with an unusual or
-- obscure encoding, encode them manually and then use 'decode'.
--
-- Since: 0.2
fromText :: T.Text -> FilePath
fromText = fromTextOn currentOS

-- | Check if a 'FilePath' is valid; it must not contain any illegal
-- characters, and must have a root appropriate to the current 'Rules'.
valid :: FilePath -> Bool
valid = validOn currentOS

-- | Split a search path, such as @$PATH@ or @$PYTHONPATH@, into a list
-- of 'FilePath's.
splitSearchPath :: PLATFORM_PATH_FORMAT -> [FilePath]
splitSearchPath = splitSearchPathOn currentOS

-- | splitSearchPathString is like 'splitSearchPath', but takes a string
-- encoded in the format used by @System.IO@.
splitSearchPathString :: String -> [FilePath]
splitSearchPathString = splitSearchPathStringOn currentOS

-- | Convert a 'FilePath' to a platform&#x2010;specific format, suitable
-- for use with external OS functions.
--
-- Note: The type @platformTextFormat@ can change depending upon the underlying
-- compilation platform. Consider using 'toText' or 'encodeString' instead.
-- See 'Filesystem.Path.Rules.Rules' for more information.
--
-- Since: 0.3
encode :: FilePath -> PLATFORM_PATH_FORMAT
encode = encodeOn currentOS

-- | Convert a 'FilePath' from a platform&#x2010;specific format, suitable
-- for use with external OS functions.
--
-- Note: The type @platformTextFormat@ can change depending upon the underlying
-- compilation platform. Consider using 'fromText' or 'decodeString' instead.
-- See 'Filesystem.Path.Rules.Rules' for more information.
--
-- Since: 0.3
decode :: PLATFORM_PATH_FORMAT -> FilePath
decode = decodeOn currentOS

-- | Attempt to convert a 'FilePath' to a string suitable for use with
-- functions in @System.IO@. The contents of this string are
-- platform&#x2010;dependent, and are not guaranteed to be
-- human&#x2010;readable. For converting 'FilePath's to a
-- human&#x2010;readable format, use 'toText'.
--
-- Since: 0.3.1
encodeString :: FilePath -> String
encodeString = encodeStringOn currentOS

-- | Attempt to parse a 'FilePath' from a string suitable for use with
-- functions in @System.IO@. Do not use this function for parsing
-- human&#x2010;readable paths, as the character set decoding is
-- platform&#x2010;dependent. For converting human&#x2010;readable text to a
-- 'FilePath', use 'fromText'.
--
-- Since: 0.3.1
decodeString :: String -> FilePath
decodeString = decodeStringOn currentOS

-------------------------------------------------------------------------------
-- Basic properties
-------------------------------------------------------------------------------

-- | @null p = (p == 'empty')@
null :: FilePath -> Bool
null = (== empty)

-- | Retrieves the 'FilePath'&#x2019;s root.
root :: FilePath -> FilePath
root p = empty {pathRoot = pathRoot p}

-- | Retrieves the 'FilePath'&#x2019;s directory. If the path is already a
-- directory, it is returned unchanged.
directory :: FilePath -> FilePath
directory p =
  empty {pathRoot = pathRoot p
        ,pathDirectories =
           let dot'
                 | isJust (pathRoot p) = []
                 | P.null (pathDirectories p) =
                   [dot]
                 | otherwise = []
           in dot' ++ pathDirectories p}

-- | Retrieves the 'FilePath'&#x2019;s parent directory.
parent :: FilePath -> FilePath
parent p =
  empty {pathRoot = pathRoot p
        ,pathDirectories =
           let starts = map Just [dot,dots]
               directories =
                 if null (filename p)
                    then safeInit (pathDirectories p)
                    else pathDirectories p
               dot'
                 | safeHead directories `elem` starts = []
                 | isNothing (pathRoot p) = [dot]
                 | otherwise = []
           in dot' ++ directories}

-- | Retrieve a 'FilePath'&#x2019;s filename component.
--
-- @
-- filename \"foo\/bar.txt\" == \"bar.txt\"
-- @
filename :: FilePath -> FilePath
filename p =
  empty {pathBasename = pathBasename p
        ,pathExtensions = pathExtensions p}

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
dirname p =
  case reverse (pathDirectories p) of
    [] -> FilePath Nothing [] Nothing []
    (d:_) ->
      case parseFilename d of
        (base,exts) ->
          FilePath Nothing [] base exts

-- | Retrieve a 'FilePath'&#x2019;s basename component.
--
-- @
-- basename \"foo/bar.txt\" == \"bar\"
-- @
basename :: FilePath -> FilePath
basename p =
  empty {pathBasename = pathBasename p}

-- | Test whether a path is absolute.
absolute :: FilePath -> Bool
absolute p =
  case pathRoot p of
    Just RootPosix -> True
    Just RootWindowsVolume{} -> True
    Just RootWindowsCurrentVolume -> False
    Just RootWindowsUnc{} -> True
    Just RootWindowsDoubleQMark -> True
    Nothing -> False

-- | Test whether a path is relative.
relative :: FilePath -> Bool
relative p =
  case pathRoot p of
    Just _ -> False
    _ -> True

-------------------------------------------------------------------------------
-- Basic operations
-------------------------------------------------------------------------------

-- | Appends two 'FilePath's. If the second path is absolute, it is returned
-- unchanged.
append :: FilePath -> FilePath -> FilePath
append x y = cased
  where cased =
          case pathRoot y of
            Just RootPosix -> y
            Just RootWindowsVolume{} -> y
            Just RootWindowsCurrentVolume ->
              case pathRoot x of
                Just RootWindowsVolume{} ->
                  y {pathRoot = pathRoot x}
                _ -> y
            Just RootWindowsUnc{} -> y
            Just RootWindowsDoubleQMark -> y
            Nothing -> xy
        xy =
          y {pathRoot = pathRoot x
            ,pathDirectories = directories}
        directories = xDirectories ++ pathDirectories y
        xDirectories =
          (pathDirectories x ++) $
          if null (filename x)
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
commonPrefix ps = foldr1 step ps
  where step x y =
          if pathRoot x /= pathRoot y
             then empty
             else let cs = commonDirectories x y
                  in if cs /= pathDirectories x || pathBasename x /=
                                                   pathBasename y
                        then empty {pathRoot = pathRoot x
                                   ,pathDirectories = cs}
                        else let exts =
                                   commonExtensions x y
                             in x {pathExtensions = exts}
        commonDirectories x y =
          common (pathDirectories x)
                 (pathDirectories y)
        commonExtensions x y =
          common (pathExtensions x)
                 (pathExtensions y)
        common [] _ = []
        common _ [] = []
        common (x:xs) (y:ys) =
          if x == y
             then x :
                  common xs ys
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
stripPrefix x y =
  if pathRoot x /= pathRoot y
     then case pathRoot x of
            Nothing -> Just y
            Just _ -> Nothing
     else do dirs <-
               strip (pathDirectories x)
                     (pathDirectories y)
             case dirs of
               [] ->
                 case (pathBasename x,pathBasename y) of
                   (Nothing,Nothing) ->
                     do exts <-
                          strip (pathExtensions x)
                                (pathExtensions y)
                        return (y {pathRoot = Nothing
                                  ,pathDirectories = dirs
                                  ,pathExtensions = exts})
                   (Nothing,Just _) ->
                     case pathExtensions x of
                       [] ->
                         Just (y {pathRoot = Nothing
                                 ,pathDirectories = dirs})
                       _ -> Nothing
                   (Just x_b,Just y_b)
                     | x_b == y_b ->
                       do exts <-
                            strip (pathExtensions x)
                                  (pathExtensions y)
                          return (empty {pathExtensions = exts})
                   _ -> Nothing
               _ ->
                 case (pathBasename x,pathExtensions x) of
                   (Nothing,[]) ->
                     Just (y {pathRoot = Nothing
                             ,pathDirectories = dirs})
                   _ -> Nothing

strip :: Eq a => [a] -> [a] -> Maybe [a]
strip [] ys = Just ys
strip _ [] = Nothing
strip (x:xs) (y:ys) =
  if x == y
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
collapse p = p {pathDirectories = newDirs}
  where newDirs =
          case pathRoot p of
            Nothing -> reverse revNewDirs
            Just _ ->
              dropWhile (\x -> x == dot || x == dots)
                        (reverse revNewDirs)
        (_,revNewDirs) =
          foldl' step
                 (True,[])
                 (pathDirectories p)
        step (True,acc) c = (False,c : acc)
        step (_,acc) c
          | c == dot = (False,acc)
        step (_,acc) c
          | c == dots =
            case acc of
              [] -> (False,c : acc)
              (h:ts)
                | h == dot -> (False,c : ts)
                | h == dots -> (False,c : acc)
                | otherwise -> (False,ts)
        step (_,acc) c = (False,c : acc)

-- | expand a FilePath into a list of the root name, directories, and file name
--
-- Since: 0.4.7
splitDirectories :: FilePath -> [FilePath]
splitDirectories p = rootName ++ dirNames ++ fileName
  where rootName =
          case pathRoot p of
            Nothing -> []
            r -> [empty {pathRoot = r}]
        dirNames =
          map (\d ->
                 empty {pathDirectories = [d]})
              (pathDirectories p)
        fileName =
          case (pathBasename p,pathExtensions p) of
            (Nothing,[]) -> []
            _ -> [filename p]

-------------------------------------------------------------------------------
-- Extensions
-------------------------------------------------------------------------------

-- | Get a 'FilePath'&#x2019;s last extension, or 'Nothing' if it has no
-- extensions.
extension :: FilePath -> Maybe T.Text
extension p =
  case extensions p of
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
addExtensions p exts =
  p {pathExtensions = newExtensions}
  where newExtensions =
          pathExtensions p ++
          map escape exts

-- | An alias for 'addExtension'.
(<.>) :: FilePath -> T.Text -> FilePath
(<.>) = addExtension

-- | Remove a 'FilePath'&#x2019;s last extension.
dropExtension :: FilePath -> FilePath
dropExtension p =
  p {pathExtensions = safeInit (pathExtensions p)}

-- | Remove all extensions from a 'FilePath'.
dropExtensions :: FilePath -> FilePath
dropExtensions p = p {pathExtensions = []}

-- | Replace a 'FilePath'&#x2019;s last extension.
replaceExtension :: FilePath -> T.Text -> FilePath
replaceExtension = addExtension . dropExtension

-- | Remove all extensions from a 'FilePath', and replace them with a new
-- list.
replaceExtensions :: FilePath -> [T.Text] -> FilePath
replaceExtensions = addExtensions . dropExtensions

-- | @splitExtension p = ('dropExtension' p, 'extension' p)@
splitExtension :: FilePath -> (FilePath, Maybe T.Text)
splitExtension p = (dropExtension p,extension p)

-- | @splitExtensions p = ('dropExtensions' p, 'extensions' p)@
splitExtensions :: FilePath -> (FilePath, [T.Text])
splitExtensions p = (dropExtensions p,extensions p)

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

safeInit :: [a] -> [a]
safeInit xs =
  case xs of
    [] -> []
    _ -> init xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-------------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------------

-- | The type of @platformFormat@ for 'Rules' is conditionally selected at
-- compilation time. As such it is only intended for direct use with external OS
-- functions and code that expects @platformFormat@ to be stable across platforms
-- may fail to subsequently compile on a differing platform.
--
-- For example: on Windows or OSX @platformFormat@ will be 'T.Text',
-- and on Linux it will be 'B.ByteString'.
--
-- If portability is a concern, restrict usage to functions which do not expose
-- @platformFormat@ directly.
data Rules platformFormat =
  Rules {rulesName :: T.Text
         -- | Check if a 'FilePath' is valid; it must not contain any illegal
         -- characters, and must have a root appropriate to the current
         -- 'Rules'.
        ,validOn :: FilePath -> Bool
         -- | Split a search path, such as @$PATH@ or @$PYTHONPATH@, into
         -- a list of 'FilePath's.
         --
         -- Note: The type of @platformTextFormat@ can change depending upon the
         -- underlying compilation platform. Consider using 'splitSearchPathStringOn'
         -- instead. See 'Rules' for more information.
        ,splitSearchPathOn :: platformFormat -> [FilePath]
         -- | splitSearchPathStringOn is like 'splitSearchPathOn', but takes a string
         -- encoded in the format used by @System.IO@.
        ,splitSearchPathStringOn :: String -> [FilePath]
         -- | Attempt to convert a 'FilePath' to human&#x2010;readable text.
         --
         -- If the path is decoded successfully, the result is a 'Right'
         -- containing the decoded text. Successfully decoded text can be
         -- converted back to the original path using 'fromTextOn'.
         --
         -- If the path cannot be decoded, the result is a 'Left' containing an
         -- approximation of the original path. If displayed to the user, this
         -- value should be accompanied by some warning that the path has an
         -- invalid encoding. Approximated text cannot be converted back to the
         -- original path.
         --
         -- This function ignores the user&#x2019;s locale, and assumes all
         -- file paths are encoded in UTF8. If you need to display file paths
         -- with an unusual or obscure encoding, use 'encodeOn' and then decode
         -- them manually.
         --
         -- Since: 0.2
        ,toTextOn :: FilePath -> Either T.Text T.Text
         -- | Convert human&#x2010;readable text into a 'FilePath'.
         --
         -- This function ignores the user&#x2019;s locale, and assumes all
         -- file paths are encoded in UTF8. If you need to create file paths
         -- with an unusual or obscure encoding, encode them manually and then
         -- use 'decodeOn'.
         --
         -- Since: 0.2
        ,fromTextOn :: T.Text -> FilePath
         -- | Convert a 'FilePath' to a platform&#x2010;specific format,
         -- suitable for use with external OS functions.
         --
         -- Note: The type of @platformTextFormat@ can change depending upon the
         -- underlying compilation platform. Consider using 'toTextOn' or
         -- 'encodeStringOn' instead. See 'Rules' for more information.
         --
         -- Since: 0.3
        ,encodeOn :: FilePath -> platformFormat
         -- | Convert a 'FilePath' from a platform&#x2010;specific format,
         -- suitable for use with external OS functions.
         --
         -- Note: The type of @platformTextFormat@ can change depending upon the
         -- underlying compilation platform. Consider using 'fromTextOn' or
         -- 'decodeStringOn' instead. See 'Rules' for more information.
         --
         -- Since: 0.3
        ,decodeOn :: platformFormat -> FilePath
         -- | Attempt to convert a 'FilePath' to a string suitable for use with
         -- functions in @System.IO@. The contents of this string are
         -- platform&#x2010;dependent, and are not guaranteed to be
         -- human&#x2010;readable. For converting 'FilePath's to a
         -- human&#x2010;readable format, use 'toTextOn'.
         --
         -- Since: 0.3.1
        ,encodeStringOn :: FilePath -> String
         -- | Attempt to parse a 'FilePath' from a string suitable for use
         -- with functions in @System.IO@. Do not use this function for parsing
         -- human&#x2010;readable paths, as the character set decoding is
         -- platform&#x2010;dependent. For converting human&#x2010;readable
         -- text to a 'FilePath', use 'fromTextOn'.
         --
         -- Since: 0.3.1
        ,decodeStringOn :: String -> FilePath}

instance Show (Rules a) where
  showsPrec d r =
    showParen (d > 10)
              (showString "Rules " .
               shows (rulesName r))

-------------------------------------------------------------------------------
-- POSIX
-------------------------------------------------------------------------------

-- | Linux, BSD, and other UNIX or UNIX-like operating systems.
posix :: Rules B.ByteString
posix =
  Rules {rulesName = T.pack "POSIX"
        ,validOn = posixValid
        ,splitSearchPathOn = posixSplitSearch
        ,splitSearchPathStringOn = posixSplitSearch . B8.pack
        ,toTextOn = posixToText
        ,fromTextOn = posixFromText
        ,encodeOn = posixToBytes
        ,decodeOn = posixFromBytes
        ,encodeStringOn = B8.unpack . posixToBytes
        ,decodeStringOn = posixFromBytes . B8.pack}

-- | Linux, BSD, and other UNIX or UNIX-like operating systems.
--
-- This is a variant of 'posix' for use with GHC 7.2, which tries to decode
-- file paths in its IO computations.
--
-- Since: 0.3.3 / 0.4.2
posix_ghc702 :: Rules B.ByteString
posix_ghc702 =
  posix {rulesName = T.pack "POSIX (GHC 7.2)"
        ,splitSearchPathStringOn = posixSplitSearchString posixFromGhc702String
        ,encodeStringOn = posixToGhc702String
        ,decodeStringOn = posixFromGhc702String}

-- | Linux, BSD, and other UNIX or UNIX-like operating systems.
--
-- This is a variant of 'posix' for use with GHC 7.4 or later, which tries to
-- decode file paths in its IO computations.
--
-- Since: 0.3.7 / 0.4.6
posix_ghc704 :: Rules B.ByteString
posix_ghc704 =
  posix {rulesName = T.pack "POSIX (GHC 7.4)"
        ,splitSearchPathStringOn = posixSplitSearchString posixFromGhc704String
        ,encodeStringOn = posixToGhc704String
        ,decodeStringOn = posixFromGhc704String}

posixToText :: FilePath -> Either T.Text T.Text
posixToText p =
  if good
     then Right text
     else Left text
  where good = and (map snd chunks)
        text =
          T.concat (root' :
                    map fst chunks)
        root' = rootText (pathRoot p)
        chunks =
          intersperse (T.pack "/",True)
                      (map unescape (directoryChunks p))

posixFromChunks :: [Chunk] -> FilePath
posixFromChunks chunks =
  FilePath root' directories basename' exts
  where (root',pastRoot) =
          if P.null (head chunks)
             then (Just RootPosix,tail chunks)
             else (Nothing,chunks)
        (directories,filename')
          | P.null pastRoot = ([],"")
          | otherwise =
            case last pastRoot of
              fn
                | fn == dot ->
                  (goodDirs pastRoot,"")
              fn
                | fn == dots ->
                  (goodDirs pastRoot,"")
              fn -> (goodDirs (init pastRoot),fn)
        goodDirs = filter (not . P.null)
        (basename',exts) = parseFilename filename'

posixFromText :: T.Text -> FilePath
posixFromText text =
  if T.null text
     then empty
     else posixFromChunks (map escape (textSplitBy (== '/') text))

posixToBytes :: FilePath -> B.ByteString
posixToBytes p = B.concat (root' : chunks)
  where root' = B8.pack (rootChunk (pathRoot p))
        chunks =
          intersperse (B8.pack "/")
                      (map chunkBytes (directoryChunks p))
        chunkBytes c = unescapeBytes' c

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes =
  if B.null bytes
     then empty
     else posixFromChunks $
          flip map (B.split 47 bytes) $
          \b ->
            case maybeDecodeUtf8 b of
              Just text -> escape text
              Nothing -> processInvalidUtf8 b

processInvalidUtf8 :: B.ByteString -> Chunk
processInvalidUtf8 bytes =
  intercalate "." textChunks
  where byteChunks = B.split 46 bytes
        textChunks = map unicodeDammit byteChunks
        unicodeDammit b =
          case maybeDecodeUtf8 b of
            Just t -> escape t
            Nothing ->
              map (\c ->
                     if ord c >= 128
                        then chr (ord c + 56320)
                        else c)
                  (B8.unpack b)

posixToGhc702String :: FilePath -> String
posixToGhc702String p = P.concat (root' : chunks)
  where root' = rootChunk (pathRoot p)
        chunks =
          intersperse "/"
                      (map escapeToGhc702 (directoryChunks p))

escapeToGhc702 :: Chunk -> String
escapeToGhc702 =
  map (\c ->
         if ord c >= 56448 && ord c <= 56575
            then chr (ord c - 56320 + 61184)
            else c)

posixFromGhc702String :: String -> FilePath
posixFromGhc702String cs =
  if P.null cs
     then empty
     else posixFromChunks (map escapeFromGhc702 (splitBy (== '/') cs))

escapeFromGhc702 :: String -> String
escapeFromGhc702 =
  map (\c ->
         if ord c >= 61312 && ord c <= 61439
            then
                 -- hopefully this isn't a valid UTF8 filename decoding to these
                 -- codepoints, but there's no way to tell here.
                 chr (ord c - 61184 + 56320)
            else c)

posixToGhc704String :: FilePath -> String
posixToGhc704String p = P.concat (root' : chunks)
  where root' = rootChunk (pathRoot p)
        chunks =
          intersperse "/"
                      (directoryChunks p)

posixFromGhc704String :: String -> FilePath
posixFromGhc704String cs =
  if P.null cs
     then empty
     else posixFromChunks (splitBy (== '/') cs)

posixValid :: FilePath -> Bool
posixValid p = validRoot && validDirectories
  where validDirectories =
          all validChunk (directoryChunks p)
        validChunk ch =
          not (any (\c -> c == '\0' || c == '/') ch)
        validRoot =
          case pathRoot p of
            Nothing -> True
            Just RootPosix -> True
            _ -> False

posixSplitSearch :: B.ByteString -> [FilePath]
posixSplitSearch =
  map (posixFromBytes . normSearch) .
  B.split 58
  where normSearch bytes =
          if B.null bytes
             then B8.pack "."
             else bytes

posixSplitSearchString :: (String -> FilePath) -> String -> [FilePath]
posixSplitSearchString toPath =
  map (toPath . normSearch) .
  splitBy (== ':')
  where normSearch s =
          if P.null s
             then "."
             else s

-------------------------------------------------------------------------------
-- Darwin
-------------------------------------------------------------------------------

-- | Darwin and Mac OS X.
--
-- This is almost identical to 'posix', but with a native path type of 'T.Text'
-- rather than 'B.ByteString'.
--
-- Since: 0.3.4 / 0.4.3
darwin :: Rules T.Text
darwin =
  Rules {rulesName = T.pack "Darwin"
        ,validOn = posixValid
        ,splitSearchPathOn = darwinSplitSearch
        ,splitSearchPathStringOn = darwinSplitSearch . TE.decodeUtf8 . B8.pack
        ,toTextOn = Right . darwinToText
        ,fromTextOn = posixFromText
        ,encodeOn = darwinToText
        ,decodeOn = posixFromText
        ,encodeStringOn = darwinToString
        ,decodeStringOn = darwinFromString}

-- | Darwin and Mac OS X.
--
-- This is a variant of 'darwin' for use with GHC 7.2 or later, which tries to
-- decode file paths in its IO computations.
--
-- Since: 0.3.4 / 0.4.3
darwin_ghc702 :: Rules T.Text
darwin_ghc702 =
  darwin {rulesName = T.pack "Darwin (GHC 7.2)"
         ,splitSearchPathStringOn = darwinSplitSearch . T.pack
         ,encodeStringOn = T.unpack . darwinToText
         ,decodeStringOn = posixFromText . T.pack}

darwinToText :: FilePath -> T.Text
darwinToText p = T.concat (root' : chunks)
  where root' = rootText (pathRoot p)
        chunks =
          intersperse (T.pack "/")
                      (map unescape' (directoryChunks p))

darwinToString :: FilePath -> String
darwinToString = B8.unpack . TE.encodeUtf8 . darwinToText

darwinFromString :: String -> FilePath
darwinFromString = posixFromText . TE.decodeUtf8 . B8.pack

darwinSplitSearch :: T.Text -> [FilePath]
darwinSplitSearch =
  map (posixFromText . normSearch) .
  textSplitBy (== ':')
  where normSearch text =
          if T.null text
             then T.pack "."
             else text

-------------------------------------------------------------------------------
-- Windows
-------------------------------------------------------------------------------

-- | Windows and DOS
windows :: Rules T.Text
windows =
  Rules {rulesName = T.pack "Windows"
        ,validOn = winValid
        ,splitSearchPathOn = winSplit
        ,splitSearchPathStringOn = winSplit . T.pack
        ,toTextOn = Right . winToText
        ,fromTextOn = winFromText
        ,encodeOn = winToText
        ,decodeOn = winFromText
        ,encodeStringOn = T.unpack . winToText
        ,decodeStringOn = winFromText . T.pack}

winToText :: FilePath -> T.Text
winToText p =
  case pathRoot p of
    Just RootWindowsUnc{} -> uncToText p
    _ -> dosToText p

dosToText :: FilePath -> T.Text
dosToText p = T.concat (root' : chunks)
  where root' = rootText (pathRoot p)
        chunks =
          intersperse (T.pack "\\")
                      (map unescape' (directoryChunks p))

uncToText :: FilePath -> T.Text
uncToText p = T.concat (root' : chunks)
  where root' =
          if all T.null chunks
             then rootText (pathRoot p)
             else rootText (pathRoot p) `T.append`
                  T.pack "\\"
        chunks =
          intersperse
            (T.pack "\\")
            (filter (not . T.null)
                    (map unescape' (directoryChunks p)))

winFromText :: T.Text -> FilePath
winFromText text =
  if T.null text
     then empty
     else path
  where path =
          FilePath root' directories basename' exts
        -- Windows has various types of absolute paths:
        --
        -- * C:\foo\bar -> DOS-style absolute path
        -- * \\?\C:\foo\bar -> extended-length absolute path
        -- * \\host\share\foo\bar -> UNC path
        -- * \\?\UNC\host\share\foo\bar -> extended-length UNC path
        --
        -- \foo\bar looks like an absolute path, but is actually a path
        -- relative to the current DOS drive.
        --
        -- http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
        (root',pastRoot) =
          if T.isPrefixOf (T.pack "\\\\")
                          text
             then case stripUncasedPrefix (T.pack "\\\\?\\UNC\\")
                                          text of
                    Just stripped ->
                      parseUncRoot stripped True
                    Nothing ->
                      case T.stripPrefix (T.pack "\\\\?\\")
                                         text of
                        Just stripped ->
                          parseDosRoot stripped True
                        Nothing ->
                          case T.stripPrefix (T.pack "\\\\")
                                             text of
                            Just stripped ->
                              parseUncRoot stripped False
                            Nothing ->
                              parseDosRoot text False
             else case T.stripPrefix (T.pack "\\??\\")
                                     text of
                    Just stripped -> parseDoubleQmark stripped
                    Nothing ->
                      parseDosRoot text False
        (directories,filename')
          | P.null pastRoot = ([],Nothing)
          | otherwise =
            case last pastRoot of
              fn
                | fn ==
                    T.pack "." ->
                  (goodDirs pastRoot,Just "")
              fn
                | fn ==
                    T.pack ".." ->
                  (goodDirs pastRoot,Just "")
              fn ->
                (goodDirs (init pastRoot),Just (escape fn))
        goodDirs :: [T.Text] -> [Chunk]
        goodDirs =
          map escape .
          filter (not . T.null)
        (basename',exts) =
          case filename' of
            Just fn -> parseFilename fn
            Nothing -> (Nothing,[])

stripUncasedPrefix :: T.Text -> T.Text -> Maybe T.Text
stripUncasedPrefix prefix text =
  if T.toCaseFold prefix ==
     T.toCaseFold (T.take (T.length prefix) text)
     then Just (T.drop (T.length prefix) text)
     else Nothing

parseDosRoot :: T.Text -> Bool -> (Maybe Root, [T.Text])
parseDosRoot text extended = parsed
  where split =
          textSplitBy (\c -> c == '/' || c == '\\')
                      text
        head' = head split
        tail' = tail split
        parsed =
          if T.null head'
             then (Just RootWindowsCurrentVolume,tail')
             else if T.any (== ':') head'
                     then (Just (parseDrive head'),tail')
                     else (Nothing,split)
        parseDrive c =
          RootWindowsVolume (toUpper (T.head c))
                            extended

parseDoubleQmark :: T.Text -> (Maybe Root, [T.Text])
parseDoubleQmark text =
  (Just RootWindowsDoubleQMark,components)
  where components =
          textSplitBy (\c -> c == '/' || c == '\\')
                      text

parseUncRoot :: T.Text -> Bool -> (Maybe Root, [T.Text])
parseUncRoot text extended = parsed
  where (host,pastHost) = T.break (== '\\') text
        (share,pastShare) =
          T.break (== '\\') (T.drop 1 pastHost)
        split =
          if T.null pastShare
             then []
             else textSplitBy (== '\\') pastShare
        parsed =
          (Just (RootWindowsUnc (T.unpack host)
                                (T.unpack share)
                                extended)
          ,split)

winValid :: FilePath -> Bool
winValid p =
  case pathRoot p of
    Nothing -> dosValid p
    Just RootWindowsCurrentVolume -> dosValid p
    Just (RootWindowsVolume v _) ->
      elem v ['A' .. 'Z'] &&
      dosValid p
    Just (RootWindowsUnc host share _) ->
      uncValid p host share
    -- don't even try to validate \??\ paths
    Just RootWindowsDoubleQMark -> True
    Just RootPosix -> False

dosValid :: FilePath -> Bool
dosValid p = noReserved && validCharacters
  where reservedChars =
          map chr [0 .. 31] ++
          "/\\?*:|\"<>"
        reservedNames =
          ["AUX"
          ,"CLOCK$"
          ,"COM1"
          ,"COM2"
          ,"COM3"
          ,"COM4"
          ,"COM5"
          ,"COM6"
          ,"COM7"
          ,"COM8"
          ,"COM9"
          ,"CON"
          ,"LPT1"
          ,"LPT2"
          ,"LPT3"
          ,"LPT4"
          ,"LPT5"
          ,"LPT6"
          ,"LPT7"
          ,"LPT8"
          ,"LPT9"
          ,"NUL"
          ,"PRN"]
        noExt = p {pathExtensions = []}
        noReserved =
          flip all (directoryChunks noExt) $
          \fn ->
            notElem (map toUpper fn) reservedNames
        validCharacters =
          flip all (directoryChunks p) $
          not .
          any (`elem` reservedChars)

uncValid :: FilePath -> String -> String -> Bool
uncValid _ "" _ = False
uncValid _ _ "" = False
uncValid p host share =
  ok host &&
  ok share &&
  all ok
      (dropWhileEnd P.null
                    (directoryChunks p))
  where ok "" = False
        ok c = not (any invalidChar c)
        invalidChar c = c == '\x00' || c == '\\'

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p =
  foldr (\x xs ->
           if p x && P.null xs
              then []
              else x : xs)
        []

winSplit :: T.Text -> [FilePath]
winSplit =
  map winFromText .
  filter (not . T.null) .
  textSplitBy (== ';')

-------------------------------------------------------------------------------
-- File Paths
-------------------------------------------------------------------------------

type Chunk = String
type Directory = Chunk
type Basename = Chunk
type Extension = Chunk

data Root
  = RootPosix
  | RootWindowsVolume Char
                      Bool
  | RootWindowsCurrentVolume
  | RootWindowsUnc String
                   String
                   Bool
  | RootWindowsDoubleQMark
  deriving (Eq,Ord,Data,Typeable,Show)

data FilePath =
  FilePath {pathRoot :: Maybe Root
           ,pathDirectories :: [Directory]
           ,pathBasename :: Maybe Basename
           ,pathExtensions :: [Extension]}
  deriving (Data,Typeable)

instance Eq FilePath where
  x == y =
    compare x y ==
    EQ

instance Ord FilePath where
  compare =
    comparing (\p ->
                 (pathRoot p
                 ,fmap unescape' (pathDirectories p)
                 ,fmap unescape' (pathBasename p)
                 ,fmap unescape' (pathExtensions p)))

instance NFData Root where
  rnf (RootWindowsVolume c extended) = rnf c `seq` rnf extended
  rnf (RootWindowsUnc host share extended) = rnf host `seq` rnf share `seq`
                                                            rnf extended
  rnf _ = ()

instance NFData FilePath where
  rnf p =
    rnf (pathRoot p) `seq`
    rnf (pathDirectories p) `seq`
    rnf (pathBasename p) `seq`
    rnf (pathExtensions p)

instance IsString FilePath where
  fromString = fromText . T.pack

instance Show FilePath where
  showsPrec d path =
    showParen (d > 10)
              (ss "FilePath " .
               s txt)
    where s = shows
          ss = showString
          txt = either id id (toText path)

instance Monoid FilePath where
  mempty = empty
  mappend = append
  mconcat = concat

-- | A file path with no root, directory, or filename
empty :: FilePath
empty = FilePath Nothing [] Nothing []

dot :: Chunk
dot = "."

dots :: Chunk
dots = ".."

filenameChunk :: FilePath -> Chunk
filenameChunk p = P.concat (name : exts)
  where name = maybe "" id (pathBasename p)
        exts =
          case pathExtensions p of
            [] -> []
            exts' ->
              intersperse dot
                          ("" : exts')

rootChunk :: Maybe Root -> Chunk
rootChunk r =
  flip (maybe "")
       r
       (\r' ->
          case r' of
            RootPosix -> "/"
            RootWindowsVolume c False -> c : ":\\"
            RootWindowsVolume c True ->
              "\\\\?\\" ++
              (c : ":\\")
            RootWindowsCurrentVolume -> "\\"
            RootWindowsUnc host share False -> "\\\\" ++ host ++ "\\" ++ share
            RootWindowsUnc host share True -> "\\\\?\\UNC\\" ++ host ++ "\\" ++
                                                                        share
            RootWindowsDoubleQMark -> "\\??\\")

rootText :: Maybe Root -> T.Text
rootText = T.pack . rootChunk

directoryChunks :: FilePath -> [Chunk]
directoryChunks path = pathDirectories path ++ [filenameChunk path]

escape :: T.Text -> Chunk
escape t = T.unpack t

unescape :: Chunk -> (T.Text, Bool)
unescape cs =
  if any (\c -> ord c >= 56448 && ord c <= 56575) cs
     then (T.pack (map (\c ->
                          if ord c >= 56448 && ord c <= 56575
                             then chr (ord c - 56320)
                             else c)
                       cs)
          ,False)
     else (T.pack cs,True)

unescape' :: Chunk -> T.Text
unescape' = fst . unescape

unescapeBytes' :: Chunk -> B.ByteString
unescapeBytes' cs =
  if any (\c -> ord c >= 56448 && ord c <= 56575) cs
     then B8.concat (map (\c ->
                            if ord c >= 56448 && ord c <= 56575
                               then B8.singleton (chr (ord c - 56320))
                               else TE.encodeUtf8 (T.singleton c))
                         cs)
     else TE.encodeUtf8 (T.pack cs)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = loop
  where loop xs =
          let (chunk,rest) = break p xs
              cont =
                chunk :
                loop (tail rest)
          in if P.null rest
                then [chunk]
                else cont

textSplitBy :: (Char -> Bool) -> T.Text -> [T.Text]
#if MIN_VERSION_text(0,11,0)
textSplitBy = T.split
#else
textSplitBy = T.splitBy
#endif

parseFilename :: Chunk -> (Maybe Basename, [Extension])
parseFilename p = parsed
  where parsed =
          if P.null p
             then (Nothing,[])
             else case span (== '.') p of
                    (leadingDots,baseAndExts) ->
                      case splitBy (== '.') baseAndExts of
                        [] ->
                          (joinDots leadingDots "",[])
                        (name':exts') ->
                          (joinDots leadingDots name',exts')
        joinDots leadingDots base =
          case leadingDots ++ base of
            [] -> Nothing
            joined -> Just joined

maybeDecodeUtf8 :: B.ByteString -> Maybe T.Text
maybeDecodeUtf8 bytes =
  case TE.decodeUtf8' bytes of
    Left _ -> Nothing
    Right text -> Just text
