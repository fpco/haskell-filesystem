-- |
-- Module: Filesystem.Path.Rules
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
module Filesystem.Path.Rules
  ( Rules
  , posix
  , posix_ghc702
  , posix_ghc704
  , windows
  , darwin
  , darwin_ghc702

  -- * Type conversions
  , toText
  , fromText
  , encode
  , decode
  , encodeString
  , decodeString

  -- * Rule&#x2010;specific path properties
  , valid
  , splitSearchPath
  , splitSearchPathString
  ) where

import           Prelude hiding (FilePath, null)
import qualified Prelude as P

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (toUpper, chr, ord)
import           Data.List (intersperse, intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.IO ()

import           Filesystem.Path hiding (root, filename, basename)
import           Filesystem.Path.Internal

-------------------------------------------------------------------------------
-- POSIX
-------------------------------------------------------------------------------

-- | Linux, BSD, and other UNIX or UNIX-like operating systems.
posix :: Rules B.ByteString
posix = Rules
  { rulesName = T.pack "POSIX"
  , valid = posixValid
  , splitSearchPath = posixSplitSearch
  , splitSearchPathString = posixSplitSearch . B8.pack
  , toText = posixToText
  , fromText = posixFromText
  , encode = posixToBytes
  , decode = posixFromBytes
  , encodeString = B8.unpack . posixToBytes
  , decodeString = posixFromBytes . B8.pack
  }

-- | Linux, BSD, and other UNIX or UNIX-like operating systems.
--
-- This is a variant of 'posix' for use with GHC 7.2, which tries to decode
-- file paths in its IO computations.
--
-- Since: 0.3.3 / 0.4.2
posix_ghc702 :: Rules B.ByteString
posix_ghc702 = posix
  { rulesName = T.pack "POSIX (GHC 7.2)"
  , splitSearchPathString = posixSplitSearchString posixFromGhc702String
  , encodeString = posixToGhc702String
  , decodeString = posixFromGhc702String
  }

-- | Linux, BSD, and other UNIX or UNIX-like operating systems.
--
-- This is a variant of 'posix' for use with GHC 7.4 or later, which tries to
-- decode file paths in its IO computations.
--
-- Since: 0.3.7 / 0.4.6
posix_ghc704 :: Rules B.ByteString
posix_ghc704 = posix
  { rulesName = T.pack "POSIX (GHC 7.4)"
  , splitSearchPathString = posixSplitSearchString posixFromGhc704String
  , encodeString = posixToGhc704String
  , decodeString = posixFromGhc704String
  }

posixToText :: FilePath -> Either T.Text T.Text
posixToText p = if good then Right text else Left text where
  good = and (map snd chunks)
  text = T.concat (root : map fst chunks)

  root = rootText (pathRoot p)
  chunks = intersperse (T.pack "/", True) (map unescape (directoryChunks p))

posixFromChunks :: [Chunk] -> FilePath
posixFromChunks chunks = FilePath root directories basename exts where
  (root, pastRoot) = if P.null (head chunks)
    then (Just RootPosix, tail chunks)
    else (Nothing, chunks)

  (directories, filename)
    | P.null pastRoot = ([], "")
    | otherwise = case last pastRoot of
      fn | fn == dot -> (goodDirs pastRoot, "")
      fn | fn == dots -> (goodDirs pastRoot, "")
      fn -> (goodDirs (init pastRoot), fn)

  goodDirs = filter (not . P.null)

  (basename, exts) = parseFilename filename

posixFromText :: T.Text -> FilePath
posixFromText text = if T.null text
  then empty
  else posixFromChunks (map escape (textSplitBy (== '/') text))

posixToBytes :: FilePath -> B.ByteString
posixToBytes p = B.concat (root : chunks) where
  root = B8.pack (rootChunk (pathRoot p))
  chunks = intersperse (B8.pack "/") (map chunkBytes (directoryChunks p))
  chunkBytes c = unescapeBytes' c

posixFromBytes :: B.ByteString -> FilePath
posixFromBytes bytes = if B.null bytes
  then empty
  else posixFromChunks $ flip map (B.split 0x2F bytes) $ \b -> case maybeDecodeUtf8 b of
    Just text -> escape text
    Nothing -> processInvalidUtf8 b

processInvalidUtf8 :: B.ByteString -> Chunk
processInvalidUtf8 bytes = intercalate "." textChunks where
  byteChunks = B.split 0x2E bytes
  textChunks = map unicodeDammit byteChunks
  unicodeDammit b = case maybeDecodeUtf8 b of
    Just t -> escape t
    Nothing -> map (\c -> if ord c >= 0x80
      then chr (ord c + 0xDC00)
      else c) (B8.unpack b)

posixToGhc702String :: FilePath -> String
posixToGhc702String p = P.concat (root : chunks) where
  root = rootChunk (pathRoot p)
  chunks = intersperse "/" (map escapeToGhc702 (directoryChunks p))

escapeToGhc702 :: Chunk -> String
escapeToGhc702 = map (\c -> if ord c >= 0xDC80 && ord c <= 0xDCFF
  then chr (ord c - 0xDC00 + 0xEF00)
  else c)

posixFromGhc702String :: String -> FilePath
posixFromGhc702String cs = if P.null cs
  then empty
  else posixFromChunks (map escapeFromGhc702 (splitBy (== '/') cs))

escapeFromGhc702 :: String -> String
escapeFromGhc702 = map (\c -> if ord c >= 0xEF80 && ord c <= 0xEFFF
  -- hopefully this isn't a valid UTF8 filename decoding to these
  -- codepoints, but there's no way to tell here.
  then chr (ord c - 0xEF00 + 0xDC00)
  else c)

posixToGhc704String :: FilePath -> String
posixToGhc704String p = P.concat (root : chunks) where
  root = rootChunk (pathRoot p)
  chunks = intersperse "/" (directoryChunks p)

posixFromGhc704String :: String -> FilePath
posixFromGhc704String cs = if P.null cs
  then empty
  else posixFromChunks (splitBy (== '/') cs)

posixValid :: FilePath -> Bool
posixValid p = validRoot && validDirectories where
  validDirectories = all validChunk (directoryChunks p)
  validChunk ch = not (any (\c -> c == '\0' || c == '/') ch)
  validRoot = case pathRoot p of
    Nothing -> True
    Just RootPosix -> True
    _ -> False

posixSplitSearch :: B.ByteString -> [FilePath]
posixSplitSearch = map (posixFromBytes . normSearch) . B.split 0x3A where
  normSearch bytes = if B.null bytes then B8.pack "." else bytes

posixSplitSearchString :: (String -> FilePath) -> String -> [FilePath]
posixSplitSearchString toPath = map (toPath . normSearch) . splitBy (== ':') where
  normSearch s = if P.null s then "." else s

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
darwin = Rules
  { rulesName = T.pack "Darwin"
  , valid = posixValid
  , splitSearchPath = darwinSplitSearch
  , splitSearchPathString = darwinSplitSearch . TE.decodeUtf8 . B8.pack
  , toText = Right . darwinToText
  , fromText = posixFromText
  , encode = darwinToText
  , decode = posixFromText
  , encodeString = darwinToString
  , decodeString = darwinFromString
  }

-- | Darwin and Mac OS X.
--
-- This is a variant of 'darwin' for use with GHC 7.2 or later, which tries to
-- decode file paths in its IO computations.
--
-- Since: 0.3.4 / 0.4.3
darwin_ghc702 :: Rules T.Text
darwin_ghc702 = darwin
  { rulesName = T.pack "Darwin (GHC 7.2)"
  , splitSearchPathString = darwinSplitSearch . T.pack
  , encodeString = T.unpack . darwinToText
  , decodeString = posixFromText . T.pack
  }

darwinToText :: FilePath -> T.Text
darwinToText p = T.concat (root : chunks) where
  root = rootText (pathRoot p)
  chunks = intersperse (T.pack "/") (map unescape' (directoryChunks p))

darwinToString :: FilePath -> String
darwinToString = B8.unpack . TE.encodeUtf8 . darwinToText

darwinFromString :: String -> FilePath
darwinFromString = posixFromText . TE.decodeUtf8 . B8.pack

darwinSplitSearch :: T.Text -> [FilePath]
darwinSplitSearch = map (posixFromText . normSearch) . textSplitBy (== ':') where
  normSearch text = if T.null text then T.pack "." else text

-------------------------------------------------------------------------------
-- Windows
-------------------------------------------------------------------------------

-- | Windows and DOS
windows :: Rules T.Text
windows = Rules
  { rulesName = T.pack "Windows"
  , valid = winValid
  , splitSearchPath = winSplit
  , splitSearchPathString = winSplit . T.pack
  , toText = Right . winToText
  , fromText = winFromText
  , encode = winToText
  , decode = winFromText
  , encodeString = T.unpack . winToText
  , decodeString = winFromText . T.pack
  }

winToText :: FilePath -> T.Text
winToText p = case pathRoot p of
  Just RootWindowsUnc{} -> uncToText p
  _ -> dosToText p

dosToText :: FilePath -> T.Text
dosToText p = T.concat (root : chunks) where
  root = rootText (pathRoot p)
  chunks = intersperse (T.pack "\\") (map unescape' (directoryChunks p))

uncToText :: FilePath -> T.Text
uncToText p = T.concat (root : chunks) where
  root = if all T.null chunks
    then rootText (pathRoot p)
    else rootText (pathRoot p) `T.append` T.pack "\\"
  chunks = intersperse (T.pack "\\") (filter (not . T.null) (map unescape' (directoryChunks p)))

winFromText :: T.Text -> FilePath
winFromText text = if T.null text then empty else path where
  path = FilePath root directories basename exts

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
  (root, pastRoot) = if T.isPrefixOf (T.pack "\\\\") text
    then case stripUncasedPrefix (T.pack "\\\\?\\UNC\\") text of
      Just stripped -> parseUncRoot stripped True
      Nothing -> case T.stripPrefix (T.pack "\\\\?\\") text of
        Just stripped -> parseDosRoot stripped True
        Nothing -> case T.stripPrefix (T.pack "\\\\") text of
          Just stripped -> parseUncRoot stripped False
          Nothing -> parseDosRoot text False
    else case T.stripPrefix (T.pack "\\??\\") text of
      Just stripped -> parseDoubleQmark stripped
      Nothing -> parseDosRoot text False

  (directories, filename)
    | P.null pastRoot = ([], Nothing)
    | otherwise = case last pastRoot of
      fn | fn == T.pack "." -> (goodDirs pastRoot, Just "")
      fn | fn == T.pack ".." -> (goodDirs pastRoot, Just "")
      fn -> (goodDirs (init pastRoot), Just (escape fn))

  goodDirs :: [T.Text] -> [Chunk]
  goodDirs = map escape . filter (not . T.null)

  (basename, exts) = case filename of
    Just fn -> parseFilename fn
    Nothing -> (Nothing, [])

stripUncasedPrefix :: T.Text -> T.Text -> Maybe T.Text
stripUncasedPrefix prefix text = if T.toCaseFold prefix == T.toCaseFold (T.take (T.length prefix) text)
  then Just (T.drop (T.length prefix) text)
  else Nothing

parseDosRoot :: T.Text -> Bool -> (Maybe Root, [T.Text])
parseDosRoot text extended = parsed where
  split = textSplitBy (\c -> c == '/' || c == '\\') text

  head' = head split
  tail' = tail split
  parsed = if T.null head'
    then (Just RootWindowsCurrentVolume, tail')
    else if T.any (== ':') head'
      then (Just (parseDrive head'), tail')
        else (Nothing, split)

  parseDrive c = RootWindowsVolume (toUpper (T.head c)) extended

parseDoubleQmark :: T.Text -> (Maybe Root, [T.Text])
parseDoubleQmark text = (Just RootWindowsDoubleQMark, components) where
  components = textSplitBy (\c -> c == '/' || c == '\\') text

parseUncRoot :: T.Text -> Bool -> (Maybe Root, [T.Text])
parseUncRoot text extended = parsed where
  (host, pastHost) = T.break (== '\\') text
  (share, pastShare) = T.break (== '\\') (T.drop 1 pastHost)
  split = if T.null pastShare
    then []
    else textSplitBy (== '\\') pastShare
  parsed = (Just (RootWindowsUnc (T.unpack host) (T.unpack share) extended), split)

winValid :: FilePath -> Bool
winValid p = case pathRoot p of
  Nothing -> dosValid p
  Just RootWindowsCurrentVolume -> dosValid p
  Just (RootWindowsVolume v _) -> elem v ['A'..'Z'] && dosValid p
  Just (RootWindowsUnc host share _) -> uncValid p host share
  -- don't even try to validate \??\ paths
  Just RootWindowsDoubleQMark -> True
  Just RootPosix -> False

dosValid :: FilePath -> Bool
dosValid p = noReserved && validCharacters where
  reservedChars = map chr [0..0x1F] ++ "/\\?*:|\"<>"
  reservedNames =
    [ "AUX", "CLOCK$", "COM1", "COM2", "COM3", "COM4"
    , "COM5", "COM6", "COM7", "COM8", "COM9", "CON"
    , "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6"
    , "LPT7", "LPT8", "LPT9", "NUL", "PRN"
    ]

  noExt = p { pathExtensions = [] }
  noReserved = flip all (directoryChunks noExt)
    $ \fn -> notElem (map toUpper fn) reservedNames

  validCharacters = flip all (directoryChunks p)
    $ not . any (`elem` reservedChars)

uncValid :: FilePath -> String -> String -> Bool
uncValid _ "" _ = False
uncValid _ _ "" = False
uncValid p host share = ok host && ok share && all ok (dropWhileEnd P.null (directoryChunks p)) where
  ok ""  = False
  ok c = not (any invalidChar c)
  invalidChar c = c == '\x00' || c == '\\'

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && P.null xs then [] else x : xs) []

winSplit :: T.Text -> [FilePath]
winSplit = map winFromText . filter (not . T.null) . textSplitBy (== ';')
