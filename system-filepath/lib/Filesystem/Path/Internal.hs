{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module: Filesystem.Path.Internal
-- Copyright: 2010 John Millikin, 2015 FPComplete
-- License: MIT
--
-- Maintainer: dev@fpcomplete.com
-- Portability: portable
--
module Filesystem.Path.Internal where

import           Control.DeepSeq (NFData, rnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (chr, ord)
import           Data.Data (Data)
import           Data.List (intersperse)
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable (Typeable)
import           Prelude hiding (FilePath)

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

-- | A file path with no root, directory, or filename
empty :: FilePath
empty = FilePath Nothing [] Nothing []

dot :: Chunk
dot = "."

dots :: Chunk
dots = ".."

filenameChunk :: FilePath -> Chunk
filenameChunk p = concat (name : exts)
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
          in if null rest
                then [chunk]
                else cont

textSplitBy :: (Char -> Bool) -> T.Text -> [T.Text]
#if MIN_VERSION_text(0,11,0)
textSplitBy = T.split
#else
textSplitBy = T.splitBy
#endif

parseFilename :: Chunk -> (Maybe Basename, [Extension])
parseFilename filename = parsed
  where parsed =
          if null filename
             then (Nothing,[])
             else case span (== '.') filename of
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
