{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: 2015 FP Complete
-- License: MIT
-- Maintainer: dev@fpcomplete.com
-- Module: Filesystem.Path
-- Portability: portable
--
module Filesystem.Path
       ( FilePath
       , Platform
       , (<.>)
       , (</>)
       , posix
       , posix_ghc702
       , posix_ghc704
       , darwin
       , darwin_ghc702
       , windows
       , toText
       , fromText
       , valid
       , splitSearchPath
       , splitSearchPathString
       , encode
       , decode
       , encodeString
       , decodeString
       , validOn
       , splitSearchPathOn
       , splitSearchPathStringOn
       , toTextOn
       , fromTextOn
       , encodeOn
       , decodeOn
       , encodeStringOn
       , decodeStringOn
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

import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Filesystem.Path.Internal as FPI
import qualified System.FilePath as SF
import qualified System.FilePath.Windows as Windows
import qualified System.FilePath.Posix as Posix
import           Prelude hiding (FilePath, null, concat)
import qualified Prelude as P

newtype FilePath =
  FilePath {unFilePath :: SF.FilePath}

instance Eq FilePath where
  (==) a b = toIFP a == toIFP b

instance IsString FilePath where
  fromString = FilePath -- TODO: . SF.normalise

instance Monoid FilePath where
  mempty = empty
  mappend = append
  mconcat = concat

instance Ord FilePath where
  compare a b =
    compare (toIFP a)
            (toIFP b)

instance Show FilePath where
  show = unFilePath

newtype Platform a = Platform {unPlatform :: FPI.Rules a}
  deriving (Show)

#if defined(__HADDOCK__)
#  define PLATFORM_PATH_FORMAT platformTextFormat
#elif defined(CABAL_OS_WINDOWS) || defined(CABAL_OS_DARWIN)
#  define PLATFORM_PATH_FORMAT T.Text
#else
#  define PLATFORM_PATH_FORMAT B.ByteString
#endif

windows :: Platform T.Text
windows = Platform FPI.windows

darwin :: Platform T.Text
darwin = Platform FPI.darwin

darwin_ghc702 :: Platform T.Text
darwin_ghc702 = Platform FPI.darwin_ghc702

posix :: Platform B.ByteString
posix = Platform FPI.posix

posix_ghc702 :: Platform B.ByteString
posix_ghc702 = Platform FPI.posix_ghc702

posix_ghc704 :: Platform B.ByteString
posix_ghc704 = Platform FPI.posix_ghc704

toText :: FilePath -> Either T.Text T.Text
toText = FPI.toText . toIFP -- TODO impl custom replacement

fromText :: T.Text -> FilePath
fromText = fromIFP . FPI.fromText -- TODO impl custom replacement

valid :: FilePath -> Bool
valid = SF.isValid . unFilePath

splitSearchPath :: PLATFORM_PATH_FORMAT -> [FilePath]
splitSearchPath = map fromIFP . FPI.splitSearchPath -- TODO impl custom replacement

splitSearchPathString :: String -> [FilePath]
splitSearchPathString = map fromIFP . FPI.splitSearchPathString -- TODO impl custom replacement

encode :: FilePath -> PLATFORM_PATH_FORMAT
encode = FPI.encode . toIFP -- TODO impl custom replacement

decode :: PLATFORM_PATH_FORMAT -> FilePath
decode = fromIFP . FPI.decode -- TODO impl custom replacement

encodeString :: FilePath -> String
encodeString = FPI.encodeString . toIFP -- TODO impl custom replacement

decodeString :: String -> FilePath
decodeString = fromIFP . FPI.decodeString -- TODO impl custom replacement

validOn :: forall a.
           Platform a -> FilePath -> Bool
validOn p = FPI.validOn (unPlatform p) . toIFP -- TODO impl custom replacement
-- validOn _p | _ = Windows.isValid . unFilePath
-- validOn _p | _ = Posix.isValid . unFilePath

splitSearchPathOn :: forall a.
                     Platform a -> a -> [FilePath]
splitSearchPathOn p = map fromIFP . FPI.splitSearchPathOn (unPlatform p) -- TODO impl custom replacement

splitSearchPathStringOn :: forall a.
                           Platform a -> String -> [FilePath]
splitSearchPathStringOn p = map fromIFP . FPI.splitSearchPathStringOn (unPlatform p) -- TODO impl custom replacement

toTextOn :: forall a.
            Platform a -> FilePath -> Either T.Text T.Text
toTextOn p = FPI.toTextOn (unPlatform p) . toIFP -- TODO impl custom replacement

fromTextOn :: forall a.
              Platform a -> T.Text -> FilePath
fromTextOn p = fromIFP . FPI.fromTextOn (unPlatform p) -- TODO impl custom replacement

encodeOn :: forall a.
            Platform a -> FilePath -> a
encodeOn p = FPI.encodeOn (unPlatform p) . toIFP -- TODO impl custom replacement

decodeOn :: forall a. Platform a -> a -> FilePath
decodeOn p = fromIFP . FPI.decodeOn (unPlatform p) -- TODO impl custom replacement

encodeStringOn :: forall a.
                  Platform a -> FilePath -> String
encodeStringOn p = FPI.encodeStringOn (unPlatform p) . toIFP -- TODO impl custom replacement

decodeStringOn :: forall a.
                  Platform a -> String -> FilePath
decodeStringOn p = fromIFP . FPI.decodeStringOn (unPlatform p) -- TODO impl custom replacement

empty :: FilePath
empty = fromString ""

null :: FilePath -> Bool
null = FPI.null . toIFP -- TODO impl custom replacement

root :: FilePath -> FilePath
root = fromIFP . FPI.root . toIFP -- TODO impl custom replacement

directory :: FilePath -> FilePath
directory =
  fromString .
  (SF.addTrailingPathSeparator . SF.takeDirectory) .
  unFilePath

parent :: FilePath -> FilePath
parent = fromIFP . FPI.parent . toIFP -- TODO impl custom replacement

filename :: FilePath -> FilePath
filename = fromString . SF.takeFileName . unFilePath

dirname :: FilePath -> FilePath
dirname = fromIFP . FPI.dirname . toIFP
-- dirname p =
--   case reverse (splitDirectories p) of
--     [] -> p
--     (dir:_) -> dir

basename :: FilePath -> FilePath
basename = fromString . SF.takeBaseName . unFilePath

absolute :: FilePath -> Bool
absolute = FPI.absolute . toIFP

relative :: FilePath -> Bool
relative = FPI.relative . toIFP

append :: FilePath -> FilePath -> FilePath
append x y = fromIFP (FPI.append (toIFP x) (toIFP y))
  -- fromString (SF.combine (unFilePath x) (unFilePath y))
  {-
     FIXME:
     system-filepath: "a" </> "" == "a/"
     filepath: "a" </> "" == "a"
  -}

(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

concat :: [FilePath] -> FilePath
concat = foldl append empty

commonPrefix :: [FilePath] -> FilePath
commonPrefix = fromIFP . FPI.commonPrefix . map toIFP -- TODO impl custom replacement

stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix x y = fmap fromIFP (FPI.stripPrefix (toIFP x) (toIFP y)) -- TODO impl custom replacement

collapse :: FilePath -> FilePath
collapse = fromIFP . FPI.collapse . toIFP -- TODO impl custom replacement

splitDirectories :: FilePath -> [FilePath]
splitDirectories = map fromIFP . FPI.splitDirectories . toIFP
  -- map fromString . SF.splitDirectories . unFilePath
  {-
     FIXME:
     system-filepath: splitDirectories "/ab/cd.txt" == ["/","ab/","cd.txt"]
     filepath: splitDirectories "/ab/cd.txt" == ["/","ab","cd.txt"]
  -}

extension :: FilePath -> Maybe T.Text
extension p =
  case reverse (extensions p) of
    [] -> Nothing
    (x:_) -> Just x

extensions :: FilePath -> [T.Text]
extensions p =
  let exts = SF.takeExtensions (unFilePath p)
      suffix =
        stripPrefix (basename p)
                    (fromString exts)
  in filterEmpty
       (splitOnExtSeparator
          (T.pack (case suffix of
                     Nothing -> exts
                     Just suffix' -> unFilePath suffix')))

hasExtension :: FilePath -> T.Text -> Bool
hasExtension p e = extension p == Just e

addExtension :: FilePath -> T.Text -> FilePath
addExtension x y =
  fromString
    (SF.addExtension (unFilePath x)
                     (T.unpack y))

addExtensions :: FilePath -> [T.Text] -> FilePath
addExtensions = foldl addExtension

(<.>) :: FilePath -> T.Text -> FilePath
(<.>) = addExtension

dropExtension :: FilePath -> FilePath
dropExtension = fromString . SF.dropExtension . unFilePath

dropExtensions :: FilePath -> FilePath
dropExtensions = fromString . SF.dropExtensions . unFilePath

replaceExtension :: FilePath -> T.Text -> FilePath
replaceExtension x y =
  fromString
    (SF.replaceExtension (unFilePath x)
                         (T.unpack y))

replaceExtensions :: FilePath -> [T.Text] -> FilePath
replaceExtensions x ys =
  foldl replaceExtension x ys

splitExtension :: FilePath -> (FilePath, Maybe T.Text)
splitExtension p =
  let (path,exts) =
        splitExtensions (fromString (unFilePath p))
  in case reverse exts of
       [] -> (path,Nothing)
       (x:xs) ->
         (addExtensions path
                        (reverse xs)
         ,Just x)

splitExtensions :: FilePath -> (FilePath, [T.Text])
splitExtensions p =
  let (path,exts) =
        SF.splitExtensions (unFilePath p)
  in (fromString path,filterEmpty (splitOnExtSeparator (fromString exts)))

--
-- Internal
--

toIFP :: FilePath -> FPI.FilePath
toIFP = FPI.decodeString . unFilePath

fromIFP :: FPI.FilePath -> FilePath
fromIFP = fromString . FPI.encodeString

splitOnExtSeparator :: T.Text -> [T.Text]
splitOnExtSeparator = T.splitOn (T.pack [SF.extSeparator])

filterEmpty :: [T.Text] -> [T.Text]
filterEmpty = filter (T.empty /=)
