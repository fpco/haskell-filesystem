{-# LANGUAGE CPP #-}
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

import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Filesystem.Path.Internal as FPI
import qualified System.FilePath as SF
import           Prelude hiding (FilePath, null, concat)

newtype FilePath =
  FilePath {unFilePath :: String}

instance Eq FilePath where
  (==) a b = toIFP a == toIFP b

instance IsString FilePath where
  fromString = FilePath

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
toText = FPI.toText . toIFP

fromText :: T.Text -> FilePath
fromText = fromIFP . FPI.fromText

valid :: FilePath -> Bool
valid = FPI.valid . toIFP

splitSearchPath :: PLATFORM_PATH_FORMAT -> [FilePath]
splitSearchPath = map fromIFP . FPI.splitSearchPath

splitSearchPathString :: String -> [FilePath]
splitSearchPathString = map fromIFP . FPI.splitSearchPathString

encode :: FilePath -> PLATFORM_PATH_FORMAT
encode = FPI.encode . toIFP

decode :: PLATFORM_PATH_FORMAT -> FilePath
decode = fromIFP . FPI.decode

encodeString :: FilePath -> String
encodeString = FPI.encodeString . toIFP

decodeString :: String -> FilePath
decodeString = fromIFP . FPI.decodeString

validOn :: forall a.
           Platform a -> FilePath -> Bool
validOn p = FPI.validOn (unPlatform p) . toIFP

splitSearchPathOn :: forall a.
                     Platform a -> a -> [FilePath]
splitSearchPathOn p = map fromIFP . FPI.splitSearchPathOn (unPlatform p)

splitSearchPathStringOn :: forall a.
                           Platform a -> String -> [FilePath]
splitSearchPathStringOn p = map fromIFP . FPI.splitSearchPathStringOn (unPlatform p)

toTextOn :: forall a.
            Platform a -> FilePath -> Either T.Text T.Text
toTextOn p = FPI.toTextOn (unPlatform p) . toIFP

fromTextOn :: forall a.
              Platform a -> T.Text -> FilePath
fromTextOn p = fromIFP . FPI.fromTextOn (unPlatform p)

encodeOn :: forall a.
            Platform a -> FilePath -> a
encodeOn p = FPI.encodeOn (unPlatform p) . toIFP

decodeOn :: forall a. Platform a -> a -> FilePath
decodeOn p = fromIFP . FPI.decodeOn (unPlatform p)

encodeStringOn :: forall a.
                  Platform a -> FilePath -> String
encodeStringOn p = FPI.encodeStringOn (unPlatform p) . toIFP

decodeStringOn :: forall a.
                  Platform a -> String -> FilePath
decodeStringOn p = fromIFP . FPI.decodeStringOn (unPlatform p)

empty :: FilePath
empty = FilePath []

null :: FilePath -> Bool
null = FPI.null . toIFP

root :: FilePath -> FilePath
root = fromIFP . FPI.root . toIFP

directory :: FilePath -> FilePath
directory = fromIFP . FPI.directory . toIFP

parent :: FilePath -> FilePath
parent = fromIFP . FPI.parent . toIFP

filename :: FilePath -> FilePath
filename = fromIFP . FPI.filename . toIFP

dirname :: FilePath -> FilePath
dirname = fromIFP . FPI.dirname . toIFP

basename :: FilePath -> FilePath
basename = fromIFP . FPI.basename . toIFP

absolute :: FilePath -> Bool
absolute = FPI.absolute . toIFP

relative :: FilePath -> Bool
relative = FPI.relative . toIFP

append :: FilePath -> FilePath -> FilePath
append x y = fromIFP (FPI.append (toIFP x) (toIFP y))

(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

concat :: [FilePath] -> FilePath
concat = fromIFP . FPI.concat . map toIFP

commonPrefix :: [FilePath] -> FilePath
commonPrefix = fromIFP . FPI.commonPrefix . map toIFP

stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix x y = fmap fromIFP (FPI.stripPrefix (toIFP x) (toIFP y))

collapse :: FilePath -> FilePath
collapse = fromIFP . FPI.collapse . toIFP

splitDirectories :: FilePath -> [FilePath]
splitDirectories = map fromIFP . FPI.splitDirectories . toIFP

extension :: FilePath -> Maybe T.Text
extension = FPI.extension . toIFP

extensions :: FilePath -> [T.Text]
extensions = FPI.extensions . toIFP

hasExtension :: FilePath -> T.Text -> Bool
hasExtension = FPI.hasExtension . toIFP

addExtension :: FilePath -> T.Text -> FilePath
addExtension x y = fromIFP (FPI.addExtension (toIFP x) y)

addExtensions :: FilePath -> [T.Text] -> FilePath
addExtensions x ys = fromIFP (FPI.addExtensions (toIFP x) ys)

(<.>) :: FilePath -> T.Text -> FilePath
(<.>) = addExtension

dropExtension :: FilePath -> FilePath
dropExtension = fromIFP . FPI.dropExtension . toIFP

dropExtensions :: FilePath -> FilePath
dropExtensions = fromIFP . FPI.dropExtensions . toIFP

replaceExtension :: FilePath -> T.Text -> FilePath
replaceExtension x y = fromIFP (FPI.replaceExtension (toIFP x) y)

replaceExtensions :: FilePath -> [T.Text] -> FilePath
replaceExtensions x ys = fromIFP (FPI.replaceExtensions (toIFP x) ys)

splitExtension :: FilePath -> (FilePath, Maybe T.Text)
splitExtension x =
  let split = FPI.splitExtension (toIFP x)
  in (fromIFP (fst split),snd split)

splitExtensions :: FilePath -> (FilePath, [T.Text])
splitExtensions x =
  let split = FPI.splitExtensions (toIFP x)
  in (fromIFP (fst split),snd split)

--
-- Internal
--

toIFP :: FilePath -> FPI.FilePath
toIFP = FPI.decodeString . unFilePath

fromIFP :: FPI.FilePath -> FilePath
fromIFP = FilePath . FPI.encodeString
