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
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Filesystem.Path.Internal as I
import           Prelude hiding (FilePath, null, concat)

newtype FilePath =
  FilePath {unFilePath :: B.ByteString}

instance Eq FilePath where
  (==) a b = toIFP a == toIFP b

instance IsString FilePath where
  fromString = FilePath . BC.pack

instance Monoid FilePath where
  mempty = empty
  mappend = append
  mconcat = concat

instance Ord FilePath where
  compare a b =
    compare (toIFP a)
            (toIFP b)

instance Show FilePath where
  show = BC.unpack . unFilePath

newtype Platform a = Platform {unPlatform :: I.Rules a}
  deriving (Show)

#if defined(__HADDOCK__)
#  define PLATFORM_PATH_FORMAT platformTextFormat
#elif defined(CABAL_OS_WINDOWS) || defined(CABAL_OS_DARWIN)
#  define PLATFORM_PATH_FORMAT T.Text
#else
#  define PLATFORM_PATH_FORMAT B.ByteString
#endif

windows :: Platform T.Text
windows = Platform I.windows

darwin :: Platform T.Text
darwin = Platform I.darwin

darwin_ghc702 :: Platform T.Text
darwin_ghc702 = Platform I.darwin_ghc702

posix :: Platform B.ByteString
posix = Platform I.posix

posix_ghc702 :: Platform B.ByteString
posix_ghc702 = Platform I.posix_ghc702

posix_ghc704 :: Platform B.ByteString
posix_ghc704 = Platform I.posix_ghc704

toText :: FilePath -> Either T.Text T.Text
toText = I.toText . toIFP

fromText :: T.Text -> FilePath
fromText = fromIFP . I.fromText

valid :: FilePath -> Bool
valid = I.valid . toIFP

splitSearchPath :: PLATFORM_PATH_FORMAT -> [FilePath]
splitSearchPath = map fromIFP . I.splitSearchPath

splitSearchPathString :: String -> [FilePath]
splitSearchPathString = map fromIFP . I.splitSearchPathString

encode :: FilePath -> PLATFORM_PATH_FORMAT
encode = I.encode . toIFP

decode :: PLATFORM_PATH_FORMAT -> FilePath
decode = fromIFP . I.decode

encodeString :: FilePath -> String
encodeString = I.encodeString . toIFP

decodeString :: String -> FilePath
decodeString = fromIFP . I.decodeString

validOn :: forall a.
           Platform a -> I.FilePath -> Bool
validOn = I.validOn . unPlatform

splitSearchPathOn :: forall a.
                     Platform a -> a -> [FilePath]
splitSearchPathOn p = map fromIFP . I.splitSearchPathOn (unPlatform p)

splitSearchPathStringOn :: forall a.
                           Platform a -> String -> [FilePath]
splitSearchPathStringOn p = map fromIFP . I.splitSearchPathStringOn (unPlatform p)

toTextOn :: forall a.
            Platform a -> FilePath -> Either T.Text T.Text
toTextOn p = I.toTextOn (unPlatform p) . toIFP

fromTextOn :: forall a.
              Platform a -> T.Text -> FilePath
fromTextOn p = fromIFP . I.fromTextOn (unPlatform p)

encodeOn :: forall a.
            Platform a -> FilePath -> a
encodeOn p = I.encodeOn (unPlatform p) . toIFP

decodeOn :: forall a. Platform a -> a -> FilePath
decodeOn p = fromIFP . I.decodeOn (unPlatform p)

encodeStringOn :: forall a.
                  Platform a -> I.FilePath -> String
encodeStringOn = I.encodeStringOn . unPlatform

decodeStringOn :: forall a.
                  Platform a -> String -> FilePath
decodeStringOn p = fromIFP . I.decodeStringOn (unPlatform p)

empty :: FilePath
empty = FilePath BC.empty

null :: FilePath -> Bool
null = I.null . toIFP

root :: FilePath -> FilePath
root = fromIFP . I.root . toIFP

directory :: FilePath -> FilePath
directory = fromIFP . I.directory . toIFP

parent :: FilePath -> FilePath
parent = fromIFP . I.parent . toIFP

filename :: FilePath -> FilePath
filename = fromIFP . I.filename . toIFP

dirname :: FilePath -> FilePath
dirname = fromIFP . I.dirname . toIFP

basename :: FilePath -> FilePath
basename = fromIFP . I.basename . toIFP

absolute :: FilePath -> Bool
absolute = I.absolute . toIFP

relative :: FilePath -> Bool
relative = I.relative . toIFP

append :: FilePath -> FilePath -> FilePath
append x y = fromIFP (I.append (toIFP x) (toIFP y))

(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

concat :: [FilePath] -> FilePath
concat = fromIFP . I.concat . map toIFP

commonPrefix :: [FilePath] -> FilePath
commonPrefix = fromIFP . I.commonPrefix . map toIFP

stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix x y = fmap fromIFP (I.stripPrefix (toIFP x) (toIFP y))

collapse :: FilePath -> FilePath
collapse = fromIFP . I.collapse . toIFP

splitDirectories :: FilePath -> [FilePath]
splitDirectories = map fromIFP . I.splitDirectories . toIFP

extension :: FilePath -> Maybe T.Text
extension = I.extension . toIFP

extensions :: FilePath -> [T.Text]
extensions = I.extensions . toIFP

hasExtension :: FilePath -> T.Text -> Bool
hasExtension = I.hasExtension . toIFP

addExtension :: FilePath -> T.Text -> FilePath
addExtension x y = fromIFP (I.addExtension (toIFP x) y)

addExtensions :: FilePath -> [T.Text] -> FilePath
addExtensions x ys = fromIFP (I.addExtensions (toIFP x) ys)

(<.>) :: FilePath -> T.Text -> FilePath
(<.>) = addExtension

dropExtension :: FilePath -> FilePath
dropExtension = fromIFP . I.dropExtension . toIFP

dropExtensions :: FilePath -> FilePath
dropExtensions = fromIFP . I.dropExtensions . toIFP

replaceExtension :: FilePath -> T.Text -> FilePath
replaceExtension x y = fromIFP (I.replaceExtension (toIFP x) y)

replaceExtensions :: FilePath -> [T.Text] -> FilePath
replaceExtensions x ys = fromIFP (I.replaceExtensions (toIFP x) ys)

splitExtension :: FilePath -> (FilePath, Maybe T.Text)
splitExtension x =
  let split = I.splitExtension (toIFP x)
  in (fromIFP (fst split),snd split)

splitExtensions :: FilePath -> (FilePath, [T.Text])
splitExtensions x =
  let split = I.splitExtensions (toIFP x)
  in (fromIFP (fst split),snd split)

--
-- Internal
--

toIFP :: FilePath -> I.FilePath
toIFP = I.fromText . T.pack . BC.unpack . unFilePath

fromIFP :: I.FilePath -> FilePath
fromIFP = FilePath . BC.pack . T.unpack . either id id . I.toText
