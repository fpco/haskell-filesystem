{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Filesystem.Path.CurrentOS
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-- Re&#x2010;exports contents of "Filesystem.Path.Rules", defaulting to the
-- current OS&#x2019;s rules when needed.
--
-- Also enables 'Show' and 'S.IsString' instances for 'F.FilePath'.
--
module Filesystem.Path.CurrentOS
  ( module Filesystem.Path
  , currentOS

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

import           Prelude hiding (FilePath)

import qualified Data.ByteString as B
import qualified Data.String as S
import qualified Data.Text as T

import           Filesystem.Path
import qualified Filesystem.Path as F
import qualified Filesystem.Path.Rules as R

#if defined(__HADDOCK_VERSION__)
#  define PLATFORM_PATH_FORMAT platformTextFormat
#elif defined(CABAL_OS_WINDOWS) || defined(CABAL_OS_DARWIN)
#  define PLATFORM_PATH_FORMAT T.Text
#else
#  define PLATFORM_PATH_FORMAT B.ByteString
#endif

currentOS :: R.Rules PLATFORM_PATH_FORMAT
#if defined(__HADDOCK_VERSION__)
currentOS = undefined
#elif defined(CABAL_OS_WINDOWS)
currentOS = R.windows
#elif defined(CABAL_OS_DARWIN)
#if __GLASGOW_HASKELL__ >= 702
currentOS = R.darwin_ghc702
#else
currentOS = R.darwin
#endif
#else
#if __GLASGOW_HASKELL__ >= 704
currentOS = R.posix_ghc704
#elif __GLASGOW_HASKELL__ >= 702
currentOS = R.posix_ghc702
#else
currentOS = R.posix
#endif
#endif

instance S.IsString F.FilePath where
  fromString = R.fromText currentOS . T.pack

instance Show F.FilePath where
  showsPrec d path = showParen (d > 10) (ss "FilePath " . s txt) where
    s = shows
    ss = showString
    txt = either id id (toText path)

-- | Attempt to convert a 'F.FilePath' to human&#x2010;readable text.
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
toText :: F.FilePath -> Either T.Text T.Text
toText = R.toText currentOS

-- | Convert human&#x2010;readable text into a 'FilePath'.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to create file paths with an unusual or
-- obscure encoding, encode them manually and then use 'decode'.
--
-- Since: 0.2
fromText :: T.Text -> F.FilePath
fromText = R.fromText currentOS

-- | Check if a 'FilePath' is valid; it must not contain any illegal
-- characters, and must have a root appropriate to the current 'R.Rules'.
valid :: F.FilePath -> Bool
valid = R.valid currentOS

-- | Split a search path, such as @$PATH@ or @$PYTHONPATH@, into a list
-- of 'FilePath's.
splitSearchPath :: PLATFORM_PATH_FORMAT -> [F.FilePath]
splitSearchPath = R.splitSearchPath currentOS

-- | splitSearchPathString is like 'splitSearchPath', but takes a string
-- encoded in the format used by @System.IO@.
splitSearchPathString :: String -> [F.FilePath]
splitSearchPathString = R.splitSearchPathString currentOS

-- | Convert a 'F.FilePath' to a platform&#x2010;specific format, suitable
-- for use with external OS functions.
--
-- Note: The type @platformTextFormat@ can change depending upon the underlying
-- compilation platform. Consider using 'toText' or 'encodeString' instead.
-- See 'Filesystem.Path.Rules.Rules' for more information.
--
-- Since: 0.3
encode :: F.FilePath -> PLATFORM_PATH_FORMAT
encode = R.encode currentOS

-- | Convert a 'F.FilePath' from a platform&#x2010;specific format, suitable
-- for use with external OS functions.
--
-- Note: The type @platformTextFormat@ can change depending upon the underlying
-- compilation platform. Consider using 'fromText' or 'decodeString' instead.
-- See 'Filesystem.Path.Rules.Rules' for more information.
--
-- Since: 0.3
decode :: PLATFORM_PATH_FORMAT -> F.FilePath
decode = R.decode currentOS

-- | Attempt to convert a 'F.FilePath' to a string suitable for use with
-- functions in @System.IO@. The contents of this string are
-- platform&#x2010;dependent, and are not guaranteed to be
-- human&#x2010;readable. For converting 'F.FilePath's to a
-- human&#x2010;readable format, use 'toText'.
--
-- Since: 0.3.1
encodeString :: F.FilePath -> String
encodeString = R.encodeString currentOS

-- | Attempt to parse a 'F.FilePath' from a string suitable for use with
-- functions in @System.IO@. Do not use this function for parsing
-- human&#x2010;readable paths, as the character set decoding is
-- platform&#x2010;dependent. For converting human&#x2010;readable text to a
-- 'F.FilePath', use 'fromText'.
--
-- Since: 0.3.1
decodeString :: String -> F.FilePath
decodeString = R.decodeString currentOS
