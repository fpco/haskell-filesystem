{-# LANGUAGE CPP #-}

-- |
-- Module: System.FilePath.CurrentOS
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-- Re-exports contents of "System.FilePath.Rules", defaulting to the
-- current OS's rules when needed.
--
-- Also enables 'Show' and 'S.IsString' instances for 'F.FilePath'.
--
module System.FilePath.CurrentOS
	( module System.FilePath
	, currentOS
	
	-- * Type conversions
	, toBytes
	, fromBytes
	, toText
	, fromText
	
	-- * Rule-specific path properties
	, valid
	, splitSearchPath
	) where

import qualified Data.ByteString as B
import qualified Data.String as S
import qualified Data.Text as T

import           System.FilePath
import qualified System.FilePath as F
import qualified System.FilePath.Rules as R

currentOS :: R.Rules
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
currentOS = R.windows
#else
currentOS = R.posix
#endif

instance S.IsString F.FilePath where
	fromString = R.fromText currentOS . T.pack

instance Show F.FilePath where
	showsPrec d path = showParen (d > 10) $
		showString "FilePath " . shows (toBytes path)

-- | Convert a 'FilePath' into a strict 'B.ByteString', suitable for passing
-- to OS libraries.
toBytes :: F.FilePath -> B.ByteString
toBytes = R.toBytes currentOS

-- | Parse a strict 'B.ByteString', such as  those received from OS libraries,
-- into a 'FilePath'.
fromBytes :: B.ByteString -> F.FilePath
fromBytes = R.fromBytes currentOS

-- | Attempt to convert a 'FilePath' to human-readable text.
--
-- If the path is decoded successfully, the result is a 'Right' containing
-- the decoded text. Successfully decoded text can be converted back to a
-- path using 'fromText'.
--
-- If the path cannot be decoded, the result is a 'Left' containing an
-- approximation of the original path. If displayed to the user, this value
-- should be accompanied by some warning that the path has an invalid
-- encoding. Approximated text cannot be converted back to the original path.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to display file paths with an unusual or
-- obscure encoding, use 'toBytes' and then decode them manually.
--
-- Since: 0.2
toText :: F.FilePath -> Either T.Text T.Text
toText = R.toText currentOS

-- | Convert human-readable text into a 'FilePath'.
--
-- This function ignores the user&#x2019;s locale, and assumes all file paths
-- are encoded in UTF8. If you need to create file paths with an unusual or
-- obscure encoding, encode them manually and then use 'fromBytes'.
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
splitSearchPath :: B.ByteString -> [F.FilePath]
splitSearchPath = R.splitSearchPath currentOS
