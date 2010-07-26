-----------------------------------------------------------------------------
-- |
-- Module: System.FilePath.CurrentOS
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer:  jmillikin@gmail.com
-- Portability:  portable
--
-- Re-exports contents of "System.FilePath", defaulting to the current OS's
-- rules when needed.
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
module System.FilePath.CurrentOS
	( module System.FilePath
	, currentOS
	
	-- * Rule-specific path properties
	, valid
	, normalise
	, equivalent
	
	-- * Parsing file paths
	, toBytes
	, toLazyBytes
	, toString
	, fromBytes
	, fromLazyBytes
	, fromString
	
	-- * Search paths
	, splitSearchPath
	) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.String as S
import System.FilePath
import qualified System.FilePath as F
import qualified System.FilePath.Rules as R

currentOS :: R.Rules
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
currentOS = R.windows
#else
currentOS = R.posix
#endif

instance S.IsString F.FilePath where
	fromString = R.fromString currentOS

instance Show F.FilePath where
	showsPrec d path = showParen (d > 10) $
		showString "FilePath " . shows (toBytes path)

valid :: F.FilePath -> Bool
valid = R.valid currentOS

normalise :: F.FilePath -> F.FilePath
normalise = R.normalise currentOS

equivalent :: F.FilePath -> F.FilePath -> Bool
equivalent = R.equivalent currentOS

toBytes :: F.FilePath -> B.ByteString
toBytes = R.toBytes currentOS

toLazyBytes :: F.FilePath -> BL.ByteString
toLazyBytes = R.toLazyBytes currentOS

toString :: F.FilePath -> String
toString = R.toString currentOS

fromBytes :: B.ByteString -> F.FilePath
fromBytes = R.fromBytes currentOS

fromLazyBytes :: BL.ByteString -> F.FilePath
fromLazyBytes = R.fromLazyBytes currentOS

fromString :: String -> F.FilePath
fromString = R.fromString currentOS

splitSearchPath :: B.ByteString -> [F.FilePath]
splitSearchPath = R.splitSearchPath currentOS
