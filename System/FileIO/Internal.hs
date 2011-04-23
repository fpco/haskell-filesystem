{-# LANGUAGE CPP #-}

-- haddock header here
module System.FileIO.Internal
	( encode
	, decode
	) where

import           Prelude hiding (FilePath)

import           System.FilePath (FilePath)
import qualified System.FilePath.Rules as R

#ifdef CABAL_OS_WINDOWS
import qualified Data.Text as T
#else
import qualified Data.ByteString.Char8 as B8
#endif

decode :: String -> FilePath
#ifdef CABAL_OS_WINDOWS
decode = R.decode R.windows . T.pack
#else
decode = R.decode R.posix . B8.pack
#endif

encode :: FilePath -> String
#ifdef CABAL_OS_WINDOWS
encode = T.unpack . R.encode R.windows
#else
encode = B8.unpack . R.encode R.posix
#endif
