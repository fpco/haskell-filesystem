-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main where

import           Prelude hiding (FilePath)

import           Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import           System.Environment
import           System.Exit
import           System.IO (hPutStrLn, stderr)

import           Criterion.Types
import qualified Criterion.Config as C
import qualified Criterion.Main as C
import qualified Progression.Config as P
import qualified Progression.Main as P

import           Filesystem.Path (FilePath)
import           Filesystem.Path.Rules

instance NFData B.ByteString

ascii :: String
ascii = "/abcdefg/abcdefg/abcdefg.abc.def.ghi"

utf8 :: String
utf8 = "/\xC3\x88\xC3\x89\xC3\x8a\xC3\x8b\xC3\x8C/\xC3\x88\xC3\x89\xC3\x8A\xC3\x8B\xC3\x8C/\xC3\x88\xC3\x89\xC3\x8A\xC3\x8B\xC3\x8C.\xC3\x88\xC3\x89.\xC3\x8A\xC3\x8B.\x8C"

iso8859 :: String
iso8859 = "/\xC8\xC9\xCA\xCB\xCC/\xC8\xC9\xCA\xCB\xCC/\xC8\xC9\xCA\xCB\xCC.\xC8\xC9.\xCA\xCB.\xCC"

text_ascii :: T.Text
text_ascii = T.pack ascii
{-# NOINLINE text_ascii #-}

text_utf8 :: T.Text
text_utf8 = T.pack utf8
{-# NOINLINE text_utf8 #-}

text_iso8859 :: T.Text
text_iso8859 = T.pack iso8859
{-# NOINLINE text_iso8859 #-}

bytes_ascii :: B.ByteString
bytes_ascii = B8.pack ascii
{-# NOINLINE bytes_ascii #-}

bytes_utf8 :: B.ByteString
bytes_utf8 = B8.pack utf8
{-# NOINLINE bytes_utf8 #-}

bytes_iso8859 :: B.ByteString
bytes_iso8859 = B8.pack iso8859
{-# NOINLINE bytes_iso8859 #-}

posix_ascii :: FilePath
posix_ascii = decode posix bytes_ascii
{-# NOINLINE posix_ascii #-}

posix_utf8 :: FilePath
posix_utf8 = decode posix bytes_utf8
{-# NOINLINE posix_utf8 #-}

posix_iso8859 :: FilePath
posix_iso8859 = decode posix bytes_iso8859
{-# NOINLINE posix_iso8859 #-}

benchmarks :: [Benchmark]
benchmarks =
	[ bgroup "posix"
	  [ bgroup "to-text"
	    [ bench "ascii" (nf (toText posix) posix_ascii)
	    , bench "utf8" (nf (toText posix) posix_utf8)
	    , bench "iso8859" (nf (toText posix) posix_iso8859)
	    ]
	  , bgroup "to-bytes"
	    [ bench "ascii" (nf (encode posix) posix_ascii)
	    , bench "utf8" (nf (encode posix) posix_utf8)
	    , bench "iso8859" (nf (encode posix) posix_iso8859)
	    ]
	  , bgroup "to-ghc"
	    [ bench "ascii" (nf (encodeString posix_ghc702) posix_ascii)
	    , bench "utf8" (nf (encodeString posix_ghc702) posix_utf8)
	    , bench "iso8859" (nf (encodeString posix_ghc702) posix_iso8859)
	    ]
	  , bgroup "from-text"
	    [ bench "ascii" (nf (fromText posix) text_ascii)
	    , bench "utf8" (nf (fromText posix) text_utf8)
	    , bench "iso8859" (nf (fromText posix) text_iso8859)
	    ]
	  , bgroup "from-bytes"
	    [ bench "ascii" (nf (decode posix) bytes_ascii)
	    , bench "utf8" (nf (decode posix) bytes_utf8)
	    , bench "iso8859" (nf (decode posix) bytes_iso8859)
	    ]
	  ]
	]

main :: IO ()
main = do
	args <- getArgs
	case args of
		"progression":extra -> withArgs extra $ P.defaultMain (bgroup "all" benchmarks)
		"criterion":extra -> withArgs extra $ let
			config = C.defaultConfig { C.cfgPerformGC = C.ljust True }
			in C.defaultMainWith config (return ()) benchmarks
		_ -> do
			name <- getProgName
			hPutStrLn stderr $ concat ["Usage: ", name, " <progression|criterion>"]
			exitFailure
