{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (tests, main) where

import           Prelude hiding (FilePath)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.List (intercalate)
import qualified Data.Text as T
import           Data.Text (Text)
import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Filesystem.Path as P
import           Filesystem.Path.CurrentOS ()
import           Filesystem.Path.Rules

main :: IO ()
main = Test.Chell.defaultMain tests

tests :: [Suite]
tests =
	[
	-- Basic properties
	  test test_Empty
	, test test_Root
	, test test_Directory
	, test test_Parent
	, test test_Filename
	, test test_Dirname
	, test test_Basename
	, test test_Absolute
	, test test_Relative
	
	-- Basic operations
	, test test_Append
	, test test_CommonPrefix
	, test test_SplitExtension
	, test test_Collapse
	
	, suite "to-from-bytes"
		[ test_Identity "posix" posix posixPaths
		, test_Identity "windows" windows windowsPaths
		, test test_MixedValidityToBytes
		]
	
	, suite "to-from-text"
		[ test test_ToText
		, test test_FromText
		]
	
	, suite "validity"
		[ property "posix" (forAll posixPaths (valid posix))
		, property "windows" (forAll windowsPaths (valid windows))
		]
	
	, test test_SplitSearchPath
	, test test_Parsing
	]

test_Empty :: Test
test_Empty = assertions "empty" $ do
	$expect $ P.null empty
	$expect $ equal (toChar8 posix empty) ""
	$expect $ equal (toString windows empty) ""

test_Root :: Test
test_Root = assertions "root" $ do
	let root x = toChar8 posix (P.root (fromChar8 posix x))
	
	$expect $ equal (root "") ""
	$expect $ equal (root "/") "/"
	$expect $ equal (root "foo") ""
	$expect $ equal (root "/foo") "/"

test_Directory :: Test
test_Directory = assertions "directory" $ do
	let directory x = toChar8 posix (P.directory (fromChar8 posix x))
	
	$expect $ equal (directory "") "./"
	$expect $ equal (directory "/") "/"
	$expect $ equal (directory "/foo/bar") "/foo/"
	$expect $ equal (directory "/foo/bar/") "/foo/bar/"
	$expect $ equal (directory ".") "./"
	$expect $ equal (directory "..") "../"
	$expect $ equal (directory "../foo") "../"
	$expect $ equal (directory "../foo/") "../foo/"
	$expect $ equal (directory "foo") "./"
	$expect $ equal (directory "foo/bar") "./foo/"

test_Parent :: Test
test_Parent = assertions "parent" $ do
	let parent x = toChar8 posix (P.parent (fromChar8 posix x))
	
	$expect $ equal (parent "") "./"
	$expect $ equal (parent "/") "/"
	$expect $ equal (parent "/foo/bar") "/foo/"
	$expect $ equal (parent "/foo/bar/") "/foo/"
	$expect $ equal (parent ".") "./"
	$expect $ equal (parent "..") "./"
	$expect $ equal (parent "../foo/bar") "../foo/"
	$expect $ equal (parent "../foo/bar") "../foo/"
	$expect $ equal (parent "foo") "./"
	$expect $ equal (parent "foo/bar") "./foo/"

test_Filename :: Test
test_Filename = assertions "filename" $ do
	let filename x = toChar8 posix (P.filename (fromChar8 posix x))
	
	$expect $ equal (filename "") ""
	$expect $ equal (filename "/") ""
	$expect $ equal (filename "/foo/") ""
	$expect $ equal (filename "/foo/bar") "bar"
	$expect $ equal (filename "/foo/bar.txt") "bar.txt"

test_Dirname :: Test
test_Dirname = assertions "dirname" $ do
	let dirname x = toChar8 posix (P.dirname (fromChar8 posix x))
	
	$expect $ equal (dirname "") ""
	$expect $ equal (dirname "/") ""
	$expect $ equal (dirname "foo") ""
	$expect $ equal (dirname "foo/bar") "foo"
	$expect $ equal (dirname "foo/bar/") "bar"
	$expect $ equal (dirname "foo/bar/baz.txt") "bar"
	
	-- the directory name will be re-parsed to a file name.
	let dirnameExts x = P.extensions (P.dirname (fromChar8 posix x))
	$expect $ equal (dirnameExts "foo.d/bar") ["d"]
	
	-- reparsing preserves good/bad encoding state
	$expect $ equal (dirnameExts "foo.\xB1.\xDD\xAA/bar") ["\xB1", "\xDD\xAA"]

test_Basename :: Test
test_Basename = assertions "basename" $ do
	let basename_posix x = toChar8 posix (basename (fromChar8 posix x))
	let basename_windows x = toString windows (basename (fromString windows x))
	
	$expect $ equal (basename_posix "/foo/bar") "bar"
	$expect $ equal (basename_posix "/foo/bar.txt") "bar"
	$expect $ equal (basename_posix ".") ""
	$expect $ equal (basename_posix "..") ""
	
	$expect $ equal (basename_windows "c:\\foo\\bar") "bar"
	$expect $ equal (basename_windows "c:\\foo\\bar.txt") "bar"
	$expect $ equal (basename_windows ".") ""
	$expect $ equal (basename_windows "..") ""

test_Absolute :: Test
test_Absolute = assertions "absolute" $ do
	$expect $ absolute (fromChar8 posix "/")
	$expect $ absolute (fromChar8 posix "/foo/bar")
	$expect . not $ absolute (fromChar8 posix "")
	$expect . not $ absolute (fromChar8 posix "foo/bar")
	
	$expect $ absolute (fromString windows "c:\\")
	$expect $ absolute (fromString windows "c:\\foo\\bar")
	$expect . not $ absolute (fromString windows "")
	$expect . not $ absolute (fromString windows "foo\\bar")
	$expect . not $ absolute (fromString windows "\\foo\\bar")

test_Relative :: Test
test_Relative = assertions "relative" $ do
	$expect . not $ relative (fromChar8 posix "/")
	$expect . not $ relative (fromChar8 posix "/foo/bar")
	$expect $ relative (fromChar8 posix "")
	$expect $ relative (fromChar8 posix "foo/bar")
	
	$expect . not $ relative (fromString windows "c:\\")
	$expect . not $ relative (fromString windows "c:\\foo\\bar")
	$expect $ relative (fromString windows "")
	$expect $ relative (fromString windows "foo\\bar")
	$expect . not $ relative (fromString windows "\\foo\\bar")

test_Identity :: Text -> Rules a -> Gen FilePath -> Suite
test_Identity name r gen = property name $ forAll gen $ \p -> p == decode r (encode r p)

test_MixedValidityToBytes :: Test
test_MixedValidityToBytes = assertions "mixed-validity-to-bytes" $ do
	let p = fromChar8 posix
	
	$expect $ equal (encode posix (p "\xB1.\xDD\xAA")) (B8.pack "\xB1.\xDD\xAA")
	$expect $ equal (encode posix (p "\xB1.\xDD\xAA" </> p "foo")) (B8.pack "\xB1.\xDD\xAA/foo")

test_ToText :: Test
test_ToText = assertions "toText" $ do
	let p = fromChar8 posix
	
	$expect $ equal (toText posix (p "")) (Right (T.pack ""))
	$expect $ equal (toText posix (p "ascii")) (Right (T.pack "ascii"))
	$expect $ equal (toText posix (p "\xF0\x9D\x84\x9E")) (Right (T.pack "\x1D11E"))
	$expect $ equal (toText posix (p "\xED\xA0\x80")) (Left (T.pack "\xED\xA0\x80"))
	$expect $ equal (toText posix (p "\xF0\x9D\x84\x9E/\xED\xA0\x80")) (Left (T.pack "\x1D11E/\xED\xA0\x80"))
	$expect $ equal (toText posix (p "\xED.\xF0\x9D\x84\x9E.\xA0\x80")) (Left (T.pack "\xED.\x1D11E.\xA0\x80"))
	$expect $ equal (toText posix (p "\xB1.\xDD\xAA")) (Left (T.pack "\xB1.\x76A"))
	$expect $ equal (toText posix (p "\xB1.\xDD\xAA" </> p "foo")) (Left (T.pack "\xB1.\xDD\xAA/foo"))

test_FromText :: Test
test_FromText = assertions "fromText" $ do
	let pt x = fromText posix (T.pack x)
	let p = fromChar8 posix
	
	$expect $ equal (pt "") (p "")
	$expect $ equal (pt "\x1D11E") (p "\xF0\x9D\x84\x9E")
	$expect $ equal (pt "\xED\xA0\x80") (p "\xC3\xAD\xC2\xA0\xC2\x80")

test_Append :: Test
test_Append = assertions "append" $ do
	let appendP x y = toChar8 posix (P.append (fromChar8 posix x) (fromChar8 posix y))
	let appendW x y = toString windows (P.append (fromString windows x) (fromString windows y))
	
	$expect $ equal (appendP "" "") ""
	$expect $ equal (appendP "" "b/") "b/"
	
	-- Relative to a directory
	$expect $ equal (appendP "a/" "") "a/"
	$expect $ equal (appendP "a/" "b/") "a/b/"
	$expect $ equal (appendP "a/" "b.txt") "a/b.txt"
	$expect $ equal (appendP "a.txt" "b.txt") "a.txt/b.txt"
	$expect $ equal (appendP "." "a") "./a"
	
	-- Relative to a file
	$expect $ equal (appendP "a" "") "a/"
	$expect $ equal (appendP "a" "b/") "a/b/"
	$expect $ equal (appendP "a/b" "c") "a/b/c"
	
	-- Absolute
	$expect $ equal (appendP "/a/" "") "/a/"
	$expect $ equal (appendP "/a/" "b") "/a/b"
	$expect $ equal (appendP "/a/" "b/") "/a/b/"
	
	-- Second parameter is absolute
	$expect $ equal (appendP "/a/" "/") "/"
	$expect $ equal (appendP "/a/" "/b") "/b"
	$expect $ equal (appendP "/a/" "/b/") "/b/"
	
	-- Windows: volume handling
	$expect $ equal (appendW "c:\\" "") "C:\\"
	$expect $ equal (appendW "c:\\foo" "bar\\baz") "C:\\foo\\bar\\baz"
	$expect $ equal (appendW "c:\\foo" "d:\\bar") "D:\\bar"
	$expect $ equal (appendW "c:\\foo" "\\bar") "C:\\bar"
	$expect $ equal (appendW "foo\\bar" "\\baz") "\\baz"

test_CommonPrefix :: Test
test_CommonPrefix = assertions "commonPrefix" $ do
	let commonPrefix xs = toChar8 posix (P.commonPrefix (map (fromChar8 posix) xs))
	
	$expect $ equal (commonPrefix ["", ""]) ""
	$expect $ equal (commonPrefix ["/", ""]) ""
	$expect $ equal (commonPrefix ["/", "/"]) "/"
	$expect $ equal (commonPrefix ["foo/", "/foo/"]) ""
	$expect $ equal (commonPrefix ["/foo", "/foo/"]) "/"
	$expect $ equal (commonPrefix ["/foo/", "/foo/"]) "/foo/"
	$expect $ equal (commonPrefix ["/foo/bar/baz.txt.gz", "/foo/bar/baz.txt.gz.bar"]) "/foo/bar/baz.txt.gz"

test_SplitExtension :: Test
test_SplitExtension = assertions "splitExtension" $ do
	let splitExtension x = (toChar8 posix base, ext) where
		(base, ext) = P.splitExtension (fromChar8 posix x)
	
	$expect $ equal (splitExtension "") ("", Nothing)
	$expect $ equal (splitExtension "foo") ("foo", Nothing)
	$expect $ equal (splitExtension "foo.") ("foo", Just (T.pack ""))
	$expect $ equal (splitExtension "foo.a") ("foo", Just (T.pack "a"))
	$expect $ equal (splitExtension "foo.a/") ("foo.a/", Nothing)
	$expect $ equal (splitExtension "foo.a/bar") ("foo.a/bar", Nothing)
	$expect $ equal (splitExtension "foo.a/bar.b") ("foo.a/bar", Just (T.pack "b"))
	$expect $ equal (splitExtension "foo.a/bar.b.c") ("foo.a/bar.b", Just (T.pack "c"))

test_Collapse :: Test
test_Collapse = assertions "collapse" $ do
	let collapse x = toChar8 posix (P.collapse (fromChar8 posix x))
	
	$expect $ equal (collapse "./") "./"
	$expect $ equal (collapse "././") "./"
	$expect $ equal (collapse "../") "../"
	$expect $ equal (collapse ".././") "../"
	$expect $ equal (collapse "./../") "../"
	$expect $ equal (collapse "parent/foo/../bar") "parent/bar"
	$expect $ equal (collapse "parent/foo/..") "parent/"

test_Parsing :: Test
test_Parsing = assertions "parsing" $ do
	let p x = toChar8 posix (fromChar8 posix x)
	let w x = toString windows (fromString windows x)
	
	$expect $ equal (p "") ""
	$expect $ equal (p "/") "/"
	$expect $ equal (p "/a") "/a"
	$expect $ equal (p "/a/") "/a/"
	$expect $ equal (p "a") "a"
	$expect $ equal (p "a/") "a/"
	$expect $ equal (p "a/b") "a/b"
	$expect $ equal (p "a//b") "a/b"
	$expect $ equal (p "a/./b") "a/./b"
	$expect $ equal (p ".") "./"
	$expect $ equal (p "./") "./"
	$expect $ equal (p "..") "../"
	$expect $ equal (p "../") "../"
	
	$expect $ equal (w "") ""
	$expect $ equal (w "c:\\") "C:\\"
	$expect $ equal (w "c:\\a") "C:\\a"
	$expect $ equal (w "c:\\a\\") "C:\\a\\"
	$expect $ equal (w "a") "a"
	$expect $ equal (w "a/") "a\\"
	$expect $ equal (w "a\\") "a\\"
	$expect $ equal (w "a\\b") "a\\b"
	$expect $ equal (w "a\\\\b") "a\\b"
	$expect $ equal (w "a\\.\\b") "a\\.\\b"
	$expect $ equal (w ".") ".\\"
	$expect $ equal (w ".\\") ".\\"
	$expect $ equal (w "..") "..\\"
	$expect $ equal (w "..\\") "..\\"

test_SplitSearchPath :: Test
test_SplitSearchPath = assertions "splitSearchPath" $ do
	let p x = map (toChar8 posix) (splitSearchPath posix (B8.pack x))
	let w x = map (toString windows) (splitSearchPath windows (T.pack x))
	
	$expect $ equal (p "a:b:c") ["a", "b", "c"]
	$expect $ equal (p "a::b:c") ["a", "./", "b", "c"]
	$expect $ equal (w "a;b;c") ["a", "b", "c"]
	$expect $ equal (w "a;;b;c") ["a", "b", "c"]

posixPaths :: Gen FilePath
posixPaths = sized $ fmap merge . genComponents where
	merge = fromChar8 posix . intercalate "/"
	validChar c = not $ elem c ['\x00', '/']
	component = do
		size <- choose (0, 10)
		vectorOf size $ arbitrary `suchThat` validChar
	genComponents n = do
		cs <- vectorOf n component
		frequency [(1, return cs), (9, return ([""] ++ cs))]

windowsPaths :: Gen FilePath
windowsPaths = sized $ \n -> genComponents n >>= merge where
	merge cs = do
		root <- genRoot
		let path = intercalate "\\" cs
		return $ fromString windows $ root ++ path
		
	reserved = ['\x00'..'\x1F'] ++ ['/', '\\', '?', '*', ':', '|', '"', '<', '>']
	validChar c = not $ elem c reserved
	component = do
		size <- choose (0, 10)
		vectorOf size $ arbitrary `suchThat` validChar
	genComponents n = do
		cs <- vectorOf n component
		frequency [(1, return cs), (9, return ([""] ++ cs))]
	
	genRoot = do
		let upperChar = elements ['A'..'Z']
		label <- frequency [(1, return Nothing), (9, fmap Just upperChar)]
		return $ case label of
			Just c -> [c, ':', '\\']
			Nothing -> "\\"

toChar8 :: Rules B.ByteString -> FilePath -> String
toChar8 r = B8.unpack . encode r

fromChar8 :: Rules B.ByteString -> String -> FilePath
fromChar8 r = decode r . B8.pack

toString :: Rules T.Text -> FilePath -> String
toString r = T.unpack . encode r

fromString :: Rules T.Text -> String -> FilePath
fromString r = decode r . T.pack
