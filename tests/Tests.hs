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
	  test_Empty
	, test_Root
	, test_Directory
	, test_Parent
	, test_Filename
	, test_Dirname
	, test_Basename
	, test_Absolute
	, test_Relative
	
	-- Basic operations
	, test_Append
	, test_CommonPrefix
	, test_StripPrefix
	, property "stripPrefix" prop_StripPrefix
	, test_SplitExtension
	, test_Collapse
	
	, suite "to-from-bytes"
		[ test_Identity "posix" posix posixPaths
		, test_Identity "windows" windows windowsPaths
		, test_MixedValidityToBytes
		]
	
	, suite "to-from-text"
		[ test_ToText
		, test_FromText
		]
	
	, suite "validity"
		[ property "posix" (forAll posixPaths (valid posix))
		, property "windows" (forAll windowsPaths (valid windows))
		]
	
	, test_SplitSearchPath
	, test_Parsing
	]

test_Empty :: Suite
test_Empty = assertions "empty" $ do
	$expect $ P.null empty
	$expect $ equal (toChar8 posix empty) ""
	$expect $ equal (toString windows empty) ""

test_Root :: Suite
test_Root = assertions "root" $ do
	let root x = toChar8 posix (P.root (fromChar8 posix x))
	
	$expect $ equal (root "") ""
	$expect $ equal (root "/") "/"
	$expect $ equal (root "foo") ""
	$expect $ equal (root "/foo") "/"

test_Directory :: Suite
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

test_Parent :: Suite
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

test_Filename :: Suite
test_Filename = assertions "filename" $ do
	let filename x = toChar8 posix (P.filename (fromChar8 posix x))
	
	$expect $ equal (filename "") ""
	$expect $ equal (filename "/") ""
	$expect $ equal (filename "/foo/") ""
	$expect $ equal (filename "/foo/bar") "bar"
	$expect $ equal (filename "/foo/bar.txt") "bar.txt"

test_Dirname :: Suite
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

test_Basename :: Suite
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

test_Absolute :: Suite
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

test_Relative :: Suite
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

test_MixedValidityToBytes :: Suite
test_MixedValidityToBytes = assertions "mixed-validity-to-bytes" $ do
	let p = fromChar8 posix
	
	$expect $ equal (encode posix (p "\xB1.\xDD\xAA")) (B8.pack "\xB1.\xDD\xAA")
	$expect $ equal (encode posix (p "\xB1.\xDD\xAA" </> p "foo")) (B8.pack "\xB1.\xDD\xAA/foo")

test_ToText :: Suite
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

test_FromText :: Suite
test_FromText = assertions "fromText" $ do
	let pt x = fromText posix (T.pack x)
	let p = fromChar8 posix
	
	$expect $ equal (pt "") (p "")
	$expect $ equal (pt "\x1D11E") (p "\xF0\x9D\x84\x9E")
	$expect $ equal (pt "\xED\xA0\x80") (p "\xC3\xAD\xC2\xA0\xC2\x80")

test_Append :: Suite
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

test_CommonPrefix :: Suite
test_CommonPrefix = assertions "commonPrefix" $ do
	let commonPrefix xs = toChar8 posix (P.commonPrefix (map (fromChar8 posix) xs))
	
	$expect $ equal (commonPrefix ["", ""]) ""
	$expect $ equal (commonPrefix ["/", ""]) ""
	$expect $ equal (commonPrefix ["/", "/"]) "/"
	$expect $ equal (commonPrefix ["foo/", "/foo/"]) ""
	$expect $ equal (commonPrefix ["/foo", "/foo/"]) "/"
	$expect $ equal (commonPrefix ["/foo/", "/foo/"]) "/foo/"
	$expect $ equal (commonPrefix ["/foo/bar/baz.txt.gz", "/foo/bar/baz.txt.gz.bar"]) "/foo/bar/baz.txt.gz"

test_StripPrefix :: Suite
test_StripPrefix = assertions "stripPrefix" $ do
	let stripPrefix x y = fmap (toChar8 posix) (P.stripPrefix (fromChar8 posix x) (fromChar8 posix y))
	
	$expect $ equal (stripPrefix "" "") (Just "")
	$expect $ equal (stripPrefix "" "/") (Just "/")
	$expect $ equal (stripPrefix "/" "/") (Just "")
	$expect $ equal (stripPrefix "/" "/foo") (Just "foo")
	$expect $ equal (stripPrefix "/" "/foo/bar") (Just "foo/bar")
	$expect $ equal (stripPrefix "/foo/" "/foo/bar") (Just "bar")
	$expect $ equal (stripPrefix "/foo/" "/foo/bar/baz") (Just "bar/baz")
	$expect $ equal (stripPrefix "/foo/bar" "/foo/bar.txt") (Just ".txt")
	$expect $ equal (stripPrefix "/foo/bar.txt" "/foo/bar.txt.gz") (Just ".gz")
	
	-- Test ignoring non-matching prefixes
	$expect $ equal (stripPrefix "/foo" "/foo/bar") Nothing
	$expect $ equal (stripPrefix "/foo/bar/baz" "/foo") Nothing
	$expect $ equal (stripPrefix "/foo/baz/" "/foo/bar/qux") Nothing
	$expect $ equal (stripPrefix "/foo/bar/baz" "/foo/bar/qux") Nothing
	$expect $ equal (stripPrefix "/foo/bar/baz" "/foo/bar/qux") Nothing

prop_StripPrefix :: Property
prop_StripPrefix =
	forAll posixPaths $ \x ->
	forAll posixPaths $ \suffix ->
	let prefix = x </> "" in
	let full = fromChar8 posix (toChar8 posix prefix ++ toChar8 posix suffix) in
	case stripPrefix prefix full of
		Nothing -> False
		Just stripped -> prefix </> stripped == full

test_SplitExtension :: Suite
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

test_Collapse :: Suite
test_Collapse = assertions "collapse" $ do
	let collapse x = toChar8 posix (P.collapse (fromChar8 posix x))
	
	$expect $ equal (collapse "./") "./"
	$expect $ equal (collapse "././") "./"
	$expect $ equal (collapse "../") "../"
	$expect $ equal (collapse ".././") "../"
	$expect $ equal (collapse "./../") "../"
	$expect $ equal (collapse "parent/foo/../bar") "parent/bar"
	$expect $ equal (collapse "parent/foo/..") "parent/"

test_Parsing :: Suite
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

test_SplitSearchPath :: Suite
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
	reservedNames =
		[ "AUX", "CLOCK$", "COM1", "COM2", "COM3", "COM4"
		, "COM5", "COM6", "COM7", "COM8", "COM9", "CON"
		, "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6"
		, "LPT7", "LPT8", "LPT9", "NUL", "PRN"
		]
	validChar c = not (elem c reserved)
	validComponent c = not (elem c reservedNames)
	component = do
		size <- choose (0, 10)
		vectorOf size $ arbitrary `suchThat` validChar
	genComponents n = do
		cs <- vectorOf n (component `suchThat` validComponent)
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
