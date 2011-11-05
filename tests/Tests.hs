{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (tests, main) where

import           Prelude hiding (FilePath)

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
	, test_EncodeString
	, test_DecodeString
	, test_EqualsIgnoresPosixEncoding
	, test_ShowRules
	]

test_Empty :: Suite
test_Empty = assertions "empty" $ do
	$expect $ P.null empty
	$expect $ equal (toChar8 empty) ""
	$expect $ equal (toString empty) ""

test_Root :: Suite
test_Root = assertions "root" $ do
	let root x = toChar8 (P.root (fromChar8 x))
	
	$expect $ equal (root "") ""
	$expect $ equal (root "/") "/"
	$expect $ equal (root "foo") ""
	$expect $ equal (root "/foo") "/"

test_Directory :: Suite
test_Directory = assertions "directory" $ do
	let directory x = toChar8 (P.directory (fromChar8 x))
	
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
	let parent x = toChar8 (P.parent (fromChar8 x))
	
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
	let filename x = toChar8 (P.filename (fromChar8 x))
	
	$expect $ equal (filename "") ""
	$expect $ equal (filename "/") ""
	$expect $ equal (filename "/foo/") ""
	$expect $ equal (filename "/foo/bar") "bar"
	$expect $ equal (filename "/foo/bar.txt") "bar.txt"

test_Dirname :: Suite
test_Dirname = assertions "dirname" $ do
	let dirname x = toChar8 (P.dirname (fromChar8 x))
	
	$expect $ equal (dirname "") ""
	$expect $ equal (dirname "/") ""
	$expect $ equal (dirname "foo") ""
	$expect $ equal (dirname "foo/bar") "foo"
	$expect $ equal (dirname "foo/bar/") "bar"
	$expect $ equal (dirname "foo/bar/baz.txt") "bar"
	
	-- the directory name will be re-parsed to a file name.
	let dirnameExts x = P.extensions (P.dirname (fromChar8 x))
	$expect $ equal (dirnameExts "foo.d/bar") ["d"]
	
	-- reparsing preserves good/bad encoding state
	$expect $ equal (dirnameExts "foo.\xB1.\xDD\xAA/bar") ["\xB1", "\x76A"]

test_Basename :: Suite
test_Basename = assertions "basename" $ do
	let basename_posix x = toChar8 (basename (fromChar8 x))
	let basename_windows x = toString (basename (fromString x))
	
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
	$expect $ absolute (fromChar8 "/")
	$expect $ absolute (fromChar8 "/foo/bar")
	$expect . not $ absolute (fromChar8 "")
	$expect . not $ absolute (fromChar8 "foo/bar")
	
	$expect $ absolute (fromString "c:\\")
	$expect $ absolute (fromString "c:\\foo\\bar")
	$expect . not $ absolute (fromString "")
	$expect . not $ absolute (fromString "foo\\bar")
	$expect . not $ absolute (fromString "\\foo\\bar")

test_Relative :: Suite
test_Relative = assertions "relative" $ do
	$expect . not $ relative (fromChar8 "/")
	$expect . not $ relative (fromChar8 "/foo/bar")
	$expect $ relative (fromChar8 "")
	$expect $ relative (fromChar8 "foo/bar")
	
	$expect . not $ relative (fromString "c:\\")
	$expect . not $ relative (fromString "c:\\foo\\bar")
	$expect $ relative (fromString "")
	$expect $ relative (fromString "foo\\bar")
	$expect . not $ relative (fromString "\\foo\\bar")

test_Identity :: Text -> Rules a -> Gen FilePath -> Suite
test_Identity name r gen = property name $ forAll gen $ \p -> p == decode r (encode r p)

test_MixedValidityToBytes :: Suite
test_MixedValidityToBytes = assertions "mixed-validity-to-bytes" $ do
	let p = fromChar8
	
	$expect $ equal (encode posix (p "\xB1.\xDD\xAA")) (B8.pack "\xB1.\xDD\xAA")
	$expect $ equal (encode posix (p "\xB1.\xDD\xAA" </> p "foo")) (B8.pack "\xB1.\xDD\xAA/foo")

test_ToText :: Suite
test_ToText = assertions "toText" $ do
	let p = fromChar8
	
	$expect $ equal (toText posix (p "")) (Right (T.pack ""))
	$expect $ equal (toText posix (p "ascii")) (Right (T.pack "ascii"))
	$expect $ equal (toText posix (p "\xF0\x9D\x84\x9E")) (Right (T.pack "\x1D11E"))
	$expect $ equal (toText posix (p "\xED\xA0\x80")) (Left (T.pack "\xED\xA0\x80"))
	$expect $ equal (toText posix (p "\xF0\x9D\x84\x9E/\xED\xA0\x80")) (Left (T.pack "\x1D11E/\xED\xA0\x80"))
	$expect $ equal (toText posix (p "\xED.\xF0\x9D\x84\x9E.\xA0\x80")) (Left (T.pack "\xED.\x1D11E.\xA0\x80"))
	$expect $ equal (toText posix (p "\xB1.\xDD\xAA")) (Left (T.pack "\xB1.\x76A"))
	$expect $ equal (toText posix (p "\xB1.\xDD\xAA" </> p "foo")) (Left (T.pack "\xB1.\x76A/foo"))

test_FromText :: Suite
test_FromText = assertions "fromText" $ do
	let pt x = fromText posix (T.pack x)
	let p = fromChar8
	
	$expect $ equal (pt "") (p "")
	$expect $ equal (pt "\x1D11E") (p "\xF0\x9D\x84\x9E")
	$expect $ equal (pt "\xED\xA0\x80") (p "\xC3\xAD\xC2\xA0\xC2\x80")

test_Append :: Suite
test_Append = assertions "append" $ do
	let appendP x y = toChar8 (P.append (fromChar8 x) (fromChar8 y))
	let appendW x y = toString (P.append (fromString x) (fromString y))
	
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
	let commonPrefix xs = toChar8 (P.commonPrefix (map (fromChar8) xs))
	
	$expect $ equal (commonPrefix ["", ""]) ""
	$expect $ equal (commonPrefix ["/", ""]) ""
	$expect $ equal (commonPrefix ["/", "/"]) "/"
	$expect $ equal (commonPrefix ["foo/", "/foo/"]) ""
	$expect $ equal (commonPrefix ["/foo", "/foo/"]) "/"
	$expect $ equal (commonPrefix ["/foo/", "/foo/"]) "/foo/"
	$expect $ equal (commonPrefix ["/foo/bar/baz.txt.gz", "/foo/bar/baz.txt.gz.bar"]) "/foo/bar/baz.txt.gz"

test_StripPrefix :: Suite
test_StripPrefix = assertions "stripPrefix" $ do
	let stripPrefix x y = fmap (toChar8) (P.stripPrefix (fromChar8 x) (fromChar8 y))
	
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
	let full = fromChar8 (toChar8 prefix ++ toChar8 suffix) in
	case stripPrefix prefix full of
		Nothing -> False
		Just stripped -> prefix </> stripped == full

test_SplitExtension :: Suite
test_SplitExtension = assertions "splitExtension" $ do
	let splitExtension x = (toChar8 base, ext) where
		(base, ext) = P.splitExtension (fromChar8 x)
	
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
	let collapse x = toChar8 (P.collapse (fromChar8 x))
	
	$expect $ equal (collapse "./") "./"
	$expect $ equal (collapse "././") "./"
	$expect $ equal (collapse "../") "../"
	$expect $ equal (collapse ".././") "../"
	$expect $ equal (collapse "./../") "../"
	$expect $ equal (collapse "../../") "../../"
	$expect $ equal (collapse "parent/foo/baz/../bar") "parent/foo/bar"
	$expect $ equal (collapse "parent/foo/baz/../../bar") "parent/bar"
	$expect $ equal (collapse "parent/foo/..") "parent/"

test_Parsing :: Suite
test_Parsing = assertions "parsing" $ do
	let p x = toChar8 (fromChar8 x)
	let w x = toString (fromString x)
	
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
	let p x = map (toChar8) (splitSearchPath posix (B8.pack x))
	let w x = map (toString) (splitSearchPath windows (T.pack x))
	
	$expect $ equal (p "a:b:c") ["a", "b", "c"]
	$expect $ equal (p "a::b:c") ["a", "./", "b", "c"]
	$expect $ equal (w "a;b;c") ["a", "b", "c"]
	$expect $ equal (w "a;;b;c") ["a", "b", "c"]

test_EncodeString :: Suite
test_EncodeString = suite "encodeString"
	[ test_EncodeString_Posix
	, test_EncodeString_Posix_Ghc702
	, test_EncodeString_Win32
	]

test_EncodeString_Posix :: Suite
test_EncodeString_Posix = assertions "posix" $ do
	let enc = encodeString posix
	$expect $ equal (enc (fromChar8 "test")) "test"
	$expect $ equal (enc (fromChar8 "test\xC2\xA1\xC2\xA2")) "test\xC2\xA1\xC2\xA2"
	$expect $ equal (enc (fromChar8 "test\xA1\xA2")) "test\xA1\xA2"
	$expect $ equal (enc (fromChar8 "\xC2\xA1\xC2\xA2/test\xA1\xA2")) "\xC2\xA1\xC2\xA2/test\xA1\xA2"
	$expect $ equal (enc (fromText posix "test\xA1\xA2")) "test\xC2\xA1\xC2\xA2"

test_EncodeString_Posix_Ghc702 :: Suite
test_EncodeString_Posix_Ghc702 = assertions "posix_ghc702" $ do
	let enc = encodeString posix_ghc702
	$expect $ equal (enc (fromChar8 "test")) "test"
	$expect $ equal (enc (fromChar8 "test\xA1\xA2")) "test\xEFA1\xEFA2"
	$expect $ equal (enc (fromChar8 "\xC2\xA1\xC2\xA2/test\xA1\xA2")) "\xA1\xA2/test\xEFA1\xEFA2"
	$expect $ equal (enc (fromText posix_ghc702 "test\xA1\xA2")) "test\xA1\xA2"

test_EncodeString_Win32 :: Suite
test_EncodeString_Win32 = assertions "windows" $ do
	let enc = encodeString windows
	$expect $ equal (enc (fromString "test")) "test"
	$expect $ equal (enc (fromString "test\xA1\xA2")) "test\xA1\xA2"
	$expect $ equal (enc (fromText windows "test\xA1\xA2")) "test\xA1\xA2"

test_DecodeString :: Suite
test_DecodeString = suite "decodeString"
	[ test_DecodeString_Posix
	, test_DecodeString_Posix_Ghc702
	, test_DecodeString_Darwin
	, test_DecodeString_Darwin_Ghc702
	, test_DecodeString_Win32
	]

test_DecodeString_Posix :: Suite
test_DecodeString_Posix = assertions "posix" $ do
	let r = posix
	let dec = decodeString
	$expect $ equal (dec r "test") (fromText r "test")
	$expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xA1\xA2")
	$expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")

test_DecodeString_Posix_Ghc702 :: Suite
test_DecodeString_Posix_Ghc702 = assertions "posix_ghc702" $ do
	let r = posix_ghc702
	let dec = decodeString
	$expect $ equal (dec r "test") (fromText r "test")
	$expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xC2\xA1\xC2\xA2")
	$expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")
	$expect $ equal (dec r "test\xEFA1\xEFA2") (fromChar8 "test\xA1\xA2")
	$expect $ equal
		(toText r (dec r "test\xEFA1\xEFA2"))
		(Left "test\xA1\xA2")

test_DecodeString_Darwin :: Suite
test_DecodeString_Darwin = assertions "darwin" $ do
	let r = darwin
	let dec = decodeString
	$expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xA1\xA2")

test_DecodeString_Darwin_Ghc702 :: Suite
test_DecodeString_Darwin_Ghc702 = assertions "darwin_ghc702" $ do
	let r = darwin_ghc702
	let dec = decodeString
	$expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xC2\xA1\xC2\xA2")
	$expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")
	$expect $ equal (dec r "test\xEFA1\xEFA2") (fromChar8 "test\xA1\xA2")

test_DecodeString_Win32 :: Suite
test_DecodeString_Win32 = assertions "windows" $ do
	let r = windows
	let dec = decodeString
	$expect $ equal (dec r "test") (fromText r "test")
	$expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xC2\xA1\xC2\xA2")
	$expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")

test_EqualsIgnoresPosixEncoding :: Suite
test_EqualsIgnoresPosixEncoding = assertions "equals-ignores-posix-encoding" $ do
	$expect $ equal
		(fromChar8 "test\xA1\xA2")
		(fromText posix "test\xA1\xA2")

test_ShowRules :: Suite
test_ShowRules = assertions "show-rules" $ do
	$expect $ equal (showsPrec 11 darwin "") "(Rules \"Darwin\")"
	$expect $ equal (showsPrec 11 darwin_ghc702 "") "(Rules \"Darwin (GHC 7.2)\")"
	$expect $ equal (showsPrec 11 posix "") "(Rules \"POSIX\")"
	$expect $ equal (showsPrec 11 posix_ghc702 "") "(Rules \"POSIX (GHC 7.2)\")"
	$expect $ equal (showsPrec 11 windows "") "(Rules \"Windows\")"

posixPaths :: Gen FilePath
posixPaths = sized $ fmap merge . genComponents where
	merge = fromChar8 . intercalate "/"
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
		return $ fromString $ root ++ path
		
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

toChar8 :: FilePath -> String
toChar8 = B8.unpack . encode posix

fromChar8 :: String -> FilePath
fromChar8 = decode posix . B8.pack

toString :: FilePath -> String
toString = T.unpack . encode windows

fromString :: String -> FilePath
fromString = decode windows . T.pack
