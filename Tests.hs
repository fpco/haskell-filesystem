module Main (tests, main) where

import Prelude hiding (FilePath)
import Data.Word (Word8)
import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Test.QuickCheck
import Test.HUnit (assert, (@?=))
import qualified Test.Framework as F
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import System.FilePath as P
import System.FilePath.CurrentOS ()
import System.FilePath.Rules

main :: IO ()
main = F.defaultMain tests

tests :: [F.Test]
tests =
	[ F.testGroup "Basic properties"
	  [ testNull
	  , testRoot
	  , testDirectory
	  , testParent
	  , testFilename
	  , testBasename
	  , testAbsolute
	  , testRelative
	  ]
	
	, F.testGroup "Basic operations"
	  [ testAppend
	  , testCommonPrefix
	  , testSplitExtension
	  ]
	
	, F.testGroup "To/From bytes"
	  [ testIdentity "POSIX" posix posixPaths
	  , testIdentity "Windows" windows windowsPaths
	  ]
	
	, F.testGroup "Validity"
	  [ testProperty "POSIX" $ forAll posixPaths $ valid posix
	  , testProperty "Windows" $ forAll windowsPaths $ valid windows
	  ]
	
	, testSplitSearchPath
	]

casePosix :: ((String -> FilePath) -> a) -> a
casePosix k = k (fromString posix)

caseWindows :: ((String -> FilePath) -> a) -> a
caseWindows k = k (fromString windows)

testCases :: F.TestName -> [Bool] -> F.Test
testCases name = F.testGroup name . zipWith (\n -> testCase n . assert) labels where
	labels = map show $ iterate (+ 1) 1

testNull :: F.Test
testNull = testCases "null"
	[ P.null empty
	]

testRoot :: F.Test
testRoot =
	let t x y = casePosix $ \p -> root (p x) == p y in
	
	testCases "root"
	[ t "" ""
	, t "/" "/"
	, t "foo" ""
	, t "/foo" "/"
	]

testDirectory :: F.Test
testDirectory =
	let t x y = casePosix $ \p -> directory (p x) == p y in
	
	testCases "directory"
	[ t "" "./"
	, t "/" "/"
	, t "/foo/bar" "/foo/"
	, t "/foo/bar/" "/foo/bar/"
	, t "." "./"
	, t ".." "./"
	, t "../foo" "../"
	, t "../foo/" "../foo/"
	, t "foo" "./"
	, t "foo/bar" "./foo/"
	]

testParent :: F.Test
testParent =
	let t x y = casePosix $ \p -> parent (p x) == p y in
	
	testCases "parent"
	[ t "" "./"
	, t "/" "/"
	, t "/foo/bar" "/foo/"
	, t "/foo/bar/" "/foo/"
	, t "." "./"
	, t ".." "./"
	, t "../foo/bar" "../foo/"
	, t "../foo/bar" "../foo/"
	, t "foo" "./"
	, t "foo/bar" "./foo/"
	]

testFilename :: F.Test
testFilename =
	let t x y = casePosix $ \p -> filename (p x) == p y in
	
	testCases "filename"
	[ t "" ""
	, t "/" ""
	, t "/foo/" ""
	, t "/foo/bar" "bar"
	, t "/foo/bar.txt" "bar.txt"
	]

testBasename :: F.Test
testBasename =
	let tp x y = casePosix $ \p -> basename (p x) == p y in
	let tw x y = caseWindows $ \p -> basename (p x) == p y in
	
	testCases "basename"
	[ tp "/foo/bar" "bar"
	, tp "/foo/bar.txt" "bar"
	, tp "." "."
	, tp ".." ".."
	
	, tw "." "."
	, tw ".." ".."
	]

testAbsolute :: F.Test
testAbsolute = testCases "absolute"
	[ absolute (fromString posix "/")
	, absolute (fromString posix "/foo/bar")
	, not $ absolute (fromString posix "")
	, not $ absolute (fromString posix "foo/bar")
	]

testRelative :: F.Test
testRelative = testCases "relative"
	[ not $ relative (fromString posix "/")
	, not $ relative (fromString posix "/foo/bar")
	, relative (fromString posix "")
	, relative (fromString posix "foo/bar")
	]

testIdentity :: F.TestName -> Rules -> Gen FilePath -> F.Test
testIdentity name r gen = testProperty name $ forAll gen $ \p -> p == fromBytes r (toBytes r p)

testAppend :: F.Test
testAppend =
	let t x y z = casePosix $ \p -> append (p x) (p y) == p z in
	
	testCases "append"
	[ t "" "" ""
	, t "" "b/" "b/"
	
	-- Relative to a directory
	, t "a/" "" "a/"
	, t "a/" "b/" "a/b/"
	, t "a/" "b.txt" "a/b.txt"
	, t "a.txt" "b.txt" "a.txt/b.txt"
	, t "." "a" "./a"
	
	-- Relative to a file
	, t "a" "" "a/"
	, t "a" "b/" "a/b/"
	, t "a/b" "c" "a/b/c"
	
	-- Absolute
	, t "/a/" "" "/a/"
	, t "/a/" "b" "/a/b"
	, t "/a/" "b/" "/a/b/"
	
	-- Second parameter is absolute
	, t "/a/" "/" "/"
	, t "/a/" "/b" "/b"
	, t "/a/" "/b/" "/b/"
	]

testCommonPrefix :: F.Test
testCommonPrefix =
	let t xs y = casePosix $ \p -> commonPrefix (map p xs) == p y in
	
	testCases "commonPrefix"
	[ t ["", ""] ""
	, t ["/", ""] ""
	, t ["/", "/"] "/"
	, t ["foo/", "/foo/"] ""
	, t ["/foo", "/foo/"] "/"
	, t ["/foo/", "/foo/"] "/foo/"
	, t ["/foo/bar/baz.txt.gz", "/foo/bar/baz.txt.gz.bar"] "/foo/bar/baz.txt.gz"
	]

testSplitExtension :: F.Test
testSplitExtension =
	let t x (y1, y2) = casePosix $ \p -> splitExtension (p x) == (p y1, fmap B8.pack y2) in
	
	testCases "splitExtension"
	[ t ""              ("", Nothing)
	, t "foo"           ("foo", Nothing)
	, t "foo."          ("foo", Just "")
	, t "foo.a"         ("foo", Just "a")
	, t "foo.a/"        ("foo.a/", Nothing)
	, t "foo.a/bar"     ("foo.a/bar", Nothing)
	, t "foo.a/bar.b"   ("foo.a/bar", Just "b")
	, t "foo.a/bar.b.c" ("foo.a/bar.b", Just "c")
	]

testSplitSearchPath :: F.Test
testSplitSearchPath =
	let tp x y = casePosix $ \p -> splitSearchPath posix (B8.pack x) == map p y in
	let tw x y = caseWindows $ \p -> splitSearchPath windows (B8.pack x) == map p y in
	
	testCases "splitSearchPath"
	[ tp "a:b:c" ["a", "b", "c"]
	, tp "a::b:c" ["a", ".", "b", "c"]
	, tw "a;b;c" ["a", "b", "c"]
	, tw "a;;b;c" ["a", "b", "c"]
	]

instance Arbitrary Rules where
	arbitrary = elements [posix, windows]

posixPaths :: Gen FilePath
posixPaths = sized $ fmap merge . genComponents where
	merge = fromString posix . intercalate "/"
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

fromString :: Rules -> String -> FilePath
fromString r = fromBytes r . B8.pack
