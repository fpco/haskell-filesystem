{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (tests, main) where

import           Prelude hiding (FilePath)

import qualified Data.ByteString.Char8 as B8
import           Data.Char (toUpper)
import           Data.List (intercalate)
import qualified Data.Text as T
import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import qualified Filesystem.Path as P
import           Filesystem.Path (FilePath, (</>), relative, stripPrefix, absolute, basename, empty)
import           Filesystem.Path.CurrentOS ()
import           Filesystem.Path.Rules

main :: IO ()
main = Test.Chell.defaultMain [tests]

tests :: Suite
tests = suite "tests" $
  -- Basic properties
  [ test_Empty
  , test_Root
  , test_Directory
  , test_Parent
  , test_Filename
  , test_Dirname
  , test_Basename
  , test_Absolute
  , test_Relative

  , test_LeadingDotSpecialCases

  -- Basic operations
  , test_Append
  , test_CommonPrefix
  , test_StripPrefix
  , property "stripPrefix" prop_StripPrefix
  , test_SplitExtension
  , test_Collapse
  , test_SplitDirectories
  , test_InvalidUtf8InDirectoryComponent
  , test_Utf8CharInGhcEscapeArea

  , test_SplitSearchPath
  , test_Parsing
  , test_EqualsIgnoresPosixEncoding
  , test_ShowRules
  ] ++ suiteTests suite_EncodeString
  ++ suiteTests suite_DecodeString
  ++ suiteTests suite_SplitSearchPathString
  ++ suiteTests (suite "to-from-bytes"
    [ test_Identity "posix" posix posixPaths
    , test_Identity "windows" windows windowsPaths
    , test_MixedValidityToBytes
    ])
  ++ suiteTests (suite "to-from-text"
    [ test_ToText
    , test_FromText
    ])
  ++ suiteTests (suite "validity" $
      -- -- Andreas Abel, 2024-11-13, the following test is broken
      -- -- because the generator produces paths with \NUL characters inside.
      -- property "posix" (forAll posixPaths (valid posix)) :
      property "windows" (forAll windowsPaths (valid windows)) :
      test_UncValidity :
      [])

test_Empty :: Test
test_Empty = assertions "empty" $ do
  $expect $ P.null empty
  $expect $ equal (toChar8 empty) ""
  $expect $ equal (toString empty) ""

test_Root :: Test
test_Root = assertions "root" $ do
  let root x = toChar8 (P.root (fromChar8 x))

  $expect $ equal (root "") ""
  $expect $ equal (root "/") "/"
  $expect $ equal (root "foo") ""
  $expect $ equal (root "/foo") "/"

test_Directory :: Test
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
  $expect $ equal (directory "foo/bar") "foo/"

test_Parent :: Test
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

test_Filename :: Test
test_Filename = assertions "filename" $ do
  let filename x = toChar8 (P.filename (fromChar8 x))

  $expect $ equal (filename "") ""
  $expect $ equal (filename "/") ""
  $expect $ equal (filename "/foo/") ""
  $expect $ equal (filename "/foo/bar") "bar"
  $expect $ equal (filename "/foo/bar.txt") "bar.txt"

test_Dirname :: Test
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

test_Basename :: Test
test_Basename = assertions "basename" $ do
  let basename_posix x = toChar8 (basename (fromChar8 x))
  let basename_windows x = toString (basename (fromString x))

  $expect $ equal (basename_posix "/foo/bar") "bar"
  $expect $ equal (basename_posix "/foo/bar.txt") "bar"

  $expect $ equal (basename_windows "c:\\foo\\bar") "bar"
  $expect $ equal (basename_windows "c:\\foo\\bar.txt") "bar"

test_Absolute :: Test
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

test_Relative :: Test
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

test_LeadingDotSpecialCases :: Test
test_LeadingDotSpecialCases = assertions "leading-dot-special-cases" $ do
  let
    components_posix x = let p = fromChar8 x in
      (toChar8 (P.directory p), toChar8 (basename p), P.extensions p)
    components_windows x = let p = fromString x in
      (toString (P.directory p), toString (basename p), P.extensions p)

  -- The filenames "." and ".." are always considered to be directory
  -- elements, because they are links to either the current or parent
  -- directories.
  --
  -- On POSIX, filenames starting with '.' are hidden files and have
  -- a basename starting with '.'.
  --
  -- On Windows there is no history of similar naming patterns. The case
  -- could be made that a filename like ".txt.gz" ought to have a
  -- basename of "", but different basename behavior in POSIX and Windows
  -- would greatly complicate the implementation of 'dirname'.
  $expect $ equal (components_posix ".") ("./", "", [])
  $expect $ equal (components_posix "..") ("../", "", [])
  $expect $ equal (components_posix "/foo/.") ("/foo/./", "", [])
  $expect $ equal (components_posix "/foo/..") ("/foo/../", "", [])
  $expect $ equal (components_posix "/foo/.foo.txt") ("/foo/", ".foo", ["txt"])

  $expect $ equal (components_windows ".") (".\\", "", [])
  $expect $ equal (components_windows "..") ("..\\", "", [])
  $expect $ equal (components_windows "\\foo\\.") ("\\foo\\.\\", "", [])
  $expect $ equal (components_windows "\\foo\\..") ("\\foo\\..\\", "", [])
  $expect $ equal (components_windows "\\foo\\.foo.txt") ("\\foo\\", ".foo", ["txt"])

test_Identity :: String -> Rules a -> Gen FilePath -> Test
test_Identity name r gen = property name $ forAll gen $ \p -> p == decode r (encode r p)

test_MixedValidityToBytes :: Test
test_MixedValidityToBytes = assertions "mixed-validity-to-bytes" $ do
  let p = fromChar8

  $expect $ equal (encode posix (p "\xB1.\xDD\xAA")) (B8.pack "\xB1.\xDD\xAA")
  $expect $ equal (encode posix (p "\xB1.\xDD\xAA" </> p "foo")) (B8.pack "\xB1.\xDD\xAA/foo")

test_ToText :: Test
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

test_FromText :: Test
test_FromText = assertions "fromText" $ do
  let pt x = fromText posix (T.pack x)
  let p = fromChar8

  $expect $ equal (pt "") (p "")
  $expect $ equal (pt "\x1D11E") (p "\xF0\x9D\x84\x9E")
  $expect $ equal (pt "\xED\xA0\x80") (p "\xC3\xAD\xC2\xA0\xC2\x80")

test_Append :: Test
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

test_CommonPrefix :: Test
test_CommonPrefix = assertions "commonPrefix" $ do
  let commonPrefix xs = toChar8 (P.commonPrefix (map (fromChar8) xs))

  $expect $ equal (commonPrefix ["", ""]) ""
  $expect $ equal (commonPrefix ["/", ""]) ""
  $expect $ equal (commonPrefix ["/", "/"]) "/"
  $expect $ equal (commonPrefix ["foo/", "/foo/"]) ""
  $expect $ equal (commonPrefix ["/foo", "/foo/"]) "/"
  $expect $ equal (commonPrefix ["/foo/", "/foo/"]) "/foo/"
  $expect $ equal (commonPrefix ["/foo/bar/baz.txt.gz", "/foo/bar/baz.txt.gz.bar"]) "/foo/bar/baz.txt.gz"

test_StripPrefix :: Test
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

test_SplitExtension :: Test
test_SplitExtension = assertions "splitExtension" $ do
  let
    splitExtension x = (toChar8 base, ext) where
      (base, ext) = P.splitExtension (fromChar8 x)

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
  $expect $ equal (collapse "/parent/foo/../../../bar") "/bar"
  $expect $ equal (collapse "/./parent/foo") "/parent/foo"

test_SplitDirectories :: Test
test_SplitDirectories = assertions "splitDirectories" $ do
  let splitDirectories x = P.splitDirectories (fromChar8 x)
      fromChar8' = map fromChar8

  $expect $ equal (splitDirectories "") (fromChar8' [])
  $expect $ equal (splitDirectories "/") (fromChar8' ["/"])
  $expect $ equal (splitDirectories "/a") (fromChar8' ["/", "a"])
  $expect $ equal (splitDirectories "/ab/cd") (fromChar8' ["/", "ab/", "cd"])
  $expect $ equal (splitDirectories "/ab/cd/") (fromChar8' ["/", "ab/", "cd/"])
  $expect $ equal (splitDirectories "ab/cd") (fromChar8' ["ab/", "cd"])
  $expect $ equal (splitDirectories "ab/cd/") (fromChar8' ["ab/", "cd/"])
  $expect $ equal (splitDirectories "ab/cd.txt") (fromChar8' ["ab/", "cd.txt"])
  $expect $ equal (splitDirectories "ab/cd/.txt") (fromChar8' ["ab/", "cd/", ".txt"])
  $expect $ equal (splitDirectories "ab/./cd") (fromChar8' ["ab/", ".", "cd"])

test_InvalidUtf8InDirectoryComponent :: Test
test_InvalidUtf8InDirectoryComponent = assertions "invalid-utf8-in-directory-component" $ do
  $expect $ equal (toText posix (fromChar8 "/\218\130.\137\141")) (Left (T.pack "/\1666.\137\141"))
  $expect $ equal (encode posix (fromChar8 "/\218\130.\137\141")) (B8.pack "/\218\130.\137\141")

  $expect $ equal (toText posix (fromChar8 "/\218\130.\137\141/")) (Left (T.pack "/\1666.\137\141/"))
  $expect $ equal (encode posix (fromChar8 "/\218\130.\137\141/")) (B8.pack "/\218\130.\137\141/")

  $expect $ equal (toText posix (fromChar8 "/\218\130.\137\141//baz")) (Left (T.pack "/\1666.\137\141/baz"))
  $expect $ equal (encode posix (fromChar8 "/\218\130.\137\141//baz")) (B8.pack "/\218\130.\137\141/baz")

test_Utf8CharInGhcEscapeArea :: Test
test_Utf8CharInGhcEscapeArea = assertions "utf8-char-in-ghc-escape-area" $ do
  let chars = "/a/\238\189\178/b"
  let path = fromChar8 chars
  $expect (equal (toChar8 path) chars)
  $expect (equal (toText posix path) (Right (T.pack "/a/\61298/b")))

  let chars = "/a/\xEE\xBC\x80/b"
  let path = fromChar8 chars
  $expect (equal (toChar8 path) chars)
  $expect (equal (toText posix path) (Right (T.pack "/a/\61184/b")))

test_Parsing :: Test
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

  -- extended-length
  $expect $ equal (w "\\\\?\\C:\\") "\\\\?\\C:\\"
  $expect $ equal (w "\\\\?\\C:\\a") "\\\\?\\C:\\a"

  -- UNC
  $expect $ equal (w "\\\\server\\share") "\\\\server\\share"
  $expect $ equal (w "\\\\server\\share\\") "\\\\server\\share"
  $expect $ equal (w "\\\\server\\share\\a") "\\\\server\\share\\a"
  $expect $ equal (w "\\\\server\\share\\.") "\\\\server\\share\\."
  $expect $ equal (w "\\\\server\\share\\..") "\\\\server\\share\\.."
  $expect $ equal (w "\\\\server\\share\\a\\") "\\\\server\\share\\a"

  -- extended-length UNC
  $expect $ equal (w "\\\\?\\unc\\server\\share") "\\\\?\\UNC\\server\\share"
  $expect $ equal (w "\\\\?\\UNC\\server\\share") "\\\\?\\UNC\\server\\share"
  $expect $ equal (w "\\\\?\\UNC\\server\\share\\a") "\\\\?\\UNC\\server\\share\\a"

  -- magical \??\ and \\.\ Windows prefixes
  $expect $ equal (w "\\??\\Volume{12345}") "\\??\\Volume{12345}"
  $expect $ equal (w "\\\\.\\PhysicalDrive0") "\\\\.\\PhysicalDrive0"

test_UncValidity :: Test
test_UncValidity = assertions "unc-validity" $ do
  let invalid rules = not . valid rules
  $expect $ invalid windows (fromString "\\\\server")
  $expect $ invalid windows (fromString "\\\\server\\")
  $expect $ valid windows (fromString "\\\\server\\share")
  $expect $ valid windows (fromString "\\\\server\\share\\")
  $expect $ valid windows (fromString "\\\\server\\share\\a")
  $expect $ valid windows (fromString "\\??\\Volume{12345}")
  $expect $ valid windows (fromString "\\\\.\\PhysicalDrive0")

test_SplitSearchPath :: Test
test_SplitSearchPath = assertions "splitSearchPath" $ do
  let p x = map toChar8 (splitSearchPath posix (B8.pack x))
  let w x = map toString (splitSearchPath windows (T.pack x))

  $expect $ equal (p "a:b:c") ["a", "b", "c"]
  $expect $ equal (p "a::b:c") ["a", "./", "b", "c"]
  $expect $ equal (w "a;b;c") ["a", "b", "c"]
  $expect $ equal (w "a;;b;c") ["a", "b", "c"]

suite_SplitSearchPathString :: Suite
suite_SplitSearchPathString = suite "splitSearchPathString"
  [ test_SplitSearchPathString_Posix
  , test_SplitSearchPathString_Posix_Ghc702
  , test_SplitSearchPathString_Posix_Ghc704
  , test_SplitSearchPathString_Darwin
  , test_SplitSearchPathString_Darwin_Ghc702
  , test_SplitSearchPathString_Win32
  ]

test_SplitSearchPathString_Posix :: Test
test_SplitSearchPathString_Posix = assertions "posix" $ do
  let split x = map (toText posix) (splitSearchPathString posix x)
  $expect $ equal (split "a::\xC2\xA1\xC2\xA2:\xA1\xA2") [Right "a", Right "./", Right "\xA1\xA2", Left "\xA1\xA2"]

test_SplitSearchPathString_Posix_Ghc702 :: Test
test_SplitSearchPathString_Posix_Ghc702 = assertions "posix_ghc702" $ do
  let split x = map (toText posix) (splitSearchPathString posix_ghc702 x)
  $expect $ equal (split "a::\xA1\xA2:\xEFA1\xEFA2") [Right "a", Right "./", Right "\xA1\xA2", Left "\xA1\xA2"]

test_SplitSearchPathString_Posix_Ghc704 :: Test
test_SplitSearchPathString_Posix_Ghc704 = assertions "posix_ghc704" $ do
  let split x = map (toText posix) (splitSearchPathString posix_ghc704 x)
  $expect $ equal (split "a::\xA1\xA2:\xDCA1\xDCA2") [Right "a", Right "./", Right "\xA1\xA2", Left "\xA1\xA2"]

test_SplitSearchPathString_Darwin :: Test
test_SplitSearchPathString_Darwin = assertions "darwin" $ do
  let split x = map (toText darwin) (splitSearchPathString darwin x)
  $expect $ equal (split "a::\xC2\xA1\xC2\xA2") [Right "a", Right "./", Right "\xA1\xA2"]

test_SplitSearchPathString_Darwin_Ghc702 :: Test
test_SplitSearchPathString_Darwin_Ghc702 = assertions "darwin_ghc702" $ do
  let split x = map (toText darwin) (splitSearchPathString darwin_ghc702 x)
  $expect $ equal (split "a::\xA1\xA2") [Right "a", Right "./", Right "\xA1\xA2"]

test_SplitSearchPathString_Win32 :: Test
test_SplitSearchPathString_Win32 = assertions "win32" $ do
  let split x = map (toText windows) (splitSearchPathString windows x)
  $expect $ equal (split "a;;\xA1\xA2") [Right "a", Right "\xA1\xA2"]

suite_EncodeString :: Suite
suite_EncodeString = suite "encodeString"
  [ test_EncodeString_Posix
  , test_EncodeString_Posix_Ghc702
  , test_EncodeString_Posix_Ghc704
  , test_EncodeString_Win32
  ]

test_EncodeString_Posix :: Test
test_EncodeString_Posix = assertions "posix" $ do
  let enc = encodeString posix
  $expect $ equal (enc (fromChar8 "test")) "test"
  $expect $ equal (enc (fromChar8 "test\xC2\xA1\xC2\xA2")) "test\xC2\xA1\xC2\xA2"
  $expect $ equal (enc (fromChar8 "test\xA1\xA2")) "test\xA1\xA2"
  $expect $ equal (enc (fromChar8 "\xC2\xA1\xC2\xA2/test\xA1\xA2")) "\xC2\xA1\xC2\xA2/test\xA1\xA2"
  $expect $ equal (enc (fromText posix "test\xA1\xA2")) "test\xC2\xA1\xC2\xA2"

test_EncodeString_Posix_Ghc702 :: Test
test_EncodeString_Posix_Ghc702 = assertions "posix_ghc702" $ do
  let enc = encodeString posix_ghc702
  $expect $ equal (enc (fromChar8 "test")) "test"
  $expect $ equal (enc (fromChar8 "test\xA1\xA2")) "test\xEFA1\xEFA2"
  $expect $ equal (enc (fromChar8 "\xC2\xA1\xC2\xA2/test\xA1\xA2")) "\xA1\xA2/test\xEFA1\xEFA2"
  $expect $ equal (enc (fromText posix_ghc702 "test\xA1\xA2")) "test\xA1\xA2"

test_EncodeString_Posix_Ghc704 :: Test
test_EncodeString_Posix_Ghc704 = assertions "posix_ghc704" $ do
  let enc = encodeString posix_ghc704
  $expect $ equal (enc (fromChar8 "test")) "test"
  $expect $ equal (enc (fromChar8 "test\xA1\xA2")) "test\xDCA1\xDCA2"
  $expect $ equal (enc (fromChar8 "\xC2\xA1\xC2\xA2/test\xA1\xA2")) "\xA1\xA2/test\xDCA1\xDCA2"
  $expect $ equal (enc (fromText posix_ghc704 "test\xA1\xA2")) "test\xA1\xA2"

test_EncodeString_Win32 :: Test
test_EncodeString_Win32 = assertions "windows" $ do
  let enc = encodeString windows
  $expect $ equal (enc (fromString "test")) "test"
  $expect $ equal (enc (fromString "test\xA1\xA2")) "test\xA1\xA2"
  $expect $ equal (enc (fromText windows "test\xA1\xA2")) "test\xA1\xA2"

suite_DecodeString :: Suite
suite_DecodeString = suite "decodeString"
  [ test_DecodeString_Posix
  , test_DecodeString_Posix_Ghc702
  , test_DecodeString_Posix_Ghc704
  , test_DecodeString_Darwin
  , test_DecodeString_Darwin_Ghc702
  , test_DecodeString_Win32
  ]

test_DecodeString_Posix :: Test
test_DecodeString_Posix = assertions "posix" $ do
  let r = posix
  let dec = decodeString
  $expect $ equal (dec r "test") (fromText r "test")
  $expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xA1\xA2")
  $expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")

test_DecodeString_Posix_Ghc702 :: Test
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

test_DecodeString_Posix_Ghc704 :: Test
test_DecodeString_Posix_Ghc704 = assertions "posix_ghc704" $ do
  let r = posix_ghc704
  let dec = decodeString
  $expect $ equal (dec r "test") (fromText r "test")
  $expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xC2\xA1\xC2\xA2")
  $expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")
  $expect $ equal (dec r "test\xDCA1\xDCA2") (fromChar8 "test\xA1\xA2")
  $expect $ equal
    (toText r (dec r "test\xDCA1\xDCA2"))
    (Left "test\xA1\xA2")

test_DecodeString_Darwin :: Test
test_DecodeString_Darwin = assertions "darwin" $ do
  let r = darwin
  let dec = decodeString
  $expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xA1\xA2")

test_DecodeString_Darwin_Ghc702 :: Test
test_DecodeString_Darwin_Ghc702 = assertions "darwin_ghc702" $ do
  let r = darwin_ghc702
  let dec = decodeString
  $expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xC2\xA1\xC2\xA2")
  $expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")

test_DecodeString_Win32 :: Test
test_DecodeString_Win32 = assertions "windows" $ do
  let r = windows
  let dec = decodeString
  $expect $ equal (dec r "test") (fromText r "test")
  $expect $ equal (dec r "test\xC2\xA1\xC2\xA2") (fromText r "test\xC2\xA1\xC2\xA2")
  $expect $ equal (dec r "test\xA1\xA2") (fromText r "test\xA1\xA2")

test_EqualsIgnoresPosixEncoding :: Test
test_EqualsIgnoresPosixEncoding = assertions "equals-ignores-posix-encoding" $ do
  $expect $ equal
    (fromChar8 "test\xA1\xA2")
    (fromText posix "test\xA1\xA2")

test_ShowRules :: Test
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
windowsPaths = oneof [dosPaths, uncPaths]

dosPaths :: Gen FilePath
dosPaths = sized $ \n -> genComponents n >>= merge where
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
  validComponent c = not (elem (map toUpper c) reservedNames)
  component = do
    size <- choose (1, 10)
    vectorOf size $ arbitrary `suchThat` validChar
  genComponents n = vectorOf n (component `suchThat` validComponent)

  genRoot = do
    let upperChar = elements ['A'..'Z']
    label <- frequency [(1, return Nothing), (9, fmap Just upperChar)]
    return $ case label of
      Just c -> [c, ':', '\\']
      Nothing -> "\\"

uncPaths :: Gen FilePath
uncPaths = sized $ \n -> genComponents n >>= merge where
  merge cs = do
    root <- genRoot
    let path = intercalate "\\" cs
    return $ case cs of
      [] -> fromString (root ++ path)
      _ -> fromString (root ++ "\\" ++ path)
  validChar c = c /= '\x00' && c /= '\\'
  component = do
    size <- choose (1, 10)
    vectorOf size (arbitrary `suchThat` validChar)
  genComponents n = vectorOf n component

  genRoot = do
    host <- component
    share <- component
    return ("\\\\" ++ host ++ "\\" ++ share)

toChar8 :: FilePath -> String
toChar8 = B8.unpack . encode posix

fromChar8 :: String -> FilePath
fromChar8 = decode posix . B8.pack

toString :: FilePath -> String
toString = T.unpack . encode windows

fromString :: String -> FilePath
fromString = decode windows . T.pack
