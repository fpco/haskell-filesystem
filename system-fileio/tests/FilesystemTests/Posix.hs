{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module FilesystemTests.Posix
  ( suite_Posix
  ) where

import           Prelude hiding (FilePath)
import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Foreign
import           Foreign.C
import           Test.Chell

#if MIN_VERSION_base(4,2,0)
import qualified GHC.IO.Exception as GHC
#else
import qualified GHC.IOBase as GHC
#endif

import qualified System.Posix.IO as PosixIO

import           Filesystem
import           Filesystem.Path
import qualified Filesystem.Path.Rules as Rules
import qualified Filesystem.Path.CurrentOS as CurrentOS

import           FilesystemTests.Util (assertionsWithTemp, todo)

suite_Posix :: Suite
suite_Posix = suite "posix" $
  (concatMap suiteTests
    [ suite_IsFile
    , suite_IsDirectory
    , suite_Rename
    , suite_CanonicalizePath
    , suite_CreateDirectory
    , suite_CreateTree
    , suite_RemoveFile
    , suite_RemoveDirectory
    , suite_RemoveTree
    , suite_GetWorkingDirectory
    , suite_SetWorkingDirectory
    , suite_GetHomeDirectory
    , suite_GetDesktopDirectory
    , suite_GetModified
    , suite_GetSize
    , suite_CopyFile
    , suite_WithFile
    , suite_WithTextFile
    , suite_RegressionTests
    ]) ++
  [ test_ListDirectory
  , todo "getDocumentsDirectory"
  , todo "getAppDataDirectory"
  , todo "getAppCacheDirectory"
  , todo "getAppConfigDirectory"
  , todo "openFile"
  , todo "readFile"
  , todo "writeFile"
  , todo "appendFile"
  , todo "openTextFile"
  , todo "readTextFile"
  , todo "writeTextFile"
  , todo "appendTextFile"
  ]

suite_IsFile :: Suite
suite_IsFile = suite "isFile"
  [ test_IsFile "ascii" (decode "test.txt")
  , test_IsFile "utf8" (fromText "\xA1\xA2.txt")
  , test_IsFile "iso8859" (decode "\xA1\xA2\xA3.txt")
  , test_PipeIsFile "pipe.ascii" (decode "test.txt")
  , test_PipeIsFile "pipe.utf8" (fromText "\xA1\xA2.txt")
  , test_PipeIsFile "pipe.iso8859" (decode "\xA1\xA2\xA3.txt")
  ]

suite_IsDirectory :: Suite
suite_IsDirectory = suite "isDirectory"
  [ test_IsDirectory "ascii" (decode "test.d")
  , test_IsDirectory "utf8" (fromText "\xA1\xA2.d")
  , test_IsDirectory "iso8859" (decode "\xA1\xA2\xA3.d")
  ]

suite_Rename :: Suite
suite_Rename = suite "rename"
  [ test_Rename "ascii"
    (decode "old_test.txt")
    (decode "new_test.txt")
  , test_Rename "utf8"
    (fromText "old_\xA1\xA2.txt")
    (fromText "new_\xA1\xA2.txt")
  , test_Rename "iso8859"
    (decode "old_\xA1\xA2\xA3.txt")
    (decode "new_\xA1\xA2\xA3.txt")
  ]

suite_CanonicalizePath :: Suite
suite_CanonicalizePath = suite "canonicalizePath"
  [ test_CanonicalizePath "ascii"
    (decode "test-a.txt")
    (decode "test-b.txt")
  , test_CanonicalizePath "utf8"
    (fromText "\xA1\xA2-a.txt")
    (fromText "\xA1\xA2-b.txt")
  , test_CanonicalizePath "iso8859"
    (decode "\xA1\xA2\xA3-a.txt")
#ifdef CABAL_OS_DARWIN
    (decode "%A1%A2%A3-b.txt")
#else
    (decode "\xA1\xA2\xA3-b.txt")
#endif
  , test_CanonicalizePath_TrailingSlash
  ]

suite_CreateDirectory :: Suite
suite_CreateDirectory = suite "createDirectory"
  [ test_CreateDirectory "ascii"
    (decode "test.d")
  , test_CreateDirectory "utf8"
    (fromText "\xA1\xA2.d")
  , test_CreateDirectory "iso8859"
    (decode "\xA1\xA2\xA3.d")
  , test_CreateDirectory_FailExists
  , test_CreateDirectory_SucceedExists
  , test_CreateDirectory_FailFileExists
  ]

suite_CreateTree :: Suite
suite_CreateTree = suite "createTree"
  [ test_CreateTree "ascii"
    (decode "test.d")
  , test_CreateTree "ascii-slash"
    (decode "test.d/")
  , test_CreateTree "utf8"
    (fromText "\xA1\xA2.d")
  , test_CreateTree "utf8-slash"
    (fromText "\xA1\xA2.d/")
  , test_CreateTree "iso8859"
    (decode "\xA1\xA2\xA3.d")
  , test_CreateTree "iso8859-slash"
    (decode "\xA1\xA2\xA3.d/")
  ]

suite_RemoveFile :: Suite
suite_RemoveFile = suite "removeFile"
  [ test_RemoveFile "ascii"
    (decode "test.txt")
  , test_RemoveFile "utf8"
    (fromText "\xA1\xA2.txt")
  , test_RemoveFile "iso8859"
    (decode "\xA1\xA2\xA3.txt")
  ]

suite_RemoveDirectory :: Suite
suite_RemoveDirectory = suite "removeDirectory"
  [ test_RemoveDirectory "ascii"
    (decode "test.d")
  , test_RemoveDirectory "utf8"
    (fromText "\xA1\xA2.d")
  , test_RemoveDirectory "iso8859"
    (decode "\xA1\xA2\xA3.d")
  ]

suite_RemoveTree :: Suite
suite_RemoveTree = suite "removeTree"
  [ test_RemoveTree "ascii"
    (decode "test.d")
  , test_RemoveTree "utf8"
    (fromText "\xA1\xA2.d")
  , test_RemoveTree "iso8859"
    (decode "\xA1\xA2\xA3.d")
  ]

suite_GetWorkingDirectory :: Suite
suite_GetWorkingDirectory = suite "getWorkingDirectory"
  [ test_GetWorkingDirectory "ascii"
    (decode "test.d")
  , test_GetWorkingDirectory "utf8"
    (fromText "\xA1\xA2.d")
  , test_GetWorkingDirectory "iso8859"
    (decode "\xA1\xA2\xA3.d")
  ]

suite_SetWorkingDirectory :: Suite
suite_SetWorkingDirectory = suite "setWorkingDirectory"
  [ test_SetWorkingDirectory "ascii"
    (decode "test.d")
  , test_SetWorkingDirectory "utf8"
    (fromText "\xA1\xA2.d")
  , test_SetWorkingDirectory "iso8859"
    (decode "\xA1\xA2\xA3.d")
  ]

suite_GetHomeDirectory :: Suite
suite_GetHomeDirectory = suite "getHomeDirectory"
  [ test_GetHomeDirectory "ascii"
    (decode "/home/test.d")
  , test_GetHomeDirectory "utf8"
    (decode "/home/\xA1\xA2.d")
  , test_GetHomeDirectory "iso8859"
    (decode "/home/\xA1\xA2\xA3.d")
  ]

suite_GetDesktopDirectory :: Suite
suite_GetDesktopDirectory = suite "getDesktopDirectory"
  [ test_GetDesktopDirectory "ascii"
    (decode "/desktop/test.d")
  , test_GetDesktopDirectory "utf8"
    (decode "/desktop/\xA1\xA2.d")
  , test_GetDesktopDirectory "iso8859"
    (decode "/desktop/\xA1\xA2\xA3.d")
  ]

suite_GetModified :: Suite
suite_GetModified = suite "getModified"
  [ test_GetModified "ascii"
    (decode "test.txt")
  , test_GetModified "utf8"
    (fromText "\xA1\xA2.txt")
  , test_GetModified "iso8859"
    (decode "\xA1\xA2\xA3.txt")
  ]

suite_GetSize :: Suite
suite_GetSize = suite "getSize"
  [ test_GetSize "ascii"
    (decode "test.txt")
  , test_GetSize "utf8"
    (fromText "\xA1\xA2.txt")
  , test_GetSize "iso8859"
    (decode "\xA1\xA2\xA3.txt")
  ]

suite_CopyFile :: Suite
suite_CopyFile = suite "copyFile"
  [ test_CopyFile "ascii"
    (decode "old_test.txt")
    (decode "new_test.txt")
  , test_CopyFile "utf8"
    (fromText "old_\xA1\xA2.txt")
    (fromText "new_\xA1\xA2.txt")
  , test_CopyFile "iso8859"
#ifdef CABAL_OS_DARWIN
    (decode "old_%A1%A2%A3.txt")
#else
    (decode "old_\xA1\xA2\xA3.txt")
#endif
#ifdef CABAL_OS_DARWIN
    (decode "new_%A1%A2%A3.txt")
#else
    (decode "new_\xA1\xA2\xA3.txt")
#endif
  ]

suite_WithFile :: Suite
suite_WithFile = suite "withFile"
  [ test_WithFile_Read "read.ascii"
    (decode "test.txt")
  , test_WithFile_Read "read.utf8"
    (fromText "\xA1\xA2.txt")
  , test_WithFile_Read "read.iso8859"
#ifdef CABAL_OS_DARWIN
    (decode "%A1%A2%A3.txt")
#else
    (decode "\xA1\xA2\xA3.txt")
#endif
  , test_WithFile_Write "write.ascii"
    (decode "test.txt")
  , test_WithFile_Write "write.utf8"
    (fromText "\xA1\xA2.txt")
  , test_WithFile_Write "write.iso8859"
    (decode "\xA1\xA2\xA3.txt")
  ]

suite_WithTextFile :: Suite
suite_WithTextFile = suite "withTextFile"
  [ test_WithTextFile "ascii"
    (decode "test.txt")
  , test_WithTextFile "utf8"
    (fromText "\xA1\xA2.txt")
  , test_WithTextFile "iso8859"
#ifdef CABAL_OS_DARWIN
    (decode "%A1%A2%A3.txt")
#else
    (decode "\xA1\xA2\xA3.txt")
#endif
  ]

suite_RegressionTests :: Suite
suite_RegressionTests = suite "regression-tests"
  [ test_ListDirectoryLeaksFds
  ]

test_IsFile :: String -> FilePath -> Test
test_IsFile test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let path = tmp </> file_name

  before <- liftIO $ Filesystem.isFile path
  $expect (not before)

  touch_ffi path "contents\n"

  after <- liftIO $ Filesystem.isFile path
  $expect after

test_PipeIsFile :: String -> FilePath -> Test
test_PipeIsFile test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let path = tmp </> file_name

  before <- liftIO $ Filesystem.isFile path
  $expect (not before)

  mkfifo_ffi path

  after <- liftIO $ Filesystem.isFile path
  $expect after

test_IsDirectory :: String -> FilePath -> Test
test_IsDirectory test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  let path = tmp </> dir_name

  before <- liftIO $ Filesystem.isDirectory path
  $expect (not before)

  mkdir_ffi path

  after <- liftIO $ Filesystem.isDirectory path
  $expect after

test_Rename :: String -> FilePath -> FilePath -> Test
test_Rename test_name old_name new_name = assertionsWithTemp test_name $ \tmp -> do
  let old_path = tmp </> old_name
  let new_path = tmp </> new_name

  touch_ffi old_path ""

  old_before <- liftIO $ Filesystem.isFile old_path
  new_before <- liftIO $ Filesystem.isFile new_path
  $expect old_before
  $expect (not new_before)

  liftIO $ Filesystem.rename old_path new_path

  old_after <- liftIO $ Filesystem.isFile old_path
  new_after <- liftIO $ Filesystem.isFile new_path
  $expect (not old_after)
  $expect new_after

test_CopyFile :: String -> FilePath -> FilePath -> Test
test_CopyFile test_name old_name new_name = assertionsWithTemp test_name $ \tmp -> do
  let old_path = tmp </> old_name
  let new_path = tmp </> new_name

  touch_ffi old_path ""

  old_before <- liftIO $ Filesystem.isFile old_path
  new_before <- liftIO $ Filesystem.isFile new_path
  $expect old_before
  $expect (not new_before)

  liftIO $ Filesystem.copyFile old_path new_path

  old_after <- liftIO $ Filesystem.isFile old_path
  new_after <- liftIO $ Filesystem.isFile new_path
  $expect old_after
  $expect new_after
  old_contents <- liftIO $
    Filesystem.withTextFile old_path ReadMode $
    Data.Text.IO.hGetContents
  new_contents <- liftIO $
    Filesystem.withTextFile new_path ReadMode $
    Data.Text.IO.hGetContents
  $expect (equalLines old_contents new_contents)

test_CanonicalizePath :: String -> FilePath -> FilePath -> Test
test_CanonicalizePath test_name src_name dst_name = assertionsWithTemp test_name $ \tmp -> do
  let src_path = tmp </> src_name
  let subdir = tmp </> "subdir"

  -- canonicalize the directory first, to avoid false negatives if
  -- it gets placed in a symlinked location.
  mkdir_ffi subdir
  canon_subdir <- liftIO (Filesystem.canonicalizePath subdir)

  let dst_path = canon_subdir </> dst_name

  touch_ffi dst_path ""
  symlink_ffi dst_path src_path

  canonicalized <- liftIO $ Filesystem.canonicalizePath src_path
  $expect $ equal canonicalized dst_path

test_CanonicalizePath_TrailingSlash :: Test
test_CanonicalizePath_TrailingSlash = assertionsWithTemp "trailing-slash" $ \tmp -> do
  let src_path = tmp </> "src"
  let subdir = tmp </> "subdir"

  -- canonicalize the directory first, to avoid false negatives if
  -- it gets placed in a symlinked location.
  mkdir_ffi subdir
  canon_subdir <- liftIO (Filesystem.canonicalizePath (tmp </> "subdir"))

  let dst_path = canon_subdir </> "dst"

  mkdir_ffi dst_path
  symlink_ffi dst_path src_path

  canonicalized <- liftIO (Filesystem.canonicalizePath (src_path </> empty))
  $expect (equal canonicalized (dst_path </> empty))

test_CreateDirectory :: String -> FilePath -> Test
test_CreateDirectory test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  let dir_path = tmp </> dir_name

  exists_before <- liftIO $ Filesystem.isDirectory dir_path
  $assert (not exists_before)

  liftIO $ Filesystem.createDirectory False dir_path
  exists_after <- liftIO $ Filesystem.isDirectory dir_path

  $expect exists_after

test_CreateDirectory_FailExists :: Test
test_CreateDirectory_FailExists = assertionsWithTemp "fail-if-exists" $ \tmp -> do
  let dir_path = tmp </> "subdir"
  mkdir_ffi dir_path

  $expect $ throwsEq
    (mkAlreadyExists "createDirectory" dir_path)
    (Filesystem.createDirectory False dir_path)

test_CreateDirectory_SucceedExists :: Test
test_CreateDirectory_SucceedExists = assertionsWithTemp "succeed-if-exists" $ \tmp -> do
  let dir_path = tmp </> "subdir"
  mkdir_ffi dir_path

  liftIO $ Filesystem.createDirectory True dir_path

test_CreateDirectory_FailFileExists :: Test
test_CreateDirectory_FailFileExists = assertionsWithTemp "fail-if-file-exists" $ \tmp -> do
  let dir_path = tmp </> "subdir"
  touch_ffi dir_path ""

  $expect $ throwsEq
    (mkAlreadyExists "createDirectory" dir_path)
    (Filesystem.createDirectory False dir_path)
  $expect $ throwsEq
    (mkAlreadyExists "createDirectory" dir_path)
    (Filesystem.createDirectory True dir_path)

mkAlreadyExists :: String -> FilePath -> GHC.IOError
mkAlreadyExists loc path = GHC.IOError Nothing GHC.AlreadyExists loc "File exists"
#if MIN_VERSION_base(4,2,0)
  (Just (errnoCInt eEXIST))
#endif
  (Just (CurrentOS.encodeString path))

test_CreateTree :: String -> FilePath -> Test
test_CreateTree test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  let dir_path = tmp </> dir_name
  let subdir = dir_path </> "subdir"

  dir_exists_before <- liftIO $ Filesystem.isDirectory dir_path
  subdir_exists_before <- liftIO $ Filesystem.isDirectory subdir
  $assert (not dir_exists_before)
  $assert (not subdir_exists_before)

  liftIO $ Filesystem.createTree subdir
  dir_exists_after <- liftIO $ Filesystem.isDirectory dir_path
  subdir_exists_after <- liftIO $ Filesystem.isDirectory subdir

  $expect dir_exists_after
  $expect subdir_exists_after

test_ListDirectory :: Test
test_ListDirectory = assertionsWithTemp "listDirectory" $ \tmp -> do
  -- OSX replaces non-UTF8 filenames with http-style %XX escapes
  let
    paths =
#ifdef CABAL_OS_DARWIN
      [ tmp </> decode "%A1%A2%A3.txt"
      , tmp </> decode "test.txt"
      , tmp </> fromText "\xA1\xA2.txt"
      ]
#else
      [ tmp </> decode "test.txt"
      , tmp </> fromText "\xA1\xA2.txt"
      , tmp </> decode "\xA1\xA2\xA3.txt"
      ]
#endif
  forM_ paths (\path -> touch_ffi path "")

  names <- liftIO $ Filesystem.listDirectory tmp
  $expect $ sameItems paths names

test_RemoveFile :: String -> FilePath -> Test
test_RemoveFile test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let file_path = tmp </> file_name

  touch_ffi file_path "contents\n"

  before <- liftIO $ Filesystem.isFile file_path
  $assert before

  liftIO $ Filesystem.removeFile file_path

  after <- liftIO $ Filesystem.isFile file_path
  $expect (not after)

test_RemoveDirectory :: String -> FilePath -> Test
test_RemoveDirectory test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  let dir_path = tmp </> dir_name

  mkdir_ffi dir_path

  before <- liftIO $ Filesystem.isDirectory dir_path
  $assert before

  liftIO $ Filesystem.removeDirectory dir_path

  after <- liftIO $ Filesystem.isDirectory dir_path
  $expect (not after)

test_RemoveTree :: String -> FilePath -> Test
test_RemoveTree test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  let dir_path = tmp </> dir_name
  let subdir = dir_path </> "subdir"

  mkdir_ffi dir_path
  mkdir_ffi subdir

  dir_before <- liftIO $ Filesystem.isDirectory dir_path
  subdir_before <- liftIO $ Filesystem.isDirectory subdir
  $assert dir_before
  $assert subdir_before

  liftIO $ Filesystem.removeTree dir_path

  dir_after <- liftIO $ Filesystem.isDirectory dir_path
  subdir_after <- liftIO $ Filesystem.isDirectory subdir
  $expect (not dir_after)
  $expect (not subdir_after)

test_GetWorkingDirectory :: String -> FilePath -> Test
test_GetWorkingDirectory test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  -- canonicalize to avoid issues with symlinked temp dirs
  canon_tmp <- liftIO (Filesystem.canonicalizePath tmp)
  let dir_path = canon_tmp </> dir_name

  mkdir_ffi dir_path
  chdir_ffi dir_path

  cwd <- liftIO $ Filesystem.getWorkingDirectory
  $expect (equal cwd dir_path)

test_SetWorkingDirectory :: String -> FilePath -> Test
test_SetWorkingDirectory test_name dir_name = assertionsWithTemp test_name $ \tmp -> do
  -- canonicalize to avoid issues with symlinked temp dirs
  canon_tmp <- liftIO (Filesystem.canonicalizePath tmp)
  let dir_path = canon_tmp </> dir_name

  mkdir_ffi dir_path
  liftIO $ Filesystem.setWorkingDirectory dir_path

  cwd <- getcwd_ffi
  $expect (equal cwd dir_path)

test_GetHomeDirectory :: String -> FilePath -> Test
test_GetHomeDirectory test_name dir_name = assertions test_name $ do
  path <- liftIO $ withEnv "HOME" (Just dir_name) Filesystem.getHomeDirectory
  $expect (equal path dir_name)

test_GetDesktopDirectory :: String -> FilePath -> Test
test_GetDesktopDirectory test_name dir_name = assertions test_name $ do
  path <- liftIO $
    withEnv "XDG_DESKTOP_DIR" (Just dir_name) $
    Filesystem.getDesktopDirectory
  $expect (equal path dir_name)

  fallback <- liftIO $
    withEnv "XDG_DESKTOP_DIR" Nothing $
    withEnv "HOME" (Just dir_name) $
    Filesystem.getDesktopDirectory
  $expect (equal fallback (dir_name </> "Desktop"))

test_GetModified :: String -> FilePath -> Test
test_GetModified test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let file_path = tmp </> file_name

  touch_ffi file_path ""
  now <- liftIO getCurrentTime

  mtime <- liftIO $ Filesystem.getModified file_path
  $expect (equalWithin (diffUTCTime mtime now) 0 2)

test_GetSize :: String -> FilePath -> Test
test_GetSize test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let file_path = tmp </> file_name
  let contents = "contents\n"

  touch_ffi file_path contents

  size <- liftIO $ Filesystem.getSize file_path
  $expect (equal size (toInteger (Data.ByteString.length contents)))

test_WithFile_Read :: String -> FilePath -> Test
test_WithFile_Read test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let file_path = tmp </> file_name
  let contents = "contents\n"

  touch_ffi file_path contents

  read_contents <- liftIO $
    Filesystem.withFile file_path ReadMode $
    Data.ByteString.hGetContents
  $expect (equalLines contents read_contents)

test_WithFile_Write :: String -> FilePath -> Test
test_WithFile_Write test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let file_path = tmp </> file_name
  let contents = "contents\n"

  liftIO $
    Filesystem.withFile file_path WriteMode $
    (\h -> Data.ByteString.hPut h contents)

  read_contents <- liftIO $
    Filesystem.withFile file_path ReadMode $
    Data.ByteString.hGetContents
  $expect (equalLines contents read_contents)

test_WithTextFile :: String -> FilePath -> Test
test_WithTextFile test_name file_name = assertionsWithTemp test_name $ \tmp -> do
  let file_path = tmp </> file_name
  let contents = "contents\n"

  touch_ffi file_path (Char8.pack contents)

  read_contents <- liftIO $
    Filesystem.withTextFile file_path ReadMode $
    Data.Text.IO.hGetContents
  $expect (equalLines (Data.Text.pack contents) read_contents)

test_ListDirectoryLeaksFds :: Test
test_ListDirectoryLeaksFds = assertionsWithTemp "listDirectory-leaks-fds" $ \tmp -> do
  -- Test that listDirectory doesn't leak file descriptors.
  let dir_path = tmp </> "subdir"
  mkdir_ffi dir_path

  nullfd1 <- liftIO openDevNull
  liftIO $ PosixIO.closeFd nullfd1

  _subdirContents <- liftIO $ listDirectory dir_path

  nullfd2 <- liftIO openDevNull
  liftIO $ PosixIO.closeFd nullfd2

  $assert (equal nullfd1 nullfd2)
  where
    openDevNull =
#if MIN_VERSION_unix(2,8,0)
      PosixIO.openFd "/dev/null" PosixIO.ReadOnly PosixIO.defaultFileFlags
#else
      PosixIO.openFd "/dev/null" PosixIO.ReadOnly Nothing PosixIO.defaultFileFlags
#endif

withPathCString :: FilePath -> (CString -> IO a) -> IO a
withPathCString p = Data.ByteString.useAsCString (encode p)

decode :: ByteString -> FilePath
decode = Rules.decode Rules.posix

encode :: FilePath -> ByteString
encode = Rules.encode Rules.posix

fromText :: Text -> FilePath
fromText = Rules.fromText Rules.posix

-- | Create a file using the raw POSIX API, via FFI
touch_ffi :: FilePath -> Data.ByteString.ByteString -> Assertions ()
touch_ffi path contents = do
  fp <- liftIO $ withPathCString path $ \path_cstr ->
    Foreign.C.withCString "wb" $ \mode_cstr ->
    c_fopen path_cstr mode_cstr

  $assert (fp /= nullPtr)

  _ <- liftIO $ Data.ByteString.useAsCStringLen contents $ \(buf, len) ->
    c_fwrite buf 1 (fromIntegral len) fp

  _ <- liftIO $ c_fclose fp
  return ()

-- | Create a directory using the raw POSIX API, via FFI
mkdir_ffi :: FilePath -> Assertions ()
mkdir_ffi path = do
  ret <- liftIO $ withPathCString path $ \path_cstr ->
    c_mkdir path_cstr 0o700

  $assert (ret == 0)

-- | Create a symlink using the raw POSIX API, via FFI
symlink_ffi :: FilePath -> FilePath -> Assertions ()
symlink_ffi dst src  = do
  ret <- liftIO $
    withPathCString dst $ \dst_p ->
    withPathCString src $ \src_p ->
    c_symlink dst_p src_p

  $assert (ret == 0)

-- | Create a FIFO using the raw POSIX API, via FFI
mkfifo_ffi :: FilePath -> Assertions ()
mkfifo_ffi path = do
  ret <- liftIO $ withPathCString path $ \path_cstr ->
    c_mkfifo path_cstr 0o700

  $assert (ret == 0)

getcwd_ffi :: Assertions FilePath
getcwd_ffi = do
  buf <- liftIO $ c_getcwd nullPtr 0
  $assert (buf /= nullPtr)
  bytes <- liftIO $ Data.ByteString.packCString buf
  liftIO $ c_free buf
  return (decode bytes)

chdir_ffi :: FilePath -> Assertions ()
chdir_ffi path = do
  ret <- liftIO $
    withPathCString path $ \path_p ->
    c_chdir path_p
  $assert (ret == 0)

errnoCInt :: Errno -> CInt
errnoCInt (Errno x) = x

withEnv :: ByteString -> Maybe FilePath -> IO a -> IO a
withEnv name val io = bracket set unset (\_ -> io) where
  set = do
    old <- getEnv name
    setEnv name (fmap encode val)
    return old
  unset = setEnv name

getEnv :: ByteString -> IO (Maybe ByteString)
getEnv name = Data.ByteString.useAsCString name $ \cName -> do
  ret <- liftIO (c_getenv cName)
  if ret == nullPtr
    then return Nothing
    else fmap Just (Data.ByteString.packCString ret)

setEnv :: ByteString -> Maybe ByteString -> IO ()
setEnv name Nothing = throwErrnoIfMinus1_ "setEnv" $
  Data.ByteString.useAsCString name c_unsetenv
setEnv name (Just val) = throwErrnoIfMinus1_ "setEnv" $
  Data.ByteString.useAsCString name $ \cName ->
  Data.ByteString.useAsCString val $ \cVal ->
  c_setenv cName cVal 1

foreign import ccall unsafe "fopen"
  c_fopen :: CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "fclose"
  c_fclose :: Ptr () -> IO CInt

foreign import ccall unsafe "fwrite"
  c_fwrite :: CString -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CInt -> IO CInt

foreign import ccall unsafe "symlink"
  c_symlink :: CString -> CString -> IO CInt

foreign import ccall unsafe "mkfifo"
  c_mkfifo :: CString -> CInt -> IO CInt

foreign import ccall unsafe "getcwd"
  c_getcwd :: CString -> CSize -> IO CString

foreign import ccall unsafe "chdir"
  c_chdir :: CString -> IO CInt

foreign import ccall unsafe "free"
  c_free :: Ptr a -> IO ()

foreign import ccall unsafe "getenv"
  c_getenv :: CString -> IO CString

foreign import ccall unsafe "setenv"
  c_setenv :: CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "unsetenv"
  c_unsetenv :: CString -> IO CInt
