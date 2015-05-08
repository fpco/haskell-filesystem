{-# LANGUAGE OverloadedStrings #-}

module Filesystem.Path
       ( FilePath

         -- * Separator Predicates
       , F.extSeparator
       , F.isExtSeparator
       , F.isPathSeparator
       , F.isSearchPathSeparator
       , F.pathSeparator
       , F.pathSeparators
       , F.searchPathSeparator

         -- * @$PATH@ Methods
       , getSearchPath
       , splitSearchPath

         -- * Extension Functions
       , (-<.>)
       , (<.>)
       , addExtension
       , dropExtension
       , dropExtensions
       , hasExtension
       , replaceExtension
       , splitExtension
       , splitExtensions
       , takeExtension
       , takeExtensions

         -- * Filename\/Directory Functions
       , (</>)
       , combine
       , dropFileName
       , joinPath
       , replaceBaseName
       , replaceDirectory
       , replaceFileName
       , splitDirectories
       , splitFileName
       , splitPath
       , takeBaseName
       , takeDirectory
       , takeFileName

         -- * Drive Functions
       , dropDrive
       , hasDrive
       , isDrive
       , joinDrive
       , splitDrive
       , takeDrive

         -- * Trailing Slash Functions
       , addTrailingPathSeparator
       , dropTrailingPathSeparator
       , hasTrailingPathSeparator

         -- * File Name Manipulations
       , equalFilePath
       , isAbsolute
       , isRelative
       , isValid
       , makeRelative
       , makeValid
       , normalise
       ) where

import           Data.Monoid
import           Data.String
import           Prelude hiding (FilePath, concat, null)
import qualified System.FilePath as F

newtype FilePath =
  FilePath {unFilePath :: F.FilePath}
  deriving (Eq,Ord)

instance Monoid FilePath where
  mempty = ""
  mappend = combine
  mconcat = foldl mappend mempty

instance Show FilePath where
  show = show . unFilePath

instance IsString FilePath where
  fromString = FilePath

---------------------
-- @$PATH@ Methods --
---------------------

getSearchPath :: IO [FilePath]
getSearchPath = fmap (map fromString) F.getSearchPath

splitSearchPath :: String -> [FilePath]
splitSearchPath = map fromString . F.splitSearchPath

-------------------------
-- Extension Functions --
-------------------------

addExtension :: FilePath -> String -> FilePath
addExtension x y =
  apply (flip F.addExtension y) x

dropExtension :: FilePath -> FilePath
dropExtension = apply F.dropExtension

dropExtensions :: FilePath -> FilePath
dropExtensions = apply F.dropExtensions

hasExtension :: FilePath -> Bool
hasExtension = F.hasExtension . unFilePath

replaceExtension :: FilePath -> String -> FilePath
replaceExtension x y =
  apply (flip F.replaceExtension y) x

splitExtension :: FilePath -> (FilePath, String)
splitExtension p =
  let (path,ext) =
        F.splitExtension (unFilePath p)
  in (fromString path,ext)

splitExtensions :: FilePath -> (FilePath, String)
splitExtensions p =
  let (path,ext) =
        F.splitExtension (unFilePath p)
  in (fromString path,ext)

(-<.>) :: FilePath -> String -> FilePath
(-<.>) = replaceExtension

(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

takeExtension :: FilePath -> String
takeExtension = F.takeExtension . unFilePath

takeExtensions :: FilePath -> String
takeExtensions = F.takeExtensions . unFilePath

-----------------------------------
-- Filename\/Directory Functions --
-----------------------------------

(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

combine :: FilePath -> FilePath -> FilePath
combine p = apply (F.combine (unFilePath p))

dropFileName :: FilePath -> FilePath
dropFileName = apply F.dropFileName

joinPath :: [FilePath] -> FilePath
joinPath = fromString . F.joinPath . map unFilePath

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName x y =
  apply (flip F.replaceBaseName y) x

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory x y =
  apply (flip F.replaceDirectory y) x

replaceFileName :: FilePath -> String -> FilePath
replaceFileName x y =
  apply (flip F.replaceFileName y) x

splitDirectories :: FilePath -> [FilePath]
splitDirectories = map fromString . F.splitDirectories . unFilePath

splitFileName :: FilePath -> (FilePath, String)
splitFileName p =
  let (path,file) =
        F.splitFileName (unFilePath p)
  in (fromString path,file)

splitPath :: FilePath -> [FilePath]
splitPath = map fromString . F.splitPath . unFilePath

takeBaseName :: FilePath -> String
takeBaseName = F.takeBaseName . unFilePath

takeDirectory :: FilePath -> FilePath
takeDirectory = apply F.takeDirectory

takeFileName :: FilePath -> FilePath
takeFileName = apply F.takeFileName

---------------------
-- Drive Functions --
---------------------

dropDrive :: FilePath -> FilePath
dropDrive = apply F.dropDrive

hasDrive :: FilePath -> Bool
hasDrive = F.hasDrive . unFilePath

isDrive :: FilePath -> Bool
isDrive = F.isDrive . unFilePath

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive p = apply (F.joinDrive (unFilePath p))

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive p =
  let (drive,path) = F.splitDrive (unFilePath p)
  in (fromString drive,fromString path)

takeDrive :: FilePath -> FilePath
takeDrive = apply F.takeDrive

------------------------------
-- Trailing Slash Functions --
------------------------------

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator = apply F.addTrailingPathSeparator

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator = apply F.dropTrailingPathSeparator

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator = F.hasTrailingPathSeparator . unFilePath

-----------------------------
-- File Name Manipulations --
-----------------------------

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath x y =
  F.equalFilePath (unFilePath x)
                  (unFilePath y)

isAbsolute :: FilePath -> Bool
isAbsolute = F.isAbsolute . unFilePath

isRelative :: FilePath -> Bool
isRelative = F.isRelative . unFilePath

isValid :: FilePath -> Bool
isValid = F.isValid . unFilePath

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative p =
  apply (F.makeRelative (unFilePath p))

makeValid :: FilePath -> FilePath
makeValid = apply F.makeValid

normalise :: FilePath -> FilePath
normalise = apply F.normalise

--------------
-- Internal --
--------------

apply :: (F.FilePath -> F.FilePath) -> FilePath -> FilePath
apply f = fromString . f . unFilePath
