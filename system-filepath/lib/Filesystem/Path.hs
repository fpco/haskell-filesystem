-- |
-- Copyright: 2015 FP Complete
-- License: MIT
-- Maintainer: dev@fpcomplete.com
-- Module: Filesystem.Path
-- Portability: portable
--
module Filesystem.Path
       ( FilePath
       , (<.>)
       , (</>)
       , absolute
       , addExtension
       , addExtensions
       , append
       , basename
       , collapse
       , commonPrefix
       , concat
       , directory
       , dirname
       , dropExtension
       , dropExtensions
       , empty
       , extension
       , extensions
       , filename
       , hasExtension
       , null
       , parent
       , relative
       , replaceExtension
       , replaceExtensions
       , root
       , splitDirectories
       , splitExtension
       , splitExtensions
       , stripPrefix
       ) where

import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Filesystem.Path.Internal as I
import           Prelude hiding (FilePath, null, concat)

newtype FilePath = FilePath { unFilePath :: B.ByteString }

instance IsString FilePath where
  fromString = FilePath . BC.pack

instance Show FilePath where
  show = BC.unpack . unFilePath

instance Monoid FilePath where
  mempty = empty
  mappend = append
  mconcat = concat

empty :: FilePath
empty = FilePath BC.empty

null :: FilePath -> Bool
null = I.null . toIFP

root :: FilePath -> FilePath
root = fromIFP . I.root . toIFP

directory :: FilePath -> FilePath
directory = fromIFP . I.directory . toIFP

parent :: FilePath -> FilePath
parent = fromIFP . I.parent . toIFP

filename :: FilePath -> FilePath
filename = fromIFP . I.filename . toIFP

dirname :: FilePath -> FilePath
dirname = fromIFP . I.dirname . toIFP

basename :: FilePath -> FilePath
basename = fromIFP . I.basename . toIFP

absolute :: FilePath -> Bool
absolute = I.absolute . toIFP

relative :: FilePath -> Bool
relative = I.relative . toIFP

append :: FilePath -> FilePath -> FilePath
append x y = fromIFP (I.append (toIFP x) (toIFP y))

(</>) :: FilePath -> FilePath -> FilePath
(</>) = append

concat :: [FilePath] -> FilePath
concat = fromIFP . I.concat . map toIFP

commonPrefix :: [FilePath] -> FilePath
commonPrefix = fromIFP . I.commonPrefix . map toIFP

stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix x y = fmap fromIFP (I.stripPrefix (toIFP x) (toIFP y))

collapse :: FilePath -> FilePath
collapse = fromIFP . I.collapse . toIFP
  
splitDirectories :: FilePath -> [FilePath]
splitDirectories = map fromIFP . I.splitDirectories . toIFP

extension :: FilePath -> Maybe T.Text
extension = I.extension . toIFP

extensions :: FilePath -> [T.Text]
extensions = I.extensions . toIFP

hasExtension :: FilePath -> T.Text -> Bool
hasExtension = I.hasExtension . toIFP

addExtension :: FilePath -> T.Text -> FilePath
addExtension x y = fromIFP (I.addExtension (toIFP x) y)

addExtensions :: FilePath -> [T.Text] -> FilePath
addExtensions x ys = fromIFP (I.addExtensions (toIFP x) ys)

(<.>) :: FilePath -> T.Text -> FilePath
(<.>) = addExtension

dropExtension :: FilePath -> FilePath
dropExtension = fromIFP . I.dropExtension . toIFP

dropExtensions :: FilePath -> FilePath
dropExtensions = fromIFP . I.dropExtensions . toIFP

replaceExtension :: FilePath -> T.Text -> FilePath
replaceExtension x y = fromIFP (I.replaceExtension (toIFP x) y)

replaceExtensions :: FilePath -> [T.Text] -> FilePath
replaceExtensions x ys = fromIFP (I.replaceExtensions (toIFP x) ys)

splitExtension :: FilePath -> (FilePath, Maybe T.Text)
splitExtension x =
  let split = I.splitExtension (toIFP x)
  in (fromIFP (fst split),snd split)

splitExtensions :: FilePath -> (FilePath, [T.Text])
splitExtensions x =
  let split = I.splitExtensions (toIFP x)
  in (fromIFP (fst split),snd split)

--
-- Internal
--

toIFP :: FilePath -> I.FilePath
toIFP = I.fromText . T.pack . BC.unpack . unFilePath

fromIFP :: I.FilePath -> FilePath
fromIFP = FilePath . BC.pack . T.unpack . either id id . I.toText
