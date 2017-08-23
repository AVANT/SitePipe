{-# language OverloadedStrings #-}
module SitePipe.Utilities
  ( addPrefix
  , setExt
  , getMeta
  ) where

import System.FilePath.Posix
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Lens hiding ((.=))

-- | Set the extension of a filepath or url to the given extension.
-- Use @setExt ""@ to remove any extension.
setExt :: String -> FilePath -> FilePath
setExt = flip replaceExtension

-- | Add a prefix to a filepath or url
addPrefix :: String -> FilePath -> FilePath
addPrefix = (++)

-- | Given a function which creates a url from a tag name and a list of posts
-- (which have a tags property which is a list of strings)
-- this returns a list of tags which contain:
--
-- * name: The tag name
-- * url: The tag's url
-- * posts: The list of posts matching that tag
getMeta :: (String -> String) -- ^ Accept a tagname and create a url
           -> [Value] -- ^ List of posts
           -> [Value]
getMeta makeUrl postList = uncurry (makeMeta makeUrl) <$> M.toList metaMap
  where
    metaMap = M.unionsWith mappend (toMap <$> postList)
    toMap post = M.fromList (zip (post ^.. key "meta" . values . _String . to T.unpack) $ repeat [post])

-- | Makes a single tag
makeMeta :: (String -> String) -> String -> [Value] -> Value
makeMeta makeUrl metaname posts = object
  [ "meta" .= metaname
  , "url" .= makeUrl metaname
  , "posts" .= posts
  ]