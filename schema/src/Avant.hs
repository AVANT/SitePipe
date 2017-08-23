{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import qualified Text.Mustache as MT
import qualified Text.Mustache.Types as MT
import qualified Data.Text as T

main :: IO ()
main = siteWithGlobals templateFuncs $ do
  -- Load all the posts from site/posts/
  posts <- resourceLoader markdownReader ["posts/*.md"]
  -- getTags will return a list of all tags from the posts,
  -- each tag has a 'tag' and a 'posts' property
  let meta = getMeta makeMetaUrl posts
      -- Create an object with the needed context for a table of contents
      indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "meta" .= meta
                            , "url" .= ("/index.html" :: String)
                            ]
      rssContext :: Value
      rssContext = object [ "posts" .= posts
                          , "domain" .= ("http://avant.org" :: String)
                          , "url" .= ("/rss.xml" :: String)
                          ]

  -- render pages
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
  writeTemplate "templates/meta-list.html" meta
  writeTemplate "templates/meta.html" meta
  writeTemplate "templates/rss.xml" [rssContext]
  staticAssets

-- We can provide a list of functions to be availabe in our mustache templates
templateFuncs :: MT.Value
templateFuncs = MT.object
  [ "metaUrl" MT.~> MT.overText (T.pack . makeMetaUrl . T.unpack)
  ]

makeMetaUrl :: String -> String
makeMetaUrl metaName = "/meta/" ++ metaName ++ ".html"

-- | All the static assets can just be copied over from our site's source
staticAssets :: SiteM ()
staticAssets = copyFiles
    -- We can copy a glob
    [ "css/*.css"
    -- Or just copy the whole folder!
    , "js/"
    , "images/"
    ]