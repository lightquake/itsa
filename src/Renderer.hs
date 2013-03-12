{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | This module creates functions that take in data and other
-- rendered templates and give another template in return. Note that
-- these templates don't have the routing renderer applied to them for
-- composability reasons/separation of concerns.
module Renderer where

import Control.Lens   (view)
import Text.Hamlet    (HtmlUrl, hamlet)

import Post
import RelativeHamlet

-- | The datatype representing a route.
data ItsaR = RootR -- ^ The docroot.

-- | 'Top-level' renderer that puts its arguments in the default layout.
renderDefault :: HtmlUrl ItsaR -- ^ The HTML to show in the left column.
                 -> HtmlUrl ItsaR
renderDefault leftColumn
    = $(hamletRelativeFile "templates/default-layout.hamlet")

-- | Render a series of posts.
renderPosts :: [Post] -> HtmlUrl ItsaR
renderPosts posts = [hamlet|$forall post <- posts
                                            <article>^{renderPost post}|]

-- | Render an individual post.
renderPost :: Post -> HtmlUrl ItsaR
renderPost post = $(hamletRelativeFile "templates/post.hamlet")
