{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | This module creates functions that take in data and other
-- rendered templates and give another template in return. Note that
-- these templates don't have the routing renderer applied to them for
-- composability reasons/separation of concerns.
module Renderer where

import Control.Lens   (view)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Text.Hamlet    (HtmlUrl, hamlet)

import Post.Types
import RelativeHamlet

-- | The datatype representing a route.
data ItsaR = RootR -- ^ The docroot.
           | TagR T.Text -- ^ Posts related to a tag.

-- | 'Top-level' renderer that puts its arguments in the default layout.
renderDefault :: HtmlUrl ItsaR -- ^ The HTML to show in the left column.
                 -> HtmlUrl ItsaR -- ^ The HTML to show in the right column.
                 -> HtmlUrl ItsaR
renderDefault leftColumn rightColumn
    = $(hamletRelativeFile "templates/default-layout.hamlet")

-- | Render a series of posts.
renderPosts :: [Post] -> HtmlUrl ItsaR
renderPosts posts = [hamlet|$forall post <- posts
                                            <article>^{renderPost post}|]

-- | Render an individual post.
renderPost :: Post -> HtmlUrl ItsaR
renderPost post = $(hamletRelativeFile "templates/post.hamlet")

-- | Render the tag list, given a list of (tag, frequency) tuples.
renderTagList :: [(T.Text, Int)] -> HtmlUrl ItsaR
renderTagList unsorted = $(hamletRelativeFile "templates/tag-list.hamlet")
  where tagList = reverse $ sortBy (comparing snd) unsorted
