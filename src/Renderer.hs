{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- | This module creates functions that take in data and other
-- rendered templates and give another template in return. Note that
-- these templates don't have the routing renderer applied to them for
-- composability reasons/separation of concerns.
module Renderer (ItsaR(..),
                 renderTwoColumn,
                 renderDefault,
                 renderPosts,
                 renderPost,
                 renderTagList,
                 render404) where

import Control.Lens
import Data.List      (sortBy)
import Data.Ord       (comparing)
import Data.Table     (count, group)
import Data.Text      (Text)
import Data.Time      (toGregorian, utctDay)
import Text.Hamlet    (HtmlUrl, hamlet)

import Application
import Post.Types
import RelativeHamlet

-- | The datatype representing a route.
data ItsaR = RootR -- ^ The docroot.
           | TagR Text -- ^ Posts related to a tag.
           | PostR Integer Int Int Text -- ^ An individual post.

-- | 'Top-level' renderer that puts its arguments in the default layout.
renderTwoColumn :: HtmlUrl ItsaR -- ^ The HTML to show in the left column.
                 -> HtmlUrl ItsaR -- ^ The HTML to show in the right column.
                 -> HtmlUrl ItsaR
renderTwoColumn leftColumn rightColumn
    = $(hamletRelativeFile "templates/default-layout.hamlet")

-- | Render a page using the default right column; i.e., the tag list.
renderDefault :: HtmlUrl ItsaR -> AppHandler (HtmlUrl ItsaR)
renderDefault tpl = do
    postTable <- getRef _postTable
    return $ renderTwoColumn tpl
        (postTable^@..group Tags .to count & renderTagList)


-- | Render a series of posts.
renderPosts :: [Post] -> HtmlUrl ItsaR
renderPosts posts = [hamlet|$forall post <- posts
                                            <article>^{renderPost post}|]

-- | Render an individual post.
renderPost :: Post -> HtmlUrl ItsaR
renderPost post = $(hamletRelativeFile "templates/post.hamlet")

-- | Render the tag list, given a list of (tag, frequency) tuples.
renderTagList :: [(Text, Int)] -> HtmlUrl ItsaR
renderTagList unsorted = $(hamletRelativeFile "templates/tag-list.hamlet")
  where tagList = reverse $ sortBy (comparing snd) unsorted

-- | Render a 404 page.
render404 :: HtmlUrl ItsaR
render404 = $(hamletRelativeFile "templates/404.hamlet")

-- | Get the route referring to a post.
postRouter :: Post -> ItsaR
postRouter post = PostR year month day $ view _slug post
  where (year, month, day) = toGregorian . utctDay . view _posted $ post
