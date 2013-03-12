{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Renderer where

import Control.Lens   (view)
import Text.Hamlet    (HtmlUrl, hamlet)

import Post
import RelativeHamlet

-- The datatype representing a route.
data ItsaR = RootR


renderDefault :: HtmlUrl ItsaR -> HtmlUrl ItsaR
renderDefault leftColumn =
    $(hamletRelativeFile "templates/default-layout.hamlet")

renderPosts :: [Post] -> HtmlUrl ItsaR
renderPosts posts = [hamlet|$forall post <- posts
                                            <article>^{renderPost post}|]

renderPost :: Post -> HtmlUrl ItsaR
renderPost post = $(hamletRelativeFile "templates/post.hamlet")
