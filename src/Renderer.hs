{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

-- | This module creates functions that take in data and other
-- rendered templates and give another template in return. Note that
-- these templates don't have the routing renderer applied to them for
-- composability reasons/separation of concerns.
module Renderer (ItsaR(..), renderRoute,
                 renderTwoColumn,
                 renderDefault,
                 renderPosts,
                 renderPost,
                 renderStaticPage,
                 renderTagList,
                 renderRss,
                 render404) where

import           Control.Lens
import           Data.List                (sortBy)
import qualified Data.Map                 as Map
import           Data.Monoid
import           Data.Ord                 (comparing)
import           Data.Table               (count, group, rows)
import           Data.Text                (Text)
import           Data.Text.Lazy           (toStrict)
import           Data.Time                (TimeZone, formatTime, utcToZonedTime)
import           System.Locale            (defaultTimeLocale)
import qualified Text.Blaze.Renderer.Text as Blaze
import           Text.Hamlet              (HtmlUrl, hamlet)
import qualified Text.XML                 as XML

import           Application
import           Config
import           Post.Types
import           RelativeHamlet

-- | The datatype representing a route.
data ItsaR = RootR -- ^ The docroot.
           | TagR Text -- ^ Posts related to a tag.
           | PostR Text -- ^ An individual post.
           | StaticPageR Text -- ^ An individual page.
           | RssR -- ^ The RSS feed.

-- | The route renderer. Make sure this synchronizes with the route
-- parser in Site.hs!
renderRoute :: ItsaR -- ^ The route to render.
               -> [(Text, Text)] -- ^ A list of query strings
                                 -- parameters(?). TODO: Figure out
                                 -- what these are and use them.
                                 -- Actually use these.
               -> Text
renderRoute RootR _ = "/"
renderRoute (TagR tag) _ = "/tagged/" <> tag
renderRoute (PostR slug) _ = "/post/" <> slug
renderRoute (StaticPageR slug) _ = "/page/" <> slug
renderRoute RssR _ = "/feed/rss"


-- | 'Top-level' renderer that puts its arguments in the default layout.
renderTwoColumn :: HtmlUrl ItsaR -- ^ The HTML to show in the left column.
                 -> HtmlUrl ItsaR -- ^ The HTML to show in the right column.
                 -> AppHandler (HtmlUrl ItsaR)
renderTwoColumn leftColumn rightColumn
    = return $ $(hamletRelativeFile "templates/two-column.hamlet")

-- | Render a page using the default right column; i.e., the tag list.
renderDefault :: HtmlUrl ItsaR -> AppHandler (HtmlUrl ItsaR)
renderDefault tpl = do
    postTable <- getPostTable
    staticPageTable <- getStaticPageTable
    blogTitle <- view $ _config._blogTitle
    subtitle <- view _subtitle
    let pageTitle = maybe blogTitle (<> " | " <> blogTitle) subtitle
        staticPages = staticPageTable^..group StaticPageSlug .rows
    body <- renderTwoColumn tpl
        (postTable^@..group Tags .to count & renderTagList)
    return $(hamletRelativeFile "templates/default-layout.hamlet")


-- | Render a series of posts.
renderPosts :: TimeZone -> [Post] -> HtmlUrl ItsaR
renderPosts tz posts = [hamlet|$forall post <- posts
                                            ^{renderPost tz post}|]

-- | Render an individual post.
renderPost :: TimeZone -> Post -> HtmlUrl ItsaR
renderPost tz post = $(hamletRelativeFile "templates/post.hamlet")

-- | Render a page; i.e., a bit of static text that's not a post.
renderStaticPage :: StaticPage -> HtmlUrl ItsaR
renderStaticPage page = $(hamletRelativeFile "templates/page.hamlet")

-- | Render the tag list, given a list of (tag, frequency) tuples.
renderTagList :: [(Text, Int)] -> HtmlUrl ItsaR
renderTagList unsorted = $(hamletRelativeFile "templates/tag-list.hamlet")
  where tagList = reverse $ sortBy (comparing snd) unsorted

-- | Render an RSS feed for a list of posts. Note that unlike the
-- others, this is an 'XML.Document', not an 'HtmlUrl' 'ItsaR' (or
-- monadic wrapper around one, or function returning one, etc.).
renderRss :: [Post] -> AppHandler XML.Document
renderRss posts = do
    blogTitle <- view $ _config._blogTitle
    let xmlElement = XML.Element "rss" (Map.fromList [ ("version", "2.0") ])
                     $(xmlRelativeFile "templates/rss.xhamlet")
    return $ XML.Document prologue xmlElement []
    where prologue = XML.Prologue [] Nothing []
          render route = renderRoute route []

-- | Render a 404 page.
render404 :: HtmlUrl ItsaR
render404 = $(hamletRelativeFile "templates/404.hamlet")



-- | Get the route referring to a post.
routePost :: Post -> ItsaR
routePost post = PostR $ view _slug post

-- | Get the route referring to a page.
routePage :: StaticPage -> ItsaR
routePage page = StaticPageR $ view _slug page
