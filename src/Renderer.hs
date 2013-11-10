{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

-- | This module creates functions that take in data and other
-- rendered templates and give another template in return. Note that
-- these templates don't have the routing renderer applied to them for
-- composability reasons/separation of concerns.
module Renderer (ItsaR(..),
                 renderRoute,
                 renderDefault,
                 renderPosts,
                 renderPagination,
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
import qualified Data.Text                as Text
import           Data.Text.Lazy           (toStrict)
import           Data.Time                (TimeZone, formatTime, utcToZonedTime)
import           System.Locale            (defaultTimeLocale)
import qualified Text.Blaze.Renderer.Text as Blaze
import           Text.Hamlet              (HtmlUrl, hamlet)
import qualified Text.XML                 as XML

import           Application
import           Config
import           RelativeHamlet
import           Types

-- | The datatype representing a route.
data ItsaR = RootR -- ^ The docroot.
           | PageR Int -- ^ An archive page.
           | TagR Text Int -- ^ Posts related to a tag.
           | PostR Text -- ^ An individual post.
           | StaticPageR Text -- ^ An individual page.
           | RssR -- ^ The RSS feed.
           | QueueR Int -- ^ Queued posts.
           | DraftsR Int -- ^ Draft posts.


-- | The route renderer. Make sure this synchronizes with the route
-- parser in Site.hs!
renderRoute :: Text -- ^ The approot.
               -> ItsaR -- ^ The route to render.
               -> [(Text, Text)] -- ^ A list of query strings
                                 -- parameters(?). TODO: Figure out
                                 -- what these are and use them.
                                 -- Actually use these.
               -> Text
renderRoute appRoot route query =
    Text.replace " " "%20" $ appRoot <> renderRoute' route query
  where
    renderRoute' RootR _ = ""
    renderRoute' (PageR page) _ = pager page
    renderRoute' (TagR tag page) _ = "/tagged/" <> tag <> pager page
    renderRoute' (PostR s) _ = "/post/" <> s
    renderRoute' (StaticPageR s) _ = "/" <> s
    renderRoute' RssR _ = "/feed/rss"
    renderRoute' (QueueR page) _ = "/queue" <> pager page
    renderRoute' (DraftsR page) _ = "/drafts" <> pager page
    pager 1 = ""
    pager n = "/page/" <> showT n
    showT = Text.pack . show


-- | Render a page using the default container; i.e., for the Negative theme,
-- the left sidebar with the title/pages.
renderDefault :: HtmlUrl ItsaR -> AppHandler (HtmlUrl ItsaR)
renderDefault pageBody = do
    postTable <- getPostTable
    staticPageTable <- getStaticPageTable
    blogTitle <- view $ _config._blogTitle
    subtitle <- view _subtitle
    mAnalytics <- view $ _config._analytics
    let pageTitle = maybe blogTitle (<> " | " <> blogTitle) subtitle
        staticPages = staticPageTable^..group StaticPageSlug .rows
        tagList = postTable^@..group Tags .to count & renderTagList
    return $(hamletRelativeFile "templates/layout.hamlet")


-- | Render a series of posts.
renderPosts :: TimeZone -- ^ The time zone to use for displaying timestamps.
               -> [Post] -- ^ The posts to render, from first to last.
               -> HtmlUrl ItsaR
renderPosts tz posts = [hamlet|$forall post <- posts
                                               ^{renderPost tz post}|]

-- | Render the pagination footer; since the page links can vary
-- depending on the route, we take the router as an argument.
renderPagination :: (Int -> ItsaR) -- ^ The router for displaying a given page.
                    -> Int -- ^ The current page.
                    -> Bool -- ^ Whether there's a next page.
                    -> HtmlUrl ItsaR
renderPagination pageRouter pageNumber hasNext =
    $(hamletRelativeFile "templates/pagination.hamlet")

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
    appRoot <- view $ _config._appRoot
    let render route = renderRoute appRoot route []
    let xmlElement = XML.Element "rss" rootAttributes
                     $(xmlRelativeFile "templates/rss.xhamlet")
    return $ XML.Document prologue xmlElement []
    where prologue = XML.Prologue [] Nothing []
          rootAttributes =
              Map.fromList [ ("version", "2.0"),
                             ("xmlns:atom", "http://www.w3.org/2005/Atom")
                           ]

-- | Render a 404 page.
render404 :: HtmlUrl ItsaR
render404 = $(hamletRelativeFile "templates/404.hamlet")



-- | Get the route referring to a post.
routePost :: Post -> ItsaR
routePost post = PostR $ view slug post

-- | Get the route referring to a page.
routePage :: StaticPage -> ItsaR
routePage page = StaticPageR $ view slug page
