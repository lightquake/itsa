{-# LANGUAGE OverloadedStrings, QuasiQuotes, Rank2Types #-}

-- | Individual handlers. We use the renderers defined in Renderer and
-- our own logic for picking which posts to render.

module Handler (draftsPage, mainPage, postPage, queuePage, tagPage) where

import Control.Applicative      ((<$>), (<*>))
import Control.Lens
import Control.Monad.IO.Class   (liftIO)
import Data.ByteString          (ByteString)
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Data.Table
import Data.Text                (Text, pack, unpack)
import Data.Text.Encoding       (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time                (fromGregorian, getCurrentTime)
import Prelude                  hiding (FilePath)
import Snap.Core
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Hamlet              (HtmlUrl, hamlet)

import Application
import Config
import Post.Types
import Renderer

-- | This handler renders the main page; i.e., the most recent posts.
mainPage :: AppHandler ()
mainPage = showPaginatedPosts id

-- | Show posts with a given tag.
tagPage :: AppHandler ()
tagPage = do
    mTagName <- getParamAsText "tagName"
    assign _subtitle mTagName
    case mTagName of
        Nothing -> error "???? failure to get tag name from tag page"
        Just tagName -> showPaginatedPosts $ withAny Tags [tagName]

-- | Show all draft posts.
draftsPage :: AppHandler ()
draftsPage = localhostOnly $ showAllPaginatedPosts (with __isDraft (==) True)

-- | Show all queued, non-draft posts.
queuePage :: AppHandler ()
queuePage = localhostOnly $ do
    now <- liftIO getCurrentTime
    showAllPaginatedPosts $ with __posted (>) now.with __isDraft (==) False

-- | Show the post with a given slug posted on a given year/month/day.
-- As an amusing side-effect of read being permissive, a URL with
-- /0x7DC/9/0o25/foo will also work. If you rely on that, you're weird.
postPage :: AppHandler ()
postPage = do
    postSlug <- fromMaybe (error "post route doesn't have :slug parameter?")
             <$> getParamAsText "slug"
    serveTemplate =<< renderDefault =<< postPage' postSlug
      where
        -- Given a slug, either render that post or 404.
        postPage' :: Text -> AppHandler (HtmlUrl ItsaR)
        postPage' slug = do
            postTable <- getPostTable
            case postTable^.at slug of
                Just post -> do
                    assign _subtitle $ Just $ view _title post
                    return $ renderPost post
                Nothing -> return render404

-- | Similar to 'showAllPaginatedPosts', but excludes drafts and queued posts.
showPaginatedPosts :: Lens' (Table Post) (Table Post) -> AppHandler ()
showPaginatedPosts postFilter = do
    now <- liftIO getCurrentTime
    showAllPaginatedPosts $ postFilter.with __isDraft (==) False
        .with __posted (<=) now

-- | Show the given page of posts filtered using the given lens. This
-- uses the :page parameter name, but defaults to page 1.
showAllPaginatedPosts :: Lens' (Table Post) (Table Post) -> AppHandler ()
showAllPaginatedPosts postFilter = do
    pageNumber <- fromMaybe 1 <$> readParam "page"
    postsPerPage <- view $ _config._postsPerPage
    postTable <- getPostTable
    let posts = postTable^..postFilter.group __posted.rows'
                & take postsPerPage . drop ((pageNumber - 1) * postsPerPage)
                . reverse
    renderDefault (renderPosts posts) >>= serveTemplate

-- | Ensure that the requesting IP is 127.0.0.1, or else 403.
localhostOnly :: AppHandler () -> AppHandler ()
localhostOnly action = do
    reqIp <- rqRemoteAddr <$> getRequest
    if reqIp == "127.0.0.1" then action else do
        modifyResponse $ setResponseCode 403
        serveTemplate =<< renderDefault [hamlet|<h1>403|]
        finishWith =<< getResponse

-- | Serve a template using Snap by supplying the route renderer to
-- it, rendering it, and writing as a lazy
-- 'Data.ByteString.Lazy.ByteString'.
serveTemplate :: (MonadSnap m) => HtmlUrl ItsaR -> m ()
serveTemplate tpl = writeLBS . renderMarkup $ tpl renderRoute
 where
    -- The route renderer. Make sure this synchronizes with the route
    -- parser in Site.hs!
    renderRoute :: ItsaR -> [(Text, Text)] -> Text
    renderRoute RootR _ = "/"
    renderRoute (TagR tag) _ = "/tagged/" <> tag
    renderRoute (PostR year month day slug) _ = "/post/" <> showT year
                                                <> "/" <> showT month
                                                <> "/" <> showT day
                                                <> "/" <> slug
      where showT :: (Show a) => a -> Text
            showT = pack . show

getParamAsText :: (MonadSnap m) => ByteString -> m (Maybe Text)
getParamAsText param = fmap (decodeUtf8With lenientDecode) <$> getParam param

readParam :: (MonadSnap m, Read a) => ByteString -> m (Maybe a)
readParam param = fmap (read . unpack) <$> getParamAsText param
