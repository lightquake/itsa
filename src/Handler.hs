{-# LANGUAGE OverloadedStrings #-}

-- | Individual handlers. We use the renderers defined in Renderer and
-- our own logic for picking which posts to render.

module Handler (mainPage, postPage, tagPage) where

import Control.Applicative      ((<$>), (<*>))
import Control.Lens
import Data.ByteString          (ByteString)
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Data.Table
import Data.Text                (Text, pack, unpack)
import Data.Text.Encoding       (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Calendar       (fromGregorian)
import Prelude                  hiding (FilePath)
import Snap.Core                (MonadSnap, getParam, writeLBS)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Hamlet              (HtmlUrl)

import Application
import Config
import Post.Types
import Renderer

-- | This handler renders the main page; i.e., the most recent posts.
mainPage :: AppHandler ()
mainPage = do
    postTable <- getPostTable
    postsPerPage <- view $ _config._postsPerPage
    let posts = postTable^..rows' & renderPosts . take postsPerPage . reverse
    renderDefault posts >>= serveTemplate

-- | Show posts with a given tag.
tagPage :: AppHandler ()
tagPage = do
    postsPerPage <- view $ _config._postsPerPage
    mTagName <- getParamAsText "tagName"
    case mTagName of
        Nothing -> error "???? failure to get tag name from tag page"
        Just tagName -> do
            postTable <- getPostTable
            let posts = postTable^..withAny Tags [tagName].rows &
                        renderPosts . take postsPerPage . reverse
            renderDefault posts >>= serveTemplate

-- | Show the post with a given slug posted on a given year/month/day.
-- As an amusing side-effect of read being permissive, a URL with
-- /0x7DC/9/0o25/foo will also work. If you rely on that, you're weird.
postPage :: AppHandler ()
postPage = do
    -- Can't turn this into a mapM because year is an Integer, but
    -- month and day are Ints.
    mYear <- readParam "year"
    mMonth <- readParam "month"
    mDay <- readParam "day"
    mSlug <- getParamAsText "slug"
    serveTemplate =<< renderDefault =<< fromMaybe (return render404)
        (postPage' <$> mYear <*> mMonth <*> mDay <*> mSlug)
    where postPage' year month day slug = do
              postTable <- getPostTable
              let key = (fromGregorian year month day, slug)
              return $ postTable^.at key & maybe render404 renderPost

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
