{-# LANGUAGE OverloadedStrings #-}

-- | Individual handlers. We use the renderers defined in Renderer and
-- our own logic for picking which posts to render.

module Handler (mainPage, tagPage) where

import Control.Applicative      ((<$>))
import Control.Lens
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.State      (gets)
import Data.ByteString          (ByteString)
import Data.IORef               (readIORef)
import Data.Monoid              ((<>))
import Data.Table
import Data.Text                (Text)
import Data.Text.Encoding       (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Prelude                  hiding (FilePath)
import Snap.Core                (MonadSnap, getParam, writeLBS)
import Snap.Snaplet
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Hamlet              (HtmlUrl)

import Application
import Post.Types
import Renderer

-- | This handler renders the main page; i.e., the most recent posts.
mainPage :: Handler App App ()
mainPage = do
    postTable <- gets _postTable >>= liftIO . readIORef
    let posts = postTable^..rows' & renderPosts . take 2 . reverse
    let renderedTags = postTable^@..group Tags. to count & renderTagList
    serveTemplate $ renderDefault posts renderedTags

-- | Show posts with a given tag.
tagPage :: Handler App App ()
tagPage = do
    mTagName <- getParamAsText "tagName"
    case mTagName of
        Nothing -> error "???? failure to get tag name from tag page"
        Just tagName -> do
            postTable <- gets _postTable >>= liftIO . readIORef
            let posts = postTable^..withAny Tags [tagName].rows & renderPosts . take 2 . reverse
            let renderedTags = postTable^@..group Tags. to count & renderTagList
            serveTemplate $ renderDefault posts renderedTags

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

getParamAsText :: (MonadSnap m) => ByteString -> m (Maybe Text)
getParamAsText param = fmap (decodeUtf8With lenientDecode) <$> getParam param
