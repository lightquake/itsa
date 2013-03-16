{-# LANGUAGE OverloadedStrings #-}

-- | Individual handlers. We use the renderers defined in Renderer and
-- our own logic for picking which posts to render.

module Handler where

import Control.Lens
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.State      (gets)
import Data.IORef               (readIORef)
import Data.Monoid              ((<>))
import Data.Table
import Data.Text                (Text)
import Prelude                  hiding (FilePath)
import Snap.Core                (MonadSnap, writeLBS)
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
    let posts = postTable^..group __posted.rows & renderPosts . take 2 . reverse
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
