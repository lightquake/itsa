{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

-- Store the templates and routes. For syntax highlighting purposes,
-- we store all templates in their own file unless they're one-liners
-- or similar.

module Template where

import Control.Lens
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.State      (gets)
import Data.IORef               (readIORef)
import Data.Table
import Data.Text                (Text)
import Prelude                  hiding (FilePath)
import Snap.Core                (MonadSnap, writeLBS)
import Snap.Snaplet
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Hamlet              (HtmlUrl)

import Application
import Post
import Renderer

mainPage :: Handler App App ()
mainPage = do
    postTable <- gets _postTable >>= liftIO . readIORef
    serveTemplate $ renderDefault . renderPosts $
        postTable^..group __posted.rows & take 2 . reverse




serveTemplate :: (MonadSnap m) => HtmlUrl ItsaR -> m ()
serveTemplate tpl = writeLBS . renderMarkup $ tpl renderRoute
 where
    -- The route renderer. Make sure this synchronizes with the route
    -- parser in Site.hs!
    renderRoute :: ItsaR -> [(Text, Text)] -> Text
    renderRoute RootR _ = "/"
