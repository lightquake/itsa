{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Data.IORef
import           Data.Table             (fromList)
import           Data.Yaml              (decodeEither)
import qualified Filesystem             as FS
import qualified Filesystem.Path        as FS
import           Snap.Core              (ifTop, modifyResponse, setContentType)
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.FSNotify        (startManager, watchTree)

import           Application
import qualified Handler
import           Loader

------------------------------------------------------------------------------
-- | The application's routes. Note that if this is updated, then so do
-- 'Loader.loadStaticPages', to update the static page slug blacklist, and
-- 'Renderer.renderRoute', to update the route rendering.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/static", serveDirectory "static"),
           ("/tagged/:tagName/page/:page", Handler.tagPage),
           ("/tagged/:tagName", Handler.tagPage),
           ("/post/:slug", Handler.postPage),
           ("/page/:page", Handler.mainPage),
           ("/drafts", Handler.draftsPage),
           ("/drafts/:page", Handler.draftsPage),
           ("/queue", Handler.queuePage),
           ("/queue/:page", Handler.queuePage),
           ("/feed/rss", Handler.rss),
           ("/:pageName", ifTop Handler.staticPage),
           ("/", Handler.mainPage)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "itsa" "A simple blog engine." Nothing $ do
    postTableRef <- liftIO $ buildWatcher (fmap fromList . loadPosts) "posts/"
    staticPageTableRef <- liftIO $ buildWatcher
                          (fmap fromList . loadStaticPages) "pages/"
    config <- liftIO (either error id . decodeEither
                      <$> FS.readFile "config.yml")
    let routeWrapper (route, handler) =
            (route, modifyResponse (setContentType "text/html") >> handler)
    addRoutes $ map routeWrapper routes
    config `seq` return $ App postTableRef staticPageTableRef config Nothing

-- | Given a function that loads data from a path and a path, set up a
-- watcher to continually load data into an 'IORef'.
buildWatcher :: (FS.FilePath -> IO a) -> FS.FilePath -> IO (IORef a)
buildWatcher loader path = do
    manager <- startManager
    ref <- newIORef =<< loader path
    watchTree manager path (const True)
        (const $ atomicWriteIORef ref =<< loader path)
    return ref
