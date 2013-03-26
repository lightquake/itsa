{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString          (ByteString)
import           Data.IORef
import           Data.Yaml                (decodeEither)
import           Filesystem               as FS
import           Snap.Snaplet
import           Snap.Util.FileServe

import           Application
import qualified Handler
import           Post.Loader

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/static", serveDirectory "static"),
           ("/tagged/:tagName/page/:1", Handler.tagPage),
           ("/tagged/:tagName", Handler.tagPage),
           ("/post/:year/:month/:day/:slug", Handler.postPage),
           ("/page/:page", Handler.mainPage),
           ("/drafts", Handler.draftsPage),
           ("/drafts/:page", Handler.draftsPage),
           ("/", Handler.mainPage)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    tableRef <- liftIO (loadPosts "posts/" >>= newIORef)
    config <- liftIO (either error id . decodeEither
                      <$> FS.readFile "config.yaml")
    addRoutes routes
    return $ App tableRef config Nothing
