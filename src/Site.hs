{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Data.IORef
import Data.Table
import Snap.Snaplet
import Snap.Util.FileServe

import Application
import Template

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/static", serveDirectory "static"),
           ("/", renderTemplate mainPage)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    tableRef <- liftIO . newIORef $ fromList []
    addRoutes routes
    return $ App tableRef
