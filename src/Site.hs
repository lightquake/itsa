{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Data.IORef
import           Snap.Snaplet
import           Snap.Util.FileServe

import           Application
import qualified Handler
import           Post

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/static", serveDirectory "static"),
           ("/", Handler.mainPage)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    tableRef <- liftIO . newIORef $ sampleTable
    addRoutes routes
    return $ App tableRef
