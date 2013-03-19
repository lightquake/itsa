{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell #-}
------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens           (makeLenses)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import Data.Table
import Post.Types
import Snap.Snaplet

import Config

------------------------------------------------------------------------------
data App = App
    { __postTable :: IORef (Table Post),
      __config :: Config
    }

getPostTable :: (MonadState App m, MonadIO m) => m (Table Post)
getPostTable = gets __postTable >>= liftIO . readIORef

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App
