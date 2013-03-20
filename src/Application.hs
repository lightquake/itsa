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
import Data.Text              (Text)
import Snap.Snaplet

import Config
import Post.Types
------------------------------------------------------------------------------
data App = App
    { __postTable :: IORef (Table Post), -- ^ The table of posts;
                                         -- stored in an IORef so that
                                         -- the reloader can reload it
                                         -- automatically.
      __config    :: Config, -- ^ Configuration data.
      __subtitle  :: Maybe Text -- ^ The 'subtitle' of the current
                                -- page, which is either set by a
                                -- handler or renderer and used by the
                                -- renderer.
    }

-- | Get the 'Table' 'Post' out of anything that has an 'App' as state.
getPostTable :: (MonadState App m, MonadIO m) => m (Table Post)
getPostTable = gets __postTable >>= liftIO . readIORef

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App
