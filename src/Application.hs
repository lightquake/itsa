{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IORef
import Data.Table
import Post.Types
import Snap.Snaplet

------------------------------------------------------------------------------
data App = App
    { _postTable :: IORef (Table Post)
    }

getRef :: (MonadState s m, MonadIO m) => (s -> IORef a) -> m a
getRef ref = gets ref >>= liftIO . readIORef


------------------------------------------------------------------------------
type AppHandler = Handler App App
