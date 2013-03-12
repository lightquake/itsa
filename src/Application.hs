------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Data.IORef
import Data.Table
import Post.Types
import Snap.Snaplet

------------------------------------------------------------------------------
data App = App
    { _postTable :: IORef (Table Post)
    }


------------------------------------------------------------------------------
type AppHandler = Handler App App
