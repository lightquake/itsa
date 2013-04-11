{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Describes the type for the configuration data: posts per page,
-- title, etc.
module Config where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text (Text, stripSuffix)
import Data.Time (TimeZone)
import Data.Yaml as Yaml

data Config = Config {
    __postsPerPage :: Int, -- ^ Number of posts to display on a page.
    __blogTitle :: Text, -- ^ Title of the blog.
    __timeZone :: TimeZone, -- ^ Time zone to display times in.
    __appRoot :: Text, -- ^ The base for all URLs used throughout,
                      -- without the trailing slash.
    __analytics :: Maybe Text -- ^ A Google Analytics ID.
}

makeLenses ''Config

instance FromJSON Config where
    parseJSON (Object o) = Config <$> o .: "posts-per-page"
                           <*> o .: "blog-title"
                           <*> fmap read (o .: "time-zone")
                           <*> fmap stripSlash (o .: "app-root")
                           <*> o .:? "analytics"
      where
        stripSlash url = fromMaybe url (stripSuffix "/" url)
    parseJSON _ = fail "Expected an object"
