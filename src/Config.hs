{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Describes the type for the configuration data: posts per page,
-- title, etc.
module Config where

import Control.Applicative ((<$>))
import Control.Lens
import Data.Yaml as Yaml

data Config = Config {
    __postsPerPage :: Int -- ^ Number of posts to display on a page.
}

makeLenses ''Config

instance FromJSON Config where
    parseJSON (Object o) = Config <$> o .: "posts-per-page"
    parseJSON _ = fail "Expected an object"
