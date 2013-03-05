{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Post where

import Control.Applicative
import Control.Lens
import Data.Table
import Data.Text
import Data.Time

data Post = Post { __title :: Text, __tags :: [Text], __slug :: Text,
                   __posted :: UTCTime } deriving (Eq, Ord, Show)

makeLenses ''Post

instance Tabular Post where
    type PKT Post = (Day, Text)
    data Key k Post b where
        PostId :: Key Primary Post (Day, Text)

    data Tab Post i = PostTab (i Primary (Day, Text))

    fetch PostId post = (post^._posted.to utctDay, post^._slug)

    primary = PostId
    primarily PostId r = r

    mkTab f = PostTab <$> f PostId
    forTab (PostTab x) f = PostTab <$> f PostId x
    ixTab (PostTab x) PostId = x
