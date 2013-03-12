{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Post where

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Table
import Data.Text (Text)
import Data.Time
import Text.Blaze.Html (Html)
import Text.Pandoc

data Post = Post { __title :: Text, __slug :: Text,
                   __body :: Html,  __tags :: [Text], __posted :: UTCTime }

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

sampleTable :: Table Post
sampleTable = fromList
              [
                  Post { __title = "Oldest Post", __slug = "post-oldest",
                         __body = writeHtml def $ readMarkdown def "From when the world was new.",
                         __tags = ["old", "so old"],
                         __posted = UTCTime (fromGregorian 1990 1 1)
                                    (secondsToDiffTime 800)},
                  Post { __title = "Post 1", __slug = "post-1",
                       __body = writeHtml def $ readMarkdown def "post *body*",
                       __tags = ["tag1", "tag2"],
                       __posted = UTCTime (fromGregorian 2013 3 5)
                                  (secondsToDiffTime 800)},
                  Post { __title = "Post Two", __slug = "post-two",
                         __body = writeHtml def $ readMarkdown def "This is also a post.",
                         __tags = ["tag2", "tag3"],
                         __posted = UTCTime (fromGregorian 2013 3 2)
                                    (secondsToDiffTime 800)}
              ]
