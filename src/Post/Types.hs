{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Here we define all the datatypes relevant to a post, which mostly
-- consist of the 'Post' type itself and a 'Tabular' instance.
module Post.Types where

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Table
import Data.Text           (Text)
import Data.Time
import Text.Blaze.Html     (Html)
import Text.Pandoc

-- | All the information corresponding to a post. Note that the tuple
-- of slug and post date should be unique, or otherwise bad things
-- will happen. This is not currently enforced, but should be!
data Post = Post { __title  :: Text -- ^ The title of the post.
                 , __slug   :: Text -- ^ The 'slug', which appears in
                                    -- the URL. This should never
                                    -- change!
                 ,  __body  :: Html -- ^ The actual body of the post.
                 ,  __tags  :: [Text] -- ^ The post tags.
                 , __posted :: UTCTime -- ^ The time at which the post
                                       -- is 'posted'.
                 }

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
