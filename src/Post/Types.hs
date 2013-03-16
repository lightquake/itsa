{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Here we define all the datatypes relevant to a post, which mostly
-- consist of the 'Post' type itself and a 'Tabular' instance.
module Post.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Default
import qualified Data.Set            as S
import           Data.Table
import           Data.Text           (Text)
import           Data.Time
import           Text.Blaze.Html     (Html)
import           Text.Pandoc

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
        Tags :: Key Inverted Post (S.Set Text)

    data Tab Post i = PostTab (i Primary (Day, Text)) (i Inverted (S.Set Text))

    fetch PostId post = (post^._posted.to utctDay, post^._slug)
    fetch Tags post = post^._tags & S.fromList

    primary = PostId
    primarily PostId r = r

    mkTab f = PostTab <$> f PostId <*> f Tags
    forTab (PostTab i t) f = PostTab <$> f PostId i <*> f Tags t
    ixTab (PostTab i _) PostId = i
    ixTab (PostTab _ t) Tags = t

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
