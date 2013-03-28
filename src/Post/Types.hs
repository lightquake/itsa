{-# LANGUAGE GADTs, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Here we define all the datatypes relevant to a post, which mostly
-- consist of the 'Post' type itself and a 'Tabular' instance.
module Post.Types where

import           Control.Applicative
import           Control.Lens
import qualified Data.Set            as S
import           Data.Table
import           Data.Text           (Text)
import           Data.Time
import           Text.Blaze.Html     (Html)

-- | All the information corresponding to a post. Note that the tuple
-- of slug and post date should be unique, or otherwise bad things
-- will happen. This is not currently enforced, but should be!
data Post = Post { __title   :: Text -- ^ The title of the post.
                 , __slug    :: Text -- ^ The 'slug', which appears in
                                    -- the URL. This should never
                                    -- change!
                 , __body    :: Html -- ^ The actual body of the post.
                 , __tags    :: [Text] -- ^ The post tags.
                 , __isDraft :: Bool -- ^ Whether the post is a draft.
                 , __posted  :: UTCTime -- ^ The time at which the post
                                       -- is 'posted'.
                 }

makeLenses ''Post

instance Tabular Post where
    type PKT Post = Text
    data Key k Post b where
        Slug :: Key Primary Post Text
        Tags :: Key Inverted Post (S.Set Text)

    data Tab Post i = PostTab (i Primary Text) (i Inverted (S.Set Text))

    fetch Slug post = post^._slug
    fetch Tags post = post^._tags & S.fromList

    primary = Slug
    primarily Slug r = r

    mkTab f = PostTab <$> f Slug <*> f Tags
    forTab (PostTab i t) f = PostTab <$> f Slug i <*> f Tags t
    ixTab (PostTab i _) Slug = i
    ixTab (PostTab _ t) Tags = t
