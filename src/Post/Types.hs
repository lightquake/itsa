{-# LANGUAGE FlexibleInstances, FunctionalDependencies, GADTs,
             MultiParamTypeClasses, OverloadedStrings, TemplateHaskell,
             TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Here we define all the datatypes relevant to posts and pages,
-- which consists of the 'Post' and 'StaticPage' types themselves and
-- 'Tabular' instances.
module Post.Types where

import           Control.Applicative
import           Control.Lens
import qualified Data.Set            as S
import           Data.Table
import           Data.Text           (Text)
import           Data.Time
import           FieldRule
import           Text.Blaze.Html     (Html)

-- | All the information corresponding to a post. Note that the slug
-- is guaranteed to be unique, since it's the directory name.
data Post = Post { __postTitle   :: Text -- ^ The title of the post.
                 , __postSlug    :: Text -- ^ The 'slug', which appears in
                                    -- the URL. This should never
                                    -- change!
                 , __postBody    :: Html -- ^ The actual body of the post.
                 , __postTags    :: [Text] -- ^ The post tags.
                 , __postIsDraft :: Bool -- ^ Whether the post is a draft.
                 , __postPosted  :: UTCTime -- ^ The time at which the post
                                       -- is 'posted'.
                 }


-- | All of the information corresponding to a page.
data StaticPage = StaticPage { __pageShortTitle :: Text
                               -- ^ The 'short title', which should be
                               -- short enough to fit alongside other
                               -- short titles.
                             , __pageTitle      :: Text -- ^ The full title.
                             , __pageSlug       :: Text -- ^ The slug.
                             , __pageBody       :: Html -- ^ The page body.
                             }


makeFieldsWith underscoredCamelCaseFields ''Post
makeFieldsWith underscoredCamelCaseFields ''StaticPage

instance Tabular Post where
    type PKT Post = Text
    data Key k Post b where
        PostSlug :: Key Primary Post Text
        Tags :: Key Inverted Post (S.Set Text)

    data Tab Post i = PostTab (i Primary Text) (i Inverted (S.Set Text))

    fetch PostSlug post = post^._slug
    fetch Tags post = post^._tags & S.fromList

    primary = PostSlug
    primarily PostSlug r = r

    mkTab f = PostTab <$> f PostSlug <*> f Tags
    forTab (PostTab i t) f = PostTab <$> f PostSlug i <*> f Tags t
    ixTab (PostTab i _) PostSlug = i
    ixTab (PostTab _ t) Tags = t

instance Tabular StaticPage where
    type PKT StaticPage = Text
    data Key k StaticPage b where
        StaticPageSlug :: Key Primary StaticPage Text

    data Tab StaticPage i = StaticPageTab (i Primary Text)

    fetch StaticPageSlug = view _slug

    primary = StaticPageSlug
    primarily StaticPageSlug r = r

    mkTab f = StaticPageTab <$> f StaticPageSlug
    forTab (StaticPageTab i) f = StaticPageTab <$> f StaticPageSlug i
    ixTab (StaticPageTab i) StaticPageSlug = i
