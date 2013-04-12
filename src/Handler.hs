{-# LANGUAGE OverloadedStrings, QuasiQuotes, Rank2Types #-}

-- | Individual handlers. We use the renderers defined in Renderer and
-- our own logic for picking which posts to render.

module Handler (draftsPage, mainPage, postPage, queuePage, tagPage, rss,
                staticPage)
       where

import           Control.Applicative      ((<$>))
import           Control.Lens
import           Control.Monad.Reader
import           Data.ByteString          (ByteString)
import           Data.Maybe               (fromMaybe)
import           Data.Table
import           Data.Text                (Text, unpack)
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time                (TimeZone, getCurrentTime)
import           Prelude                  hiding (FilePath)
import           Snap.Core
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.Hamlet              (HtmlUrl, hamlet)
import qualified Text.XML                 as XML

import           Application
import           Config
import           Renderer
import           Types

-- | This handler renders the main page; i.e., the most recent posts.
mainPage :: AppHandler ()
mainPage = showPaginatedPosts PageR id

-- | Show posts with a given tag.
tagPage :: AppHandler ()
tagPage = do
    mTagName <- getParamAsText "tagName"
    assign _subtitle mTagName
    case mTagName of
        Nothing -> error "???? failure to get tag name from tag page"
        Just tagName -> showPaginatedPosts (TagR tagName) $
                        withAny Tags [tagName]

-- | Show all draft posts.
draftsPage :: AppHandler ()
draftsPage = localhostOnly $
             showAllPaginatedPosts QueueR (withG _isDraft (==) True)

-- | Show all queued, non-draft posts.
queuePage :: AppHandler ()
queuePage = localhostOnly $ do
    now <- liftIO getCurrentTime
    showAllPaginatedPosts DraftsR $ withG _posted (>) now
        .withG _isDraft (==) False

-- | Show the post with a given slug.
postPage :: AppHandler ()
postPage = do
    postSlug <- fromMaybe (error "post route doesn't have :slug parameter?")
             <$> getParamAsText "slug"
    tz <- view $ _config._timeZone
    serveTemplate =<< renderDefault =<< postPage' tz postSlug
      where
        -- Given a slug, either render that post or 404.
        postPage' :: TimeZone -> Text -> AppHandler (HtmlUrl ItsaR)
        postPage' tz slug = do
            postTable <- getPostTable
            case postTable^.at slug of
                Just post -> do
                    assign _subtitle $ Just $ view _title post
                    return $ renderPost tz post
                Nothing -> return render404

staticPage :: AppHandler ()
staticPage = do
    pageName <- fromMaybe (error "page route doesn't have :pageName parameter?")
                <$> getParamAsText "pageName"
    serveTemplate =<< renderDefault =<< page' pageName
    where
      page' :: Text -> AppHandler (HtmlUrl ItsaR)
      page' slug = do
          staticPageTable <- getStaticPageTable
          case staticPageTable^.at slug of
               Just page -> do
                   assign _subtitle $ Just $ view _title page
                   return $ renderStaticPage page
               Nothing -> return render404

rss :: AppHandler ()
rss = do
    postTable <- getPostTable
    now <- liftIO getCurrentTime
    let posts = postTable^..withG _isDraft (==) False .withG _posted (<=) now
                .group (^._posted).rows'
                & take 10 . reverse
    writeLBS =<< XML.renderLBS XML.def <$> renderRss posts


-- | Similar to 'showAllPaginatedPosts', but excludes drafts and queued posts.
showPaginatedPosts :: (Int -> ItsaR) -- ^ A function from page numbers
                                     -- to URLs.
                      -> Lens' (Table Post) (Table Post)
                      -- ^ The lens to apply to the post table.
                      -> AppHandler ()
showPaginatedPosts pageRouter postFilter = do
    now <- liftIO getCurrentTime
    showAllPaginatedPosts pageRouter $ postFilter.withG _isDraft (==) False
        .withG _posted (<=) now

-- | Show the given page of posts filtered using the given lens. This
-- uses the :page parameter name, but defaults to page 1.
showAllPaginatedPosts :: (Int -> ItsaR) -- ^ A function from page
                         -- numbers to URLs.
                         -> Lens' (Table Post) (Table Post)
                         -- ^ The lens to apply to the post table.
                         -> AppHandler ()
showAllPaginatedPosts pageRouter postFilter = do
    pageNumber <- fromMaybe 1 <$> readParam "page"
    postsPerPage <- view $ _config._postsPerPage
    tz <- view $ _config._timeZone
    postTable <- getPostTable
    let posts' = postTable^..postFilter.group (^._posted).rows'
                & take (postsPerPage + 1) . drop ((pageNumber - 1) * postsPerPage)
                . reverse
        -- By taking one more post than is necessary, we can determine
        -- whether there are more posts to show.
        hasNext = length posts' > postsPerPage
        posts = if hasNext then init posts' else posts'
    let pagination = renderPagination pageRouter pageNumber hasNext
    serveTemplate =<< renderDefault
        (liftM2 (>>) (renderPosts tz posts) pagination)

-- | Ensure that the requesting IP is 127.0.0.1, or else 403.
localhostOnly :: AppHandler () -> AppHandler ()
localhostOnly action = do
    reqIp <- rqRemoteAddr <$> getRequest
    if reqIp == "127.0.0.1" then action else do
        modifyResponse $ setResponseCode 403
        serveTemplate =<< renderDefault [hamlet|<h1>403|]
        finishWith =<< getResponse

-- | Serve a template using Snap by supplying the route renderer to
-- it, rendering it, and writing as a lazy
-- 'Data.ByteString.Lazy.ByteString'.
serveTemplate :: HtmlUrl ItsaR -> AppHandler ()
serveTemplate tpl = do
    appRoot <- view $ _config._appRoot
    writeLBS . renderMarkup $ tpl $ Renderer.renderRoute appRoot

getParamAsText :: (MonadSnap m) => ByteString -> m (Maybe Text)
getParamAsText param = fmap (decodeUtf8With lenientDecode) <$> getParam param

readParam :: (MonadSnap m, Read a) => ByteString -> m (Maybe a)
readParam param = fmap (read . unpack) <$> getParamAsText param

-- | Like 'with', but taking a lens as its first argument.
withG :: (Ord a) => Getter s a -> (forall x. Ord x => x -> x -> Bool)
         -> a -> Simple Lens (Table s) (Table s)
withG l = with (^.l)
