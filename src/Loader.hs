{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
-- | This is the module responsible for loading posts from disk, both
-- the initial load and the reload triggered by file modification.
module Loader (loadStaticPages, loadPosts) where

import           Control.Applicative           ((<$>))
import           Control.Exception             (catch, handle)
import           Control.Lens
import           Control.Monad                 (filterM, forM)
import           Data.ByteString               (ByteString)
import           Data.Data.Lens
import           Data.Either                   (partitionEithers)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8With)
import           Data.Text.Encoding.Error      (lenientDecode)
import qualified Data.Text.IO                  as T
import           Data.Time                     (formatTime, getCurrentTime,
                                                zonedTimeToUTC)
import           Data.Yaml                     ((.!=), (.:), (.:?))
import qualified Data.Yaml                     as Yaml
import qualified Filesystem                    as FS
import           Filesystem.Path.CurrentOS     ((</>))
import qualified Filesystem.Path.CurrentOS     as FS
import           System.IO.Error               (isDoesNotExistError)
import           System.Locale                 (defaultTimeLocale)
import           Text.InterpolatedString.Perl6 (qc)
import           Text.Pandoc                   (Block (CodeBlock), def,
                                                readMarkdown, writeHtml)

import           Types

-- | Do a one-shot load of all objects off of disk. Error messages are
-- printed to stdout.
loadObjects :: (FS.FilePath -> IO (Either String a))
               -- ^ The \'loader\' function, which takes the
               -- subdirectory.
               -> FS.FilePath
               -- ^ Path to the directory containing object folders.
               -> IO [a]
loadObjects loader dir = do
    subdirectories <- FS.listDirectory dir >>= filterM FS.isDirectory
    (errors, posts) <-
        (partitionEithers <$>) . forM subdirectories $ \subdir -> do
            loaded <- loader subdir
            case loaded of
                Left errMsg -> return . Left $
                               FS.encodeString subdir ++ ": " ++ errMsg
                Right post -> return . Right $ post
    mapM_ putStrLn errors
    return posts

-- | Load a list of posts from disk.
loadPosts :: FS.FilePath -> IO [Post]
loadPosts = loadObjects $ \subdir -> handle catcher $ do
    let Right slug = FS.toText . FS.basename $ subdir
    objText <- decodeUtf8With lenientDecode <$>
               readWithDefault (subdir </> "post.markdown")
               (return "Lorem ipsum")
    maybeYaml <- Yaml.decodeEither <$>
                 readWithDefault (subdir </> "meta.yml") postMetaTemplate
    return $ maybeYaml >>= Yaml.parseEither (buildPost slug objText)
      where catcher :: IOError -> IO (Either String a)
            catcher err = return . Left $ show err

-- | Load a list of static pages (i.e., pages rendered as if they were
-- posts that aren't actually posts) from disk.
loadStaticPages :: FS.FilePath -> IO [StaticPage]
loadStaticPages path = flip loadObjects path $ \subdir -> do
    let Right slug = FS.toText . FS.basename $ subdir
    objText <- decodeUtf8With lenientDecode <$> readWithDefault
               (subdir </> "page.markdown")
               (return "Lorem ipsum")
    maybeYaml <- Yaml.decodeEither <$>
                 readWithDefault
                 (subdir </> "meta.yml")
                 (return "title: Page title")
    return $ maybeYaml >>= Yaml.parseEither (buildStaticPage slug objText)
            >>= checkBadSlug
  where
    checkBadSlug post | view _slug post `elem` badSlugs =
        Left $ "Bad page slug " <> T.unpack (view _slug post) <> "."
                      | otherwise = Right post
    -- Based on the routes in Site.hs.
    badSlugs = ["static", "tagged", "post", "page", "drafts", "queue", "feed"]

-- | Build an individual post out of the 'Data.Text.Text' representing
-- the body and the 'Data.Yaml.Object' containing the metadata.
buildPost :: T.Text -> T.Text -> Yaml.Object -> Yaml.Parser Post
buildPost slug body o = do
    title <- o .: "title"
    tags <- o .:? "tags" .!= []
    isDraft <- o .:? "draft" .!= True
    posted <- zonedTimeToUTC . read <$> o .: "posted"
    language <- o .:? "code-language"
    -- If the code-language attribute is set, add it to all the code
    -- blocks and inline code.
    let addLang attrs@(ident, classes, pairs) = case language of
            Just lang -> (ident, lang:classes, pairs)
            Nothing -> attrs
        blockTransform (CodeBlock attrs code) = CodeBlock (addLang attrs) code
        blockTransform block = block
    return Post { __postTitle = title,
                  __postSlug = slug,
                  __postTags = tags,
                  __postIsDraft = isDraft,
                  __postBody = writeHtml def
                               . over template blockTransform
                               . readMarkdown def . T.unpack
                               $ body,
                  __postPosted = posted}

-- | Build a page out of the 'T.Text' representing the slug and body
-- and the 'Yaml.Object' object containing the metadata.
buildStaticPage :: T.Text -> T.Text -> Yaml.Object -> Yaml.Parser StaticPage
buildStaticPage slug body o = do
    title <- o .: "title"
    shortTitle <- o .:? "short-title" .!= title
    return StaticPage { __pageShortTitle = shortTitle
                      , __pageTitle = title
                      , __pageSlug = slug
                      , __pageBody = writeHtml def . readMarkdown def . T.unpack
                                     $ body
                }

-- | A metadata template with the most common fields filled in.
postMetaTemplate :: IO ByteString
postMetaTemplate = do
    now <- getCurrentTime
    return [qc|title: Post title
posted: {formatTime defaultTimeLocale "%F %T %Z" now}
draft: true
tags:
 - a sample tag|]

-- | Read a file from disk with optional default content.
readWithDefault :: FS.FilePath -> IO ByteString -> IO ByteString
readWithDefault path defaultTemplate = do
    FS.readFile path `catch` handler
  where handler err | isDoesNotExistError err = do
            tpl <- defaultTemplate
            FS.writeFile path tpl
            return tpl
                    | otherwise = ioError err
