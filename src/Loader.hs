{-# LANGUAGE OverloadedStrings #-}
-- | This is the module responsible for loading posts from disk, both
-- the initial load and the reload triggered by file modification.
module Loader (loadStaticPages, loadPosts) where

import           Control.Applicative       ((<$>))
import           Control.Exception         (handle)
import           Control.Lens
import           Control.Monad             (filterM, forM)
import           Data.Data.Lens
import           Data.Either               (partitionEithers)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8With)
import           Data.Text.Encoding.Error  (lenientDecode)
import           Data.Time.LocalTime       (zonedTimeToUTC)
import           Data.Yaml                 ((.!=), (.:), (.:?))
import qualified Data.Yaml                 as Yaml
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FS
import           Text.Pandoc               (Block (CodeBlock), def,
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
loadPosts = loadObjects $ \subdir -> loadObject buildPost
                                     (subdir </> "post.markdown")
                                     (subdir </> "meta.yml")

-- | Load a list of static pages (i.e., pages rendered as if they were
-- posts that aren't actually posts) from disk.
loadStaticPages :: FS.FilePath -> IO [StaticPage]
loadStaticPages = loadObjects $ \subdir -> loadObject buildStaticPage
                                     (subdir </> "page.markdown")
                                     (subdir </> "meta.yml")


-- | Load some kind of post-y object given the files to load from and
-- a \'builder\' function. The slug is taken from the directory name
-- of the content.
loadObject :: (T.Text -> T.Text -> Yaml.Object -> Yaml.Parser a)
              -- ^ The \'builder\', which takes the slug, body, and the
              -- yaml metadata.
              -> FS.FilePath -- ^ Path to the content.
              -> FS.FilePath -- ^ Path to the metadata.
              -> IO (Either String a)
loadObject builder contentPath metadataPath = handle catcher $ do
    let Right slug = FS.toText . FS.dirname $ contentPath
    objText <- decodeUtf8With lenientDecode <$> FS.readFile contentPath
    maybeYaml <- Yaml.decodeEither <$> FS.readFile metadataPath
    return (maybeYaml >>= Yaml.parseEither (builder slug objText))
    where catcher :: IOError -> IO (Either String a)
          catcher err = return . Left $ show err


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
