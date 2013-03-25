{-# LANGUAGE OverloadedStrings #-}
-- | This is the module responsible for loading posts from disk, both
-- the initial load and the reload triggered by file modification.
module Post.Loader where

import           Control.Applicative       ((<$>))
import           Control.Exception
import           Control.Monad             (filterM, forM)
import           Data.Either               (partitionEithers)
import           Data.Table
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8With)
import           Data.Text.Encoding.Error  (lenientDecode)
import           Data.Time.LocalTime       (zonedTimeToUTC)
import           Data.Yaml                 ((.!=), (.:), (.:?))
import qualified Data.Yaml                 as Yaml
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FS
import           Text.Pandoc               (def, readMarkdown, writeHtml)

import           Post.Types

-- | Do a one-shot load of all posts off of disk. Error messages are
-- printed to stdout, but do not show up on the webpage!
loadPosts :: FS.FilePath -- ^ Path to the directory containing post folders.
             -> IO (Table Post)
loadPosts dir = do
    subdirectories <- FS.listDirectory dir >>= filterM FS.isDirectory
    (errors, posts) <-
        (partitionEithers <$>) . forM subdirectories $ \subdir -> do
            loaded <- loadPost subdir
            case loaded of
                Left errMsg -> return . Left $
                               FS.encodeString subdir ++ ": " ++ errMsg
                Right post -> return . Right $ post
    mapM_ putStrLn errors
    return $ fromList posts

-- | Try to load an individual post using its post.markdown and
-- meta.yml. Returns Left on parse failure or failure to laod a post,
-- or Right on success.
loadPost :: FS.FilePath -- ^ Path to the directory containing post data.
            -> IO (Either String Post)
loadPost directoryPath = handle catcher $ do
    postText <- decodeUtf8With lenientDecode <$>
                FS.readFile (directoryPath </> "post.markdown")
    maybeYaml <- Yaml.decodeEither <$>
                 FS.readFile (directoryPath </> "meta.yml")
    return (maybeYaml >>= Yaml.parseEither (buildPost postText))
    where catcher :: IOError -> IO (Either String Post)
          catcher err = return . Left $ show err

-- | Build an individual post out of the 'Data.Text.Text' representing
-- the body and the 'Data.Yaml.Object' containing the metadata.
buildPost :: T.Text -> Yaml.Object -> Yaml.Parser Post
buildPost body o = do
    title <- o .: "title"
    slug <- o .: "slug"
    tags <- o .:? "tags" .!= []
    isDraft <- o .:? "draft" .!= True
    posted <- zonedTimeToUTC . read <$> o .: "posted"
    return Post { __title = title,
                  __slug = slug,
                  __tags = tags,
                  __isDraft = isDraft,
                  __body = writeHtml def . readMarkdown def . T.unpack $ body,
                  __posted = posted}
