{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

-- Store the templates and routes. For syntax highlighting purposes,
-- we store all templates in their own file unless they're one-liners
-- or similar.

module Templates where

import Data.Text (Text)
import Prelude hiding (FilePath)
import RelativeHamlet
import Snap.Core (writeLBS, MonadSnap)
import Text.Blaze.Renderer.Utf8 (renderMarkup)

data ItsaR = RootR


renderRoute :: ItsaR -> [(Text, Text)] -> Text
renderRoute RootR _ = "/"

mainPage :: Html
mainPage = $(hamletRelativeFile "templates/default-layout.hamlet") renderRoute

renderTemplate :: (MonadSnap m) => Html -> m ()
renderTemplate = writeLBS . renderMarkup
