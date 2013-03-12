{-# LANGUAGE OverloadedStrings #-}

-- This module is... not nice. flycheck checks from this directory,
-- but cabal builds from one directory above. So we export
-- quasiquoters that find the .cabal file and look relative to it (as
-- well as the Html type for convenience.) This has to be in a
-- separate module from anything that uses hamletRelativeFile due to
-- how TH staging works.

module RelativeHamlet (hamletRelativeFile, Html) where

import           Control.Applicative        ((<$>))
import           Data.Maybe                 (mapMaybe)
import           Filesystem
import           Filesystem.Path.CurrentOS  hiding (FilePath, null)
import qualified Filesystem.Path.CurrentOS  as FS
import           Language.Haskell.TH.Syntax (Exp, Q, qAddDependentFile, qRunIO)

import           Text.Hamlet                (Html, defaultHamletSettings,
                                             hamletFileWithSettings,
                                             hamletRules)

findCabalDirFrom :: FS.FilePath -> IO FS.FilePath
findCabalDirFrom path = do
    files <- filter ((/= ".cabal") . filename) <$> listDirectory path
    let dirExtensions = mapMaybe extension files
    if "cabal" `elem` dirExtensions
        then return path
        else do
            canonicalParent <- canonicalizePath $ parent path
            canonicalPath <- canonicalizePath path
            if canonicalParent == canonicalPath
               then error "reached root (not below a .cabal file?)"
                else findCabalDirFrom canonicalParent

-- Load a Hamlet file with a path relative to the .cabal directory.
hamletRelativeFile :: String -> Q Exp
hamletRelativeFile path = do
    dir <- qRunIO $ getWorkingDirectory >>= findCabalDirFrom
    let realPath = encodeString (dir </> decodeString path)
    qAddDependentFile realPath
    hamletFileWithSettings hamletRules defaultHamletSettings realPath
