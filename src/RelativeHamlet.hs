{-# LANGUAGE OverloadedStrings #-}

-- This module is... not nice. flycheck checks from this directory,
-- but cabal builds from one directory above. So we export
-- quasiquoters that find the .cabal file and look relative to it (as
-- well as the Html type for convenience.) This has to be in a
-- separate module from anything that uses hamletRelativeFile due to
-- how TH staging works.

module RelativeHamlet (hamletRelativeFile, Html) where

import           Control.Applicative ((<$>))
import           Data.Maybe (catMaybes)
import           Filesystem
import qualified Filesystem.Path.CurrentOS as FS
import           Filesystem.Path.CurrentOS hiding (null, FilePath)
import           Language.Haskell.TH.Syntax (qAddDependentFile, qRunIO, Q, Exp)

import           Text.Hamlet (defaultHamletSettings, hamletFileWithSettings,
                              hamletRules, Html)

findCabalDirFrom :: FS.FilePath -> IO FS.FilePath
findCabalDirFrom path = do
    files <- filter ((/= ".cabal") . filename) <$> listDirectory path
    let dirExtensions = catMaybes . map extension $ files
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
