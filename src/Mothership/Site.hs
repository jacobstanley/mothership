{-# LANGUAGE OverloadedStrings #-}

module Mothership.Site
    ( site
    , siteTH
    , getRepoDir
    ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Trans (MonadIO, liftIO)
import           Snap.Util.FileServe
import           System.FilePath
import           System.Directory

import           Mothership.Application
import           Mothership.Site.UI
import           Mothership.Site.Git

------------------------------------------------------------------------

site :: FilePath -> Application ()
site repoDir = serveUI repoDir
           <|> serveGit repoDir
           <|> serveDirectory "resources/static"

------------------------------------------------------------------------

siteTH :: Application ()
siteTH = do
    repoDir <- liftIO $ getRepoDir
    site repoDir

getRepoDir :: IO FilePath
getRepoDir = do
    dir <- getCurrentDirectory
    return $ dir </> "repositories"
