{-# LANGUAGE OverloadedStrings #-}

module Mothership.Site
    ( site
    ) where

import           Control.Applicative ((<|>))
import           Snap.Util.FileServe

import           Mothership.Application
import           Mothership.Site.UI
import           Mothership.Site.Git

------------------------------------------------------------------------

site :: Application ()
site = serveUI <|> serveGit <|> serveDirectory "resources/static"
