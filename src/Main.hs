{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

#ifdef DEVELOPMENT
import Snap.Extension.Loader.Devel
import Snap.Http.Server (quickHttpServe)
#else
import Snap.Extension.Server
#endif

import GitSnap.Application
import GitSnap.Site

main :: IO ()
#ifdef DEVELOPMENT
main = do
    -- All source directories will be watched for updates
    -- automatically.  If any extra directories should be watched for
    -- updates, include them here.
    snap <- $(let extraWatcheDirs = ["resources/templates"]
              in loadSnapTH 'applicationInitializer 'siteTH extraWatcheDirs)
    quickHttpServe snap
#else
main = do
    repoDir <- getRepoDir
    quickHttpServe applicationInitializer (site repoDir)
#endif

