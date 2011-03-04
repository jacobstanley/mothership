{-# LANGUAGE OverloadedStrings #-}

module Mothership.Site.Git
    ( serveGit
    ) where

import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Snap.Types hiding (path, dir)
import           Snap.Util.FileServe
import           System.FilePath
import           System.Directory
import           Text.Printf

import           Mothership.Application
import           Mothership.Snap
import           Mothership.Util

------------------------------------------------------------------------

serveGit :: Application ()
serveGit = route [(":user/:repo", serveRepo)]

serveRepo :: Application ()
serveRepo = do
    repoDir <- getRepoDir
    user <- getParamStr "user"
    repo <- getParamStr "repo"

    let path = repoDir </> user </> repo

    repoExists <- liftIO $ doesDirectoryExist path
    guard repoExists

    routeTop
      [ ("git-upload-pack",  method POST $ rpc "upload-pack" path)
      , ("git-receive-pack", method POST $ rpc "receive-pack" path)
      , ("info/refs", method GET $ infoRefs path)
      , ("HEAD",      method GET $ serveText $ path </> "HEAD") ]

------------------------------------------------------------------------

serveText :: FilePath -> Application ()
serveText = serveFileAs "text/plain"

------------------------------------------------------------------------

rpc :: ByteString -> FilePath -> Application ()
rpc service path = do
    contentType' ["application/x-git-", service, "-result"]
    git path [service, "--stateless-rpc", "."]

------------------------------------------------------------------------

infoRefs :: FilePath -> Application ()
infoRefs path = do
    service <- getService

    cacheDisabled
    contentType' ["application/x-git-", service, "-advertisement"]

    writeBS $ pktWrite' ["# service=git-", service, "\n"]
    writeBS pktFlush

    git path [service, "--stateless-rpc", "--advertise-refs", "."]

getService :: MonadSnap m => m ByteString
getService = getParamMap f "service"
  where
    f x = if "git-" `B.isPrefixOf` x
          then Just (B.drop 4 x)
          else Nothing

pktFlush :: ByteString
pktFlush = "0000"

pktWrite :: ByteString -> ByteString
pktWrite str = size `B.append` str
  where
    size = B.pack $ printf "%04x" (B.length str + 4)

pktWrite' :: [ByteString] -> ByteString
pktWrite' = pktWrite . B.concat

------------------------------------------------------------------------

git :: MonadSnap m => FilePath -> [ByteString] -> m ()
git repo args = do
    g <- process repo "git" (map B.unpack args)
    runRequestBody' (stdin g)
    addToOutputBS (stdout g)
