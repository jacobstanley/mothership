{-# LANGUAGE OverloadedStrings #-}

module GitSnap.Site.Git
    ( serveGit
    ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Snap.Types hiding (path, dir)
import           Snap.Util.FileServe
import           System.FilePath
import           System.Directory
import           Text.Printf

import           GitSnap.Application
import           GitSnap.Snap
import           GitSnap.Util

------------------------------------------------------------------------

serveGit :: FilePath -> Application ()
serveGit repoDir = route [(":user/:repo", serveRepo repoDir)]

serveRepo :: FilePath -> Application ()
serveRepo repoDir = do
    user <- getParamStr "user"
    repo <- getParamStr "repo"

    let path = repoDir </> user </> repo

    repoExists <- liftIO $ doesDirectoryExist path
    guard repoExists

    routes path
  where
    with m = method m . routeTop
    routes path =
         with POST [ ("git-upload-pack", rpc "upload-pack" path)
                   , ("git-receive-pack", rpc "receive-pack" path)
                   ]
     <|> with GET  [ ("info/refs", infoRefs path)
                   , textFile path "HEAD"
                   ]

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

textFile :: FilePath -> ByteString -> (ByteString, Application ())
textFile dir file = (file, serve)
  where
    path = dir </> B.unpack file
    serve = do
        liftIO $ putStrLn $ "Serving " ++ path
        serveFileAs "text/plain" path

------------------------------------------------------------------------

git :: MonadSnap m => FilePath -> [ByteString] -> m ()
git repo args = do
    g <- process repo "git" (map B.unpack args)
    runRequestBody' (stdin g)
    addToOutputBS (stdout g)
