{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Snap.Http.Server
import           Snap.Types hiding (path)
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath
import           Text.Printf

import           GitSnap.Snap
import           GitSnap.Util

------------------------------------------------------------------------

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    let root = cwd </> "repositories"
    httpServe defaultConfig (site root)

------------------------------------------------------------------------

site :: FilePath -> Snap ()
site root = ifTop (writeBS "gitsnap")
        <|> route [(":user/:repo", repository root)]

repository :: FilePath -> Snap ()
repository root = do
    user <- getParamStr "user"
    repo <- getParamStr "repo"
    let path = root </> user </> repo

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

textFile :: FilePath -> ByteString -> (ByteString, Snap ())
textFile p f = (f, serve)
  where
    path = p </> B.unpack f
    serve = do
        liftIO $ putStrLn $ "Serving " ++ path
        serveFileAs "text/plain" path

------------------------------------------------------------------------

rpc :: ByteString -> FilePath -> Snap ()
rpc service path = do
    contentType' ["application/x-git-", service, "-result"]
    git path [service, "--stateless-rpc", "."]

------------------------------------------------------------------------

infoRefs :: FilePath -> Snap ()
infoRefs path = do
    service <- getService

    cacheDisabled
    contentType' ["application/x-git-", service, "-advertisement"]

    writeBS $ pktWrite' ["# service=git-", service, "\n"]
    writeBS pktFlush

    git path [service, "--stateless-rpc", "--advertise-refs", "."]

------------------------------------------------------------------------

git :: MonadSnap m => FilePath -> [ByteString] -> m ()
git repo args = do
    g <- gitProcess repo args
    runRequestBody' (stdin g)
    addToOutputBS (stdout g)

gitProcess :: MonadIO m => FilePath -> [ByteString] -> m Process
gitProcess repo = process repo "git" . map B.unpack

------------------------------------------------------------------------

getService :: Snap ByteString
getService = getParamMap f "service"
  where
    f x = if "git-" `B.isPrefixOf` x
          then Just (B.drop 4 x)
          else Nothing

------------------------------------------------------------------------

pktFlush :: ByteString
pktFlush = "0000"

pktWrite :: ByteString -> ByteString
pktWrite str = size `B.append` str
  where
    size = B.pack $ printf "%04x" (B.length str + 4)

pktWrite' :: [ByteString] -> ByteString
pktWrite' = pktWrite . B.concat

------------------------------------------------------------------------

printURI :: Snap ()
printURI = do
    req <- getRequest
    liftIO $ putStrLn $
        "\n" ++ show (rqMethod req) ++ " " ++ B.unpack (rqURI req)
