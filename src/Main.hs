{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Blaze.ByteString.Builder
import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Enumerator (joinE)
import           Snap.Http.Server
import           Snap.Iteratee hiding (map)
import qualified Snap.Iteratee as I
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

    printURI

    routes path
  where
    with m = method m . routeTop
    routes path =
         with POST [ ("git-upload-pack", rpc "upload-pack" path)
                   ]
     <|> with GET  [ ("info/refs", infoRefs path)
                   , textFile path "HEAD"
                   ]

textFile :: FilePath -> ByteString -> (ByteString, Snap ())
textFile p f = (f, do
    liftIO $ putStrLn "Serving file"
    serveFileAs "text/plain" $ p </> B.unpack f)

------------------------------------------------------------------------

rpc :: ByteString -> FilePath -> Snap ()
rpc service path = do
    contentType' ["application/x-git-", service, "-result"]
    transformRequestBody' $ git' path [service, "--stateless-rpc", "."]

------------------------------------------------------------------------

infoRefs :: FilePath -> Snap ()
infoRefs path = do
    service <- getService

    cacheDisabled
    contentType' ["application/x-git-", service, "-advertisement"]

    transformRequestBody $
        git path [service, "--stateless-rpc", "--advertise-refs", "."]
        <==< enumBS' (pktWrite' ["# service=git-", service, "\n"])
        <==< enumBS' pktFlush


enumBS' :: forall a. ByteString -> Enumerator Builder IO a
enumBS' bs = enumBuilder $ fromByteString bs

------------------------------------------------------------------------

git :: MonadIO m
    => FilePath
    -> [ByteString]
    -> Enumerator Builder m a
git repo args = git' repo args

git' :: MonadIO m
     => FilePath
     -> [ByteString]
     -> Enumerator Builder m a
git' repo args step = do
    liftIO $ putStr info
    cmdI repo "git" args' step
  where
    args' = map B.unpack args
    info = "(in " ++ repo ++ ")\n$ git " ++ unwords args' ++ "\n"

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

transformRequestBody' :: (forall a . Enumerator Builder IO a) -> Snap ()
transformRequestBody' enum = do
    req <- getRequest
    transformRequestBody $ joinE enum $ decompress req
  where
    decompress :: Request -> (forall a. Enumeratee Builder Builder IO a)
    decompress req =
        case getHeader "content-encoding" req of
            Just "gzip" -> ungzip
            _ -> I.map id

printURI :: Snap ()
printURI = do
    req <- getRequest
    liftIO $ putStrLn $
        "\n" ++ show (rqMethod req) ++ " " ++ B.unpack (rqURI req)
