{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           Snap.Http.Server
import           Snap.Types
import           System.Directory
import           System.Exit
import           System.FilePath
import           Text.Printf

import           Snap hiding (path)
import           Util

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
                   ]
     <|> with GET  [ ("info/refs", infoRefs path)
                   ]

------------------------------------------------------------------------

rpc :: ByteString -> FilePath -> Snap ()
rpc service path = do
    contentType' ["application/x-git-", service, "-result"]
    body <- getRequestBody
    out <- git' path [service, "--stateless-rpc", "."] body
    writeBS out

------------------------------------------------------------------------

infoRefs :: FilePath -> Snap ()
infoRefs path = do
    service <- getService
    refs <- git path [service, "--stateless-rpc", "--advertise-refs", "."]

    contentType' ["application/x-git-", service, "-advertisement"]
    cacheDisabled

    writeBS $ pktWrite' ["# service=git-", service, "\n"]
    writeBS pktFlush
    writeBS refs

------------------------------------------------------------------------

git :: FilePath -> [ByteString] -> Snap ByteString
git repo args = git' repo args ""

git' :: FilePath -> [ByteString] -> L.ByteString -> Snap ByteString
git' repo args input = do
    liftIO $ putStr $ "\n" : info
    result <- liftIO $ cmd' repo "git" args' input
    case result of
        (ExitSuccess, out, _)        -> do
            liftIO $ putStrLn $ "[sending " ++ show (B.length out) ++ " bytes]"
            return out
        (ExitFailure code, out, err) -> do
            let msg = formatError code out err

            liftIO $ B.putStr $ msg `B.snoc` '\n'
            error $ B.unpack $ B.concat
                [ "git failed:\n"
                , B.pack info
                , formatError code out err]
  where
    args' = map B.unpack args
    info = "(in " ++ repo ++ ")\n$ git " ++ unwords args' ++ "\n"
    formatError code out err = B.concat
        [ out, err
        , "(exit code was ", B.pack (show code), ")" ]

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
