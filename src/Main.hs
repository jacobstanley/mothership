{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Control.Monad.Trans (liftIO)
import           Text.Printf

import           Snap
import           Util

------------------------------------------------------------------------

main :: IO ()
main = httpServe' defaultConfig site

------------------------------------------------------------------------

site :: Snap ()
site = route
    [ ("",                      ifTop $ writeBS "gitsnap")
    , (":user/:repo/info/refs", ifTop refs)
    ]

refs :: Snap ()
refs = do
    --Just user <- getParam "user"
    --Just repo <- getParam "repo"
    Just service <- getService
    (_, out, _) <- liftIO $ run "." "git" [B.unpack service, "--stateless-rpc", "--advertise-refs", "."]

    contentType $ B.concat ["application/x-git-", service,"-advertisement"]
    cacheDisabled
    writeBS $ pktWrite $ B.concat ["# service=git-", service, "\n"]
    writeBS pktFlush
    writeBS out

------------------------------------------------------------------------

getService :: Snap (Maybe B.ByteString)
getService = do
    service <- getParam "service"
    return (checked service)
  where
    checked Nothing        = Nothing
    checked (Just service) = if "git-" `B.isPrefixOf` service
                             then Just (B.drop 4 service)
                             else Nothing

pktFlush :: ByteString
pktFlush = "0000"

pktWrite :: ByteString -> ByteString
pktWrite str = size `B.append` str
  where
    size = B.pack $ printf "%04x" (B.length str + 4)
