{-# LANGUAGE OverloadedStrings #-}

module Snap.Util.BasicAuth
    ( basicAuth
    ) where

import           Control.Arrow (second)
import           Data.ByteString.Base64 (decodeLenient)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Snap.Types

------------------------------------------------------------------------

-- | Protects the current handler using basic access authentication
-- (RFC2617).  This authentication scheme relies on a secure connection
-- between the client and server. If SSL is not used, the username and
-- password are transmitted in plain text.
basicAuth :: MonadSnap m
          => ByteString
          -- ^ the realm that is protected (e.g. your app name)
          -> (ByteString -> ByteString -> m Bool)
          -- ^ function to authenicate a username & password
          -> m ()
basicAuth realm authenticate = do
    req <- getRequest
    case getCredentials req of
        Nothing -> denyAccess
        Just (usr, pwd) -> do
            ok <- authenticate usr pwd
            if ok then return () else denyAccess
  where
    challenge = B.concat ["Basic realm=\"", realm, "\""]

    denyAccess = do
        modifyResponse
            $ setResponseCode 401
            . addHeader "WWW-Authenticate" challenge
        getResponse >>= finishWith

    getCredentials x = getHeader "Authorization" x >>= decode

    decode x = case B.words x of
        ["Basic", cs] -> Just $ splitColon $ decodeLenient cs
        _             -> Nothing

    splitColon = second (B.drop 1) . B.break (== ':')
