{-# LANGUAGE OverloadedStrings #-}

module Snap
    ( contentType
    , contentType'
    , cacheDisabled
    , cacheForever
    , expires

    , routeTop

    , getParamBS
    , getParamStr
    , getParamMap
    ) where

import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Data.Time.Format (formatTime)
import           Prelude hiding (catch)
import           Snap.Types hiding (formatHttpTime)
import           System.Locale (defaultTimeLocale)

------------------------------------------------------------------------

contentType :: B.ByteString -> Snap ()
contentType ct = modifyResponse (setContentType ct)

contentType' :: [B.ByteString] -> Snap ()
contentType' = contentType . B.concat

cacheDisabled :: Snap ()
cacheDisabled = modifyResponse $
    setHeader "Expires"       "Fri, 01 Jan 1980 00:00:00 GMT" .
    setHeader "Pragma"        "no-cache" .
    setHeader "Cache-Control" "no-cache, max-age=0, must-revalidate"

cacheForever :: Snap ()
cacheForever = expires 31536000

------------------------------------------------------------------------

expires :: Integer -> Snap ()
expires seconds = do
    now <- liftIO getCurrentTime
    let expiry = addUTCTime (fromInteger seconds) now
    modifyResponse (setExpires now expiry)

setExpires :: UTCTime -> UTCTime -> Response -> Response
setExpires now expiry =
    setHeader "Expires"       (formatHttpTime expiry) .
    setHeader "Cache-Control" ("public, must-revalidate, max-age=" `B.append` maxAge)
  where
    diff = round (diffUTCTime expiry now) :: Integer
    maxAge = B.pack (show diff)

formatHttpTime :: UTCTime -> ByteString
formatHttpTime = B.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

------------------------------------------------------------------------

routeTop :: [(ByteString, Snap a)] -> Snap a
routeTop = route . map (mapSnd ifTop)
  where
    mapSnd f (x, y) = (x, f y)

------------------------------------------------------------------------

getParamBS :: ByteString -> Snap ByteString
getParamBS = getParamMap Just

getParamStr :: ByteString -> Snap String
getParamStr = getParamMap (Just . B.unpack)

getParamMap :: (ByteString -> Maybe a) -> ByteString -> Snap a
getParamMap f name = getParam name >>= \mstr -> case mstr of
    Nothing  -> throwEx ["parameter '", name, "' does not exist"]
    Just str -> case f str of
        Nothing  -> throwEx ["'", str, "' is not a valid value for ",
                             "parameter '", name, "'"]
        Just val -> return val
  where
    throwEx = error . B.unpack . B.concat
