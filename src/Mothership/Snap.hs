{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mothership.Snap
    ( contentType
    , contentType'
    , cacheDisabled
    , cacheForever
    , expires

    , routeTop

    , getParamBS
    , getParamStr
    , getParamMap

    , runRequestBody'
    , addToOutputBS
    ) where

import           Blaze.ByteString.Builder
import           Codec.Zlib
import           Control.Monad.Trans (MonadIO, lift, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Data.Time.Format (formatTime)
import           Prelude hiding (catch)
import           Snap.Iteratee hiding (map)
import qualified Snap.Iteratee as I
import           Snap.Types hiding (formatHttpTime)
import           System.Locale (defaultTimeLocale)

------------------------------------------------------------------------

contentType :: MonadSnap m => B.ByteString -> m ()
contentType ct = modifyResponse (setContentType ct)

contentType' :: MonadSnap m => [B.ByteString] -> m ()
contentType' = contentType . B.concat

cacheDisabled :: MonadSnap m => m ()
cacheDisabled = modifyResponse $
    setHeader "Expires"       "Fri, 01 Jan 1980 00:00:00 GMT" .
    setHeader "Pragma"        "no-cache" .
    setHeader "Cache-Control" "no-cache, max-age=0, must-revalidate"

cacheForever :: MonadSnap m => m ()
cacheForever = expires 31536000

------------------------------------------------------------------------

expires :: MonadSnap m => Integer -> m ()
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

routeTop :: MonadSnap m => [(ByteString, m a)] -> m a
routeTop = route . map (mapSnd ifTop)
  where
    mapSnd f (x, y) = (x, f y)

------------------------------------------------------------------------

getParamBS :: MonadSnap m => ByteString -> m ByteString
getParamBS = getParamMap Just

getParamStr :: MonadSnap m => ByteString -> m String
getParamStr = getParamMap (Just . B.unpack)

getParamMap :: MonadSnap m => (ByteString -> Maybe a) -> ByteString -> m a
getParamMap f name = getParam name >>= \mstr -> case mstr of
    Nothing  -> throwEx ["parameter '", name, "' does not exist"]
    Just str -> case f str of
        Nothing  -> throwEx ["'", str, "' is not a valid value for ",
                             "parameter '", name, "'"]
        Just val -> return val
  where
    throwEx = error . B.unpack . B.concat

------------------------------------------------------------------------

runRequestBody' :: MonadSnap m => Iteratee ByteString IO a -> m a
runRequestBody' iter = do
    req <- getRequest
    runRequestBody $ decompress req $ iter
  where
    decompress req x = case getHeader "content-encoding" req of
        Just "gzip" -> joinI $ ungzip $$ x
        _ -> x

addToOutputBS :: MonadSnap m => (forall a. Enumerator ByteString IO a) -> m ()
addToOutputBS e = addToOutput $ mapEnum toByteString fromByteString e

------------------------------------------------------------------------

-- Stolen from http-enumerator (Network.HTTP.Enumerator.Zlib)
ungzip :: MonadIO m => Enumeratee B.ByteString B.ByteString m b
ungzip inner = do
    fzstr <- liftIO $ initInflate $ WindowBits 31
    ungzip' fzstr inner

ungzip' :: MonadIO m => Inflate -> Enumeratee B.ByteString B.ByteString m b
ungzip' fzstr (Continue k) = do
    x <- I.head
    case x of
        Nothing -> do
            chunk <- liftIO $ finishInflate fzstr
            lift $ runIteratee $ k $ Chunks [chunk]
        Just bs -> do
            chunks <- liftIO $ withInflateInput fzstr bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            ungzip' fzstr step
  where
    go front pop = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just y -> go (front . (:) y) pop
ungzip' _ step = return step
