{-# LANGUAGE OverloadedStrings #-}

module Snap
    ( SnapConfig (..)
    , defaultConfig
    , httpServe'

    , contentType
    , contentType'
    , cacheDisabled
    , cacheForever
    , expires

    , routeTop

    , getParamBS
    , getParamStr
    , getParamMap

    , module Snap.Types
    ) where

import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Data.Time.Format (formatTime)
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types hiding (formatHttpTime)
import           Snap.Util.GZip
import           System.Locale (defaultTimeLocale)
import           Text.XHtmlCombinators.Escape (escape)

------------------------------------------------------------------------

data SnapConfig = SnapConfig
    { cfgInterface       :: ByteString
    , cfgPort            :: Int
    , cfgHostname        :: ByteString
    , cfgAccessLog       :: Maybe FilePath
    , cfgErrorLog        :: Maybe FilePath
    , cfgCompression     :: Bool
    , cfgError500Handler :: SomeException -> Snap ()
    }

defaultConfig :: SnapConfig
defaultConfig = SnapConfig
    { cfgInterface       = "0.0.0.0"
    , cfgPort            = 8000
    , cfgHostname        = "hostname"
    , cfgAccessLog       = Just "access.log"
    , cfgErrorLog        = Just "error.log"
    , cfgCompression     = True
    , cfgError500Handler = \e -> do
        let t = T.pack $ show e
            r = setContentType "text/html; charset=utf-8" $
                setResponseStatus 500 "Internal Server Error" emptyResponse
        putResponse r
        writeBS "<html><head><title>Internal Server Error</title></head>"
        writeBS "<body><h1>Internal Server Error</h1>"
        writeBS "<p>A web handler threw an exception. Details:</p>"
        writeBS "<pre>\n"
        writeText (escape t)
        writeBS "\n</pre></body></html>"
    }

------------------------------------------------------------------------

httpServe' :: SnapConfig -> Snap () -> IO ()
httpServe' config handler = do
    putStrLn $ "Listening on " ++ B.unpack (cfgInterface config) ++ ":" ++ show (cfgPort config)
    tryHttpServe
    putStrLn "Shutting down"
  where
    tryHttpServe :: IO (Either SomeException ())
    tryHttpServe = try $ httpServe (cfgInterface config)
                                   (cfgPort      config)
                                   (cfgHostname  config)
                                   (cfgAccessLog config)
                                   (cfgErrorLog  config)
                                   (catch500 $ compress handler)

    catch500 = (`catch` cfgError500Handler config)
    compress = if cfgCompression config then withCompression else id

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
