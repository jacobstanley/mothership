{-# LANGUAGE OverloadedStrings #-}

module Snap
    ( SnapConfig (..)
    , defaultConfig
    , httpServe'
    , module Snap.Types
    ) where

import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.GZip
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
