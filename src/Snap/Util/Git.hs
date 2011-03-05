{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Snap.Util.Git
    ( serveRepo
    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Prelude hiding (catch, head)
import           Snap.Iteratee hiding (map)
import           Snap.Types hiding (path, dir)
import           Snap.Util.FileServe
import           System.Exit
import           System.FilePath
import           System.IO (Handle, hClose)
import           System.Process
import           Text.Printf

import           Snap.Util

------------------------------------------------------------------------

serveRepo :: MonadSnap m => FilePath -> m ()
serveRepo repo = route
    [ ("git-upload-pack",  method POST $ rpc "upload-pack" repo)
    , ("git-receive-pack", method POST $ rpc "receive-pack" repo)
    , ("info/refs", method GET $ infoRefs repo)
    , ("HEAD",      method GET $ serveText $ repo </> "HEAD") ]

------------------------------------------------------------------------

serveText :: MonadSnap m => FilePath -> m ()
serveText = serveFileAs "text/plain"

rpc :: MonadSnap m => ByteString -> FilePath -> m ()
rpc service repo = do
    contentType' ["application/x-git-", service, "-result"]
    git repo [service, "--stateless-rpc", "."]

infoRefs :: MonadSnap m => FilePath -> m ()
infoRefs repo = do
    service <- getService

    cacheDisabled
    contentType' ["application/x-git-", service, "-advertisement"]

    writeBS $ pktWrite' ["# service=git-", service, "\n"]
    writeBS pktFlush

    git repo [service, "--stateless-rpc", "--advertise-refs", "."]

------------------------------------------------------------------------

git :: MonadSnap m => FilePath -> [ByteString] -> m ()
git repo args = do
    g <- process repo "git" (map B.unpack args)
    decompressRequestBody (stdin g)
    addToOutputBS (stdout g)

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
-- Process creation/interaction

bufferSize :: Int
bufferSize = 4 * 1024

data Process = Process
    { stdin  :: MonadIO m => Iteratee ByteString m ()
    , stdout :: MonadIO m => Enumerator ByteString m a
    }

process :: MonadIO m
        => FilePath  -- ^ working directory
        -> FilePath  -- ^ executable to run
        -> [String]  -- ^ any arguments
        -> m Process
process dir exe args = do

    (Just inH, Just outH, Just errH, pid) <-
        liftIO $ createProcess (proc exe args)
            { cwd = Just dir
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe }

    errM <- liftIO $ forkGetContents errH

    let output :: MonadIO m => Enumerator ByteString m a
        output (Continue k) = do
            bs <- liftIO $ B.hGet outH bufferSize
            if B.null bs
                then do
                    err <- liftIO $ takeMVar errM
                    ex  <- liftIO $ waitForProcess pid
                    case ex of
                        ExitSuccess -> continue k
                        ExitFailure _ -> error $ B.unpack err
                else do
                    k (Chunks [bs]) >>== output
        output step = returnI step

        input :: MonadIO m => Iteratee ByteString m ()
        input = do
            mbs <- head
            case mbs of
                Nothing -> do
                    liftIO $ hClose inH
                Just bs -> do
                    liftIO $ B.hPut inH bs
                    input

    return $ Process input output

forkGetContents :: Handle -> IO (MVar ByteString)
forkGetContents h = do
    m <- newEmptyMVar
    forkIO $ do
        bs <- B.hGetContents h
        putMVar m bs
    return m
