{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mothership.Util
    ( Process (..)
    , process
    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Snap.Iteratee hiding (map)
import           System.Exit
import           System.IO
import           System.Process
import           Prelude hiding (catch, head)

------------------------------------------------------------------------

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

------------------------------------------------------------------------

forkGetContents :: Handle -> IO (MVar ByteString)
forkGetContents h = do
    m <- newEmptyMVar
    forkIO $ do
        bs <- B.hGetContents h
        putMVar m bs
    return m
