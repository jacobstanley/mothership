{-# LANGUAGE OverloadedStrings #-}

module GitSnap.Util
    ( cmd
    , cmd'
    , cmdI

    , ungzip
    ) where

import           Codec.Zlib
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           Snap.Iteratee
import           System.Exit
import           System.IO
import           System.Process
import           Prelude hiding (catch, head)

------------------------------------------------------------------------

-- | Runs a process and reads the output
cmd :: FilePath                              -- ^ working directory
    -> FilePath                              -- ^ executable to run
    -> [String]                              -- ^ any arguments
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
cmd dir exe args = cmd' dir exe args ""

-- | Runs a process and reads the output (allows for providing stdin)
cmd' :: FilePath                              -- ^ working directory
     -> FilePath                              -- ^ executable to run
     -> [String]                              -- ^ any arguments
     -> L.ByteString                          -- ^ standard input
     -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
cmd' dir exe args input = do

    (Just inH, Just outH, Just errH, pid) <-
        createProcess (proc exe args)
            { cwd = Just dir
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe }

    outM <- forkGetContents outH
    errM <- forkGetContents errH

    unless (L.null input) $ do L.hPut inH input; hFlush inH
    hClose inH

    out <- takeMVar outM
    err <- takeMVar errM
    ex  <- waitForProcess pid

    return (ex, out, err)

------------------------------------------------------------------------

cmdI :: MonadIO m
     => FilePath  -- ^ working directory
     -> FilePath  -- ^ executable to run
     -> [String]  -- ^ any arguments
     -> Iteratee ByteString m (ExitCode, ByteString, ByteString)
cmdI dir exe args = do

    (Just inH, Just outH, Just errH, pid) <-
        liftIO $ createProcess (proc exe args)
            { cwd = Just dir
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe }

    outM <- liftIO $ forkGetContents outH
    errM <- liftIO $ forkGetContents errH

    let loop = do
        mbs <- head
        case mbs of
            Nothing -> liftIO $ do
                hClose inH
                out <- takeMVar outM
                err <- takeMVar errM
                ex  <- waitForProcess pid
                return (ex, out, err)
            Just bs -> do
                liftIO $ B.putStr bs
                liftIO $ B.hPut inH bs
                loop

    loop

------------------------------------------------------------------------

forkGetContents :: Handle -> IO (MVar ByteString)
forkGetContents h = do
    m <- newEmptyMVar
    forkIO $ do
        bs <- B.hGetContents h
        putMVar m bs
    return m

------------------------------------------------------------------------

-- Stolen from http-enumerator (Network.HTTP.Enumerator.Zlib)
ungzip :: MonadIO m => Enumeratee B.ByteString B.ByteString m b
ungzip inner = do
    fzstr <- liftIO $ initInflate $ WindowBits 31
    ungzip' fzstr inner

ungzip' :: MonadIO m => Inflate -> Enumeratee B.ByteString B.ByteString m b
ungzip' fzstr (Continue k) = do
    x <- head
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
