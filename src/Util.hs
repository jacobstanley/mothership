{-# LANGUAGE OverloadedStrings #-}

module Util
    ( run
    , run'
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           System.Exit
import           System.IO
import           System.Process
import           Prelude hiding (catch)

------------------------------------------------------------------------

-- | Runs a process and reads the output
run :: FilePath                              -- ^ working directory
    -> FilePath                              -- ^ command to run
    -> [String]                              -- ^ any arguments
    -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
run wd cmd args = run' wd cmd args ""

-- | Runs a process and reads the output (allows for providing stdin)
run' :: FilePath                              -- ^ working directory
     -> FilePath                              -- ^ command to run
     -> [String]                              -- ^ any arguments
     -> L.ByteString                          -- ^ standard input
     -> IO (ExitCode, ByteString, ByteString) -- ^ exitcode, stdout, stderr
run' wd cmd args input = do

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args)
            { cwd = Just wd
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe }

    outMVar <- newEmptyMVar

    outM <- newEmptyMVar
    errM <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    forkIO $ do
        out <- B.hGetContents outh
        putMVar outM out
        putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    forkIO $ do
        err  <- B.hGetContents errh
        putMVar errM err
        putMVar outMVar ()

    -- now write and flush any input
    unless (L.null input) $ do L.hPut inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    out <- readMVar outM
    err <- readMVar errM

    return (ex, out, err)
