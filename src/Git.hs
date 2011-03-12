module Git
    ( gitExec
    , gitExec_
    ) where

import           Control.Concurrent
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           System.Exit
import           System.IO
import           System.Process

------------------------------------------------------------------------

-- | Runs a git command on a repository and returns the stdout
gitExec :: FilePath -> [ByteString] -> IO ByteString
gitExec repo args = do
    result <- process repo "git" args
    case result of
      (ExitSuccess, out, _)        -> return out
      (ExitFailure code, out, err) -> error (errMsg code out err)
  where
    errMsg = formatErrorMsg $ "git " ++ (unwords $ map B.unpack args)

gitExec_ :: FilePath -> [ByteString] -> IO ()
gitExec_ repo args = gitExec repo args >> return ()

------------------------------------------------------------------------

-- | Formats and error message based on the exit code, stdout and stderr
-- from a process execution
formatErrorMsg :: String -> Int -> B.ByteString -> B.ByteString -> String
formatErrorMsg cmd code out err = concat
    [ "$ ", cmd, "\n"
    , B.unpack out
    , B.unpack err
    , "(exit code was ", show code, ")" ]

------------------------------------------------------------------------
-- Process interaction

-- | Exit code, stdout, stderr
type ProcessOutput = (ExitCode, ByteString, ByteString)

-- | Runs a process and reads the output
process :: FilePath     -- ^ working directory
        -> FilePath     -- ^ command to run
        -> [ByteString] -- ^ any arguments
        -> IO ProcessOutput
process wd cmd args = do
    (Just inH, Just outH, Just errH, pid) <-
        createProcess (proc cmd $ map B.unpack args)
            { cwd = Just wd
            , std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe }

    outM <- forkGetContents outH
    errM <- forkGetContents errH
    hClose inH

    ex <- waitForProcess pid
    out <- readMVar outM
    err <- readMVar errM

    return (ex, out, err)

forkGetContents :: Handle -> IO (MVar ByteString)
forkGetContents h = do
    m <- newEmptyMVar
    forkIO $ do
        bs <- B.hGetContents h
        putMVar m bs
    return m
