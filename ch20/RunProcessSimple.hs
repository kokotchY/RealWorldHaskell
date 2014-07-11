{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE DatatypeContexts #-}

module RunProcessSimple where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.Process
import System.IO
import System.Exit
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
{-import Text.Regex-}

type SysCommnd = (String, [String])

data CommandResult = CommandResult {
    cmdOutput :: IO String,
    getExitStatus :: IO ProcessStatus
}

type CloseFDs = MVar [Fd]

class CommandLike a where
    invoke :: a -> CloseFDs -> String -> IO CommandResult

instance CommandLike SysCommnd where
    invoke (cmd, args) closefds input = do
        (stdinread, stdinwrite) <- createPipe
        (stdoutread, stdoutwrite) <- createPipe
        addCloseFDs closefds [stdinwrite, stdoutread]
        childPID <- withMVar closefds (\fds -> forkProcess (child fds stdinread stdoutwrite))
        closeFd stdinread
        closeFd stdoutwrite
        stdinhdl <- fdToHandle stdinwrite
        forkIO $ do
            hPutStr stdinhdl input
            hClose stdinhdl
        stdouthdl <- fdToHandle stdoutread
        let waitfunc = do
            status <- getProcessStatus True False childPID
            case status of
                Nothing -> fail $ "Error: Nothing from getProcessStatus"
                Just ps -> do
                    removeCloseFds closefds [stdinwrite, stdoutread]
                    return ps
        return $ CommandResult { cmdOutput = hGetContents stdouthdl, getExitStatus = waitfunc }
        where
            child closefds stdinread stdoutwrite = do
                dupTo stdinread stdInput
                dupTo stdoutwrite stdOutput
                closeFd stdinread
                closeFd stdoutwrite
                mapM_ (\fd -> catch (closeFd fd) (\(SomeException _) -> return ())) closefds
                executeFile cmd True args Nothing

addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds = modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

removeCloseFds :: CloseFDs -> [Fd] -> IO ()
removeCloseFds closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)
    where
        procfdlist fdlist [] = fdlist
        procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs
        removefd [] _ = []
        removefd (x:xs) fd
            | fd == x = xs
            | otherwise = x : removefd xs fd

data (CommandLike src, CommandLike dest) =>
    PipeCommand src dest = PipeCommand src dest

(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

instance (CommandLike a, CommandLike b) => CommandLike (PipeComman:q
