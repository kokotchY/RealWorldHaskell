module NiceFork (
    ThreadManager
    , newManager
    , forManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception
import Control.Exception.IOException
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished
                  | Threw Exception
                  deriving (Eq, Show)

newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            result <- try body
            putMVar state (either Threw (const Finished) result)
        return (M.insert tid state m, tid)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus = undefined

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor = undefined

waitAll :: ThreadManager -> IO ()
waitAll = undefined

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)
