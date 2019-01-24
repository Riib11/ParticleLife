module NiceFork
( ThreadManager, newManager
, forkManaged,
, getStatus
, waitFor
, waitAll
) where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (Exception,  try)
import qualified Data.Map as M

data ThreadStatus
  = Running
  | Finished
  | Threw Exception
  deriving (Eq, Show)

newtype ThreadManager =
  Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving (Eq)

newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid   <- forkIO $ do
      result <- try body
      putMVar state (either Threw (const Finished) result)


getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

waitFor :: ThreadManager -> IO ()


