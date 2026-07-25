-- | In-process rendezvous between the submit handler (which enqueues a
-- command and waits) and the agent result endpoint (which resolves it).
--
-- The database is the source of truth for command state; this map only
-- provides the wakeup so a waiting HTTP handler learns about the result
-- without polling. Both sides run in the same process (Warp handlers),
-- so an STM 'TMVar' per command id suffices.
module Agent.CommandQueue
  ( CommandWaiters
  , newCommandWaiters
  , awaitCommand
  , resolveCommand
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hydra.Submit (SubmitResult)
import System.Timeout (timeout)

newtype CommandWaiters = CommandWaiters (TVar (Map Text (TMVar SubmitResult)))

newCommandWaiters :: IO CommandWaiters
newCommandWaiters = CommandWaiters <$> newTVarIO Map.empty

-- | Register interest in a command's result and block until it is
-- resolved or the timeout (seconds) elapses. The waiter entry is
-- removed on the way out either way.
awaitCommand :: CommandWaiters -> Text -> Int -> IO (Maybe SubmitResult)
awaitCommand (CommandWaiters var) cmdId timeoutSeconds = do
  slot <- newEmptyTMVarIO
  atomically $ modifyTVar' var (Map.insert cmdId slot)
  result <- timeout (timeoutSeconds * 1_000_000) (atomically $ takeTMVar slot)
  atomically $ modifyTVar' var (Map.delete cmdId)
  pure result

-- | Deliver a result to a waiting caller. Returns 'False' when nobody
-- is waiting (command already timed out, or resolved twice) — callers
-- still persist the result to the database regardless.
resolveCommand :: CommandWaiters -> Text -> SubmitResult -> IO Bool
resolveCommand (CommandWaiters var) cmdId result = do
  mSlot <- Map.lookup cmdId <$> readTVarIO var
  case mSlot of
    Nothing -> pure False
    Just slot -> atomically $ tryPutTMVar slot result
