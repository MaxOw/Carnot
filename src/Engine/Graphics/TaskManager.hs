module Engine.Graphics.TaskManager
    ( TaskManager

    , new
    , addTask, addSyncTask
    , allDone
    , performOne, performUntil
    ) where

import Relude
import Control.Concurrent.STM.TQueue
import qualified Control.Concurrent.Async as Async

--------------------------------------------------------------------------------

newtype Task = Task { syncTask :: IO () }
newtype TaskManager = TaskManager { field_tasks :: TQueue Task }

--------------------------------------------------------------------------------

new :: MonadIO m => m TaskManager
new = TaskManager <$> liftIO newTQueueIO

addTask :: (MonadIO m, NFData a) => TaskManager -> IO a -> (a -> IO ()) -> m ()
addTask tm a s = do
    aa <- liftIO $ Async.async (evaluateNF =<< a)
    let t = Task (s =<< Async.wait aa)
    atomically $ writeTQueue (field_tasks tm) t

addSyncTask :: MonadIO m => TaskManager -> IO () -> m ()
addSyncTask tm = atomically . writeTQueue (field_tasks tm) . Task

allDone :: MonadIO m => TaskManager -> m Bool
allDone tm = atomically $ isEmptyTQueue (field_tasks tm)

performOne :: MonadIO m => TaskManager -> m ()
performOne tm = whenJustM
    (atomically . tryReadTQueue $ field_tasks tm)
    (liftIO . syncTask)

performUntil :: MonadIO m => TaskManager -> m Bool -> m ()
performUntil tm timesOut = go
    -- where go = unlessM (orM [timesOut, allDone tm]) (performOne tm >> go)
    where go = unlessM (allDone tm) (performOne tm >> unlessM timesOut go)

