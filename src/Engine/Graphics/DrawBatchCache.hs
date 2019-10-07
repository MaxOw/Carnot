module Engine.Graphics.DrawBatchCache
    ( DrawBatchCache
    , new
    , retriveOrAdd
    , endFrame
    ) where

import Delude
import qualified Data.IntMap as IntMap
import Engine.Lens.Utils
import Engine.Graphics.DrawBatchCache.Types
import Engine.Graphics.Types

new :: MonadIO m => m DrawBatchCache
new = do
    lfc <- newIORef mempty
    cfc <- newIORef mempty
    return $ DrawBatchCache
        { field_lastFrameCache = lfc
        , field_currentCache   = cfc
        }

retriveOrAdd :: MonadIO m
    => DrawBatchCache -> Int -> a -> (a -> m DrawBatch) -> m DrawBatch
retriveOrAdd c key val toBatch = do
    oc <- readIORef $ c^.ff#lastFrameCache
    case IntMap.lookup key oc of
        Nothing -> addBatch =<< toBatch val
        Just bb -> addBatch bb
    where
    addBatch b
        = atomicModifyIORef' (c^.ff#currentCache) $ (,b)
        . IntMap.insert key b

endFrame :: MonadIO m => DrawBatchCache -> m ()
endFrame c = do
    writeIORef (c^.ff#lastFrameCache) =<< readIORef (c^.ff#currentCache)
    writeIORef (c^.ff#currentCache) mempty
