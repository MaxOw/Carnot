module Engine.Graphics.DrawBatchCache.Types where

import Relude
import Engine.Graphics.Buffer.Types (DrawBatch)

data DrawBatchCache = DrawBatchCache
   { field_lastFrameCache :: IORef (IntMap DrawBatch)
   , field_currentCache   :: IORef (IntMap DrawBatch)
   } deriving (Generic)

