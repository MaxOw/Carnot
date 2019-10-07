module Engine.Graphics.TextureCache.Types where

import Delude
import Engine.Backend.Types
import Engine.Graphics.TextureAtlas.Types
import Engine.Graphics.TaskManager (TaskManager)

data TextureCache = TextureCache
   { field_atlas       :: TextureAtlas
   , field_cache       :: IORef (IntMap Texture)
   , field_taskManager :: TaskManager
   } deriving (Generic)

