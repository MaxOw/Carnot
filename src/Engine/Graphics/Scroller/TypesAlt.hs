module Engine.Graphics.Scroller.TypesAlt where

import Delude
import Engine.Backend.Types (TextureBuffer)
import Engine.Common.Types (Size)
import Engine.Graphics.TextureAtlas.Types (PageId)
import Engine.Lens.Utils

--------------------------------------------------------------------------------

data ScrollerConfig = ScrollerConfig
   { field_scale :: Float
   } deriving (Generic)
instance Default ScrollerConfig

data ScrollerState = ScrollerState
   { field_frontBuffer       :: TextureBuffer
   , field_backBuffer        :: TextureBuffer
   , field_valid             :: Bool
   , field_position          :: V2 Float
   , field_config            :: ScrollerConfig
   , field_atlasCustomPageId :: PageId
   } deriving (Generic)

newtype Scroller = Scroller (IORef ScrollerState) deriving (Generic)
instance Wrapped Scroller
instance Rewrapped Scroller Scroller

--------------------------------------------------------------------------------

frontBuffer :: Lens' ScrollerState TextureBuffer
frontBuffer = ff#frontBuffer

backBuffer :: Lens' ScrollerState TextureBuffer
backBuffer = ff#backBuffer

config :: Lens' ScrollerState ScrollerConfig
config = ff#config

{-
-- Pixels on screen
newtype Pixels = Pixels Int32 deriving (Generic)
instance Wrapped Pixels
instance Rewrapped Pixels Pixels

-- Units in game space
newtype Units = Units Float deriving (Generic)
instance Wrapped Units
instance Rewrapped Units Units
-}

