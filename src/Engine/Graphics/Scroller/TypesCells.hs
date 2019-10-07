module Engine.Graphics.Scroller.TypesCells where

import Delude
import Engine.Backend.Types (TextureBuffer)
import Engine.Common.Types (Size)
import Engine.Graphics.TextureAtlas.Types (PageId)
import Engine.Lens.Utils

data ScrollerConfig = ScrollerConfig
   { field_cellSize :: Int32
   , field_scale    :: Float
   } deriving (Generic)
instance Default ScrollerConfig where
    def = ScrollerConfig
        { field_cellSize = 512
        , field_scale    = 1
        }

data ScrollerCell = ScrollerCell
   { field_cellPosition :: V2 Int
   , field_bufferRegion :: V2 Int32
   } deriving (Show, Generic)

data ScrollerState = ScrollerState
   { field_buffer     :: TextureBuffer
   , field_freedCells :: [ScrollerCell]
   , field_validCells :: [ScrollerCell]
   , field_position   :: V2 Float
   , field_config     :: ScrollerConfig
   , field_atlasPid   :: PageId
   } deriving (Generic)

config :: Lens' ScrollerState ScrollerConfig
config = ff#config

newtype Scroller = Scroller (IORef ScrollerState) deriving (Generic)
instance Wrapped Scroller
instance Rewrapped Scroller Scroller

