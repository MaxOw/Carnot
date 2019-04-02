{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
module Engine.Graphics.Scroller.Types where

import Delude
import Engine.Backend.Types (TextureBuffer)
import Engine.Common.Types (Size)
import Engine.Graphics.TextureAtlas.Types (PageId)

data ScrollerMargin
   = ScrollerMargin_Percent Float
   | ScrollerMargin_Pixels  Int

data ScrollerConfig = ScrollerConfig
   { field_bufferMargin     :: ScrollerMargin
   , field_initialPosition  :: V2 Float
   } deriving (Generic)
instance Default ScrollerConfig where
    def = ScrollerConfig
        { field_bufferMargin    = ScrollerMargin_Percent 0.2
        , field_initialPosition = 0
        }

data ScrollerState = ScrollerState
   { field_bufferMargin      :: ScrollerMargin
   , field_sourceBuffer      :: TextureBuffer
   , field_targetBuffer      :: TextureBuffer
   , field_atlasCustomPageId :: PageId
   , field_drawScale         :: Float
   , field_position          :: V2 Float
   , field_size              :: Size Float
   , field_valid             :: Bool
   } deriving (Generic)

newtype Scroller = Scroller { unScroller :: IORef ScrollerState }
makeWrapped ''Scroller
