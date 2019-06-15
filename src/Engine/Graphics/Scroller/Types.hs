{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
module Engine.Graphics.Scroller.Types where

import Delude
import Engine.Backend.Types (TextureBuffer)
import Engine.Common.Types (Size)
import Engine.Graphics.Types (DrawBatch)
import Engine.Graphics.TextureAtlas.Types (PageId)
import Control.Concurrent.Async (Async)

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
   , field_drawPosition      :: V2 Float
   , field_position          :: V2 Float
   , field_size              :: Size Float
   , field_valid             :: Bool
   , field_asyncBatch        :: Maybe (Async DrawBatch)
   } deriving (Generic)
instance HasSize ScrollerState (Size Float)

newtype Scroller = Scroller { unScroller :: IORef ScrollerState }
makeWrapped ''Scroller
