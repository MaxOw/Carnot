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
   { scrollerConfig_bufferMargin     :: ScrollerMargin
   , scrollerConfig_initialPosition  :: V2 Float
   } deriving (Generic)
makeFieldsCustom ''ScrollerConfig
instance Default ScrollerConfig where
    def = ScrollerConfig
        { scrollerConfig_bufferMargin    = ScrollerMargin_Percent 0.2
        , scrollerConfig_initialPosition = 0
        }

data ScrollerState = ScrollerState
   { scrollerState_bufferMargin      :: ScrollerMargin
   , scrollerState_sourceBuffer      :: TextureBuffer
   , scrollerState_targetBuffer      :: TextureBuffer
   , scrollerState_atlasCustomPageId :: PageId
   , scrollerState_drawScale         :: Float
   , scrollerState_position          :: V2 Float
   , scrollerState_size              :: Size Float
   , scrollerState_valid             :: Bool
   }
makeFieldsCustom ''ScrollerState

newtype Scroller = Scroller { unScroller :: IORef ScrollerState }
makeWrapped ''Scroller
