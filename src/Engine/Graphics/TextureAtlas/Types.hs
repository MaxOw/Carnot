{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language StrictData #-}
{-# Language BangPatterns #-}
module Engine.Graphics.TextureAtlas.Types where

import Delude
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Control.Concurrent.STM.TChan (TChan)
import Graphics.GL (GLint)

import Engine.Backend.Types
import Engine.Graphics.TaskManager (TaskManager)

--------------------------------------------------------------------------------

data QuadName = TL | TR | BL | BR deriving (Show)

data QuadTree a
   = QuadNode
     { topLeft  :: ~(QuadTree a)
     , topRight :: ~(QuadTree a)
     , botLeft  :: ~(QuadTree a)
     , botRight :: ~(QuadTree a)
     }
   | QuadLeaf a
   | QuadEmpty

data SlotSize
   = SlotSize_8   | SlotSize_16  | SlotSize_32   | SlotSize_64   | SlotSize_128
   | SlotSize_256 | SlotSize_512 | SlotSize_1024 | SlotSize_2048 | SlotSize_4096
   deriving (Eq, Ord, Show, Enum, Bounded)

data AtlasLayout a = AtlasLayout
   { field_topSlotSize :: SlotSize
   , field_quadTree    :: QuadTree a
   } deriving (Generic)

type QuadPath = Seq QuadName

--------------------------------------------------------------------------------

data AtlasPage = AtlasPage
   { field_buffer :: TextureBuffer
   , field_slots  :: AtlasLayout ()
   } deriving (Generic)

type AtlasPages = Vector AtlasPage

data AtlasLocation = AtlasLocation
   { field_page      :: Int
   , field_offset    :: V2 Int
   , field_size      :: V2 Int
   -- , field_texCoords :: Rect Float -- [V2 Float]
   } deriving (Generic, Show)
instance HasSize AtlasLocation (V2 Int)

emptyAtlasLocation :: AtlasLocation
emptyAtlasLocation = AtlasLocation
    { field_page      = 0
    , field_offset    = 0
    , field_size      = 0
    -- , field_texCoords = unitRect
    }

data AtlasTask
   = TaskAddTexture AtlasLocation TextureBuffer

data TextureAtlas  = TextureAtlas
   { field_maxTextureUnits :: GLint
   , field_maxTextureSize  :: GLint
   , field_atlasMap        :: IORef (HashMap Texture AtlasLocation)
   , field_primaryPages    :: IORef AtlasPages
   -- , field_secondaryPages  :: IORef AtlasPages
   , field_customPages     :: IORef (Vector TextureBuffer)
   , field_tasks           :: TChan AtlasTask
   , field_taskManager     :: TaskManager
   } deriving (Generic)

newtype PageId = PageId { unPageId :: Int }
makeWrapped ''PageId

--------------------------------------------------------------------------------

