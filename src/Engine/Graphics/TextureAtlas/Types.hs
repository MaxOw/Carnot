{-# Language TemplateHaskell #-}
{-# Language StrictData #-}
{-# Language BangPatterns #-}
module Engine.Graphics.TextureAtlas.Types where

import Delude
import Linear
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Control.Concurrent.STM.TChan (TChan)
import Graphics.GL (GLint)

import Engine.Backend.Types

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
   { atlasLayout_topSlotSize :: SlotSize
   , atlasLayout_quadTree    :: QuadTree a
   }
makeFieldsCustom ''AtlasLayout

type QuadPath = Seq QuadName

--------------------------------------------------------------------------------

data AtlasPage = AtlasPage
   { atlasPage_buffer :: TextureBuffer
   , atlasPage_slots  :: AtlasLayout ()
   }
makeFieldsCustom ''AtlasPage

type AtlasPages = Vector AtlasPage

data AtlasLocation = AtlasLocation
   { atlasLocation_page      :: Int
   , atlasLocation_offset    :: V2 Int
   , atlasLocation_size      :: V2 Int
   , atlasLocation_texCoords :: [V2 Float]
   } deriving (Show)
makeFieldsCustom ''AtlasLocation

emptyAtlasLocation :: AtlasLocation
emptyAtlasLocation = AtlasLocation
    { atlasLocation_page      = 0
    , atlasLocation_offset    = 0
    , atlasLocation_size      = 0
    , atlasLocation_texCoords = []
    }

data AtlasTask
   = TaskAddTexture AtlasLocation TextureBuffer

data TextureAtlas  = TextureAtlas
   { textureAtlas_maxTextureUnits :: GLint
   , textureAtlas_maxTextureSize  :: GLint
   , textureAtlas_atlasMap        :: IORef (HashMap Texture AtlasLocation)
   , textureAtlas_primaryPages    :: IORef AtlasPages
   , textureAtlas_secondaryPages  :: IORef AtlasPages
   , textureAtlas_tasks           :: TChan AtlasTask
   }
makeFieldsCustom ''TextureAtlas

--------------------------------------------------------------------------------

