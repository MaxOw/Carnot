{-# Language TemplateHaskell #-}
{-# Language StrictData      #-}
{-# Language PatternSynonyms #-}
{-# Language TypeFamilies #-}
module Engine.Graphics.Types
    ( module Engine.Graphics.Types
    ) where

import Delude
import Engine.Context (Context)
import Linear (M44, V2, V3, V4)
import Data.Colour (Colour, AlphaColour)
import qualified Data.Colour as Color
import qualified Data.Colour.Names as Color

import Diagrams.Transform
import Diagrams.Core.V

import Engine.Backend.Types as Engine.Graphics.Types
import Engine.Graphics.TextureAtlas.Types
import Engine.FontsManager.Types
import Engine.Common.Types

-- import Engine.Graphics.Utils

--------------------------------------------------------------------------------

type Vec2 = V2 Double
type Vec3 = V3 Double
type Vec4 = V4 Double
type Mat4 = M44 Double

type Color = Colour Float
type AlphaColor = AlphaColour Float

data SimpleShape
   = SimpleSquare
   | SimpleCircle
   deriving (Show, Generic)
instance NFData SimpleShape

data ShapeDesc = ShapeDesc
   { shapeDesc_shapeType      :: SimpleShape
   , shapeDesc_color          :: AlphaColor
   , shapeDesc_borderColor    :: AlphaColor
   , shapeDesc_modelTransform :: T2D
   } deriving (Generic)
makeFieldsCustom ''ShapeDesc
instance Default ShapeDesc where
    def = ShapeDesc
        { shapeDesc_shapeType      = SimpleCircle
        , shapeDesc_color          = Color.opaque Color.white
        , shapeDesc_borderColor    = Color.opaque Color.black
        , shapeDesc_modelTransform = mempty -- Linear.identity
        }

data TextureDesc = TextureDesc
   { textureDesc_textureId      :: Texture
   , textureDesc_modelTransform :: T2D
   } deriving (Generic)
makeFieldsCustom ''TextureDesc
instance Default TextureDesc where
    def = TextureDesc
        { textureDesc_textureId      = 0 -- noTexture
        , textureDesc_modelTransform = mempty
        }

data AtlasDesc = AtlasDesc
   { atlasDesc_textureId      :: Texture
   , atlasDesc_color          :: AlphaColor
   , atlasDesc_modelTransform :: T2D
   } deriving (Generic)
makeFieldsCustom ''AtlasDesc
instance Default AtlasDesc where
    def = AtlasDesc
        { atlasDesc_textureId      = 0 -- noTexture
        , atlasDesc_color          = Color.opaque Color.black
        , atlasDesc_modelTransform = mempty
        }

type ShapesBatch   = [ShapeDesc]
type TexturesBatch = [TextureDesc]
type AtlasBatch    = [AtlasDesc]

data DrawRequest = DrawRequest
   { _requestedShapes   :: ShapesBatch
   , _requestedAtlas    :: AtlasBatch
   , _requestedTextures :: TexturesBatch
-- , _requestedPolygons :: PolygonsBatch
   } deriving (Generic) -- , NFData)
instance Default DrawRequest
makeLenses ''DrawRequest

type DrawBatch batch = Mat4 -> batch -> IO ()
type DrawProcedure = Mat4 -> DrawRequest -> IO ()
data GraphicsState = GraphicsState
   { _context            :: Context
   , _drawProcedure      :: DrawProcedure
   , _textureAtlas       :: TextureAtlas
   , _fontsManager       :: FontsManager
   }
makeLenses ''GraphicsState

data RenderAction
   = RenderShape     ShapeDesc
   | RenderFromAtlas AtlasDesc
   | RenderTexture   TextureDesc
   | RenderComposition T2D [RenderAction]

instance Semigroup RenderAction where
    a <> b  = RenderComposition mempty [a, b]
    sconcat = RenderComposition mempty . toList
    stimes  = stimesIdempotent
instance Monoid RenderAction where
    mempty  = RenderComposition mempty []
    mconcat = RenderComposition mempty

type instance V RenderAction = V2
type instance N RenderAction = Double

instance Transformable RenderAction where
    transform t = \case
        RenderShape     d -> RenderShape     $ over modelTransform (t <>) d
        RenderFromAtlas d -> RenderFromAtlas $ over modelTransform (t <>) d
        RenderTexture   d -> RenderTexture   $ over modelTransform (t <>) d
        RenderComposition t0 ds -> RenderComposition (t <> t0) ds

data DrawCharStep = DrawCharStep
   { drawCharStep_texture        :: Texture
   , drawCharStep_color          :: AlphaColor
   , drawCharStep_modelTransform :: T2D
   , drawCharStep_advance        :: Int
   }
makeFieldsCustom ''DrawCharStep

-- pattern RenderNothing :: RenderAction
-- pattern RenderNothing = RenderComposition m []

data RenderTextLayout = RenderTextLayout
   { renderTextLayout_size         :: Size AbsoluteSize
   , renderTextLayout_renderAction :: RenderAction
   }
makeFieldsCustom ''RenderTextLayout

