{-# Language TemplateHaskell #-}
-- {-# Language StrictData      #-}
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
-- import Data.Vector.Mutable (IOVector)

import Diagrams.Transform
import Diagrams.Core.V

import Engine.Backend.Types as Engine.Graphics.Types
import Engine.Graphics.TextureAtlas.Types
import Engine.FontsManager.Types
import Engine.Common.Types

--------------------------------------------------------------------------------

type Vec2 = V2 Float
type Vec3 = V3 Float
type Vec4 = V4 Float
type Mat4 = M44 Float

type Color = Colour Float
type AlphaColor = AlphaColour Float


--------------------------------------------------------------------------------

data Img = Img
   { img_texture  :: Texture
   , img_size     :: Size Int
   , img_part     :: Maybe (Rect Int)
   , img_zindex   :: Word32
   , img_color    :: AlphaColor
   , img_colorMix :: Float
   }
makeFieldsCustom ''Img
instance Default Img where
    def = Img
        { img_texture  = noTexture
        , img_size     = pure 0
        , img_part     = Nothing -- unitRect
        , img_zindex   = 0
        , img_color    = Color.transparent
        , img_colorMix = 0
        }

mkImg :: Texture -> Size Int -> Img
mkImg t s = set texture t $ set size s def

--------------------------------------------------------------------------------

data SimpleShape
   = SimpleSquare
   | SimpleCircle
   deriving (Show, Eq, Generic)
instance NFData SimpleShape

data ShapeDesc = ShapeDesc
   { shapeDesc_shapeType      :: SimpleShape
   , shapeDesc_color          :: AlphaColor
   , shapeDesc_modelTransform :: T2D
   , shapeDesc_zindex         :: Word32
   } deriving (Generic)
makeFieldsCustom ''ShapeDesc
instance Default ShapeDesc where
    def = ShapeDesc
        { shapeDesc_shapeType      = SimpleCircle
        , shapeDesc_color          = Color.opaque Color.white
        , shapeDesc_modelTransform = mempty -- Linear.identity
        , shapeDesc_zindex         = 0
        }
type instance N ShapeDesc = Float
type instance V ShapeDesc = V2
instance Transformable ShapeDesc where
    transform t = over modelTransform (t <>)

--------------------------------------------------------------------------------

{-
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
type instance N TextureDesc = Float
type instance V TextureDesc = V2
instance Transformable TextureDesc where
    transform t = over modelTransform (t <>)
-}

class BoundingPoints a where
    boundingPoints :: a -> [V2 Float]

data AtlasDesc = AtlasDesc
   { atlasDesc_textureId      :: Maybe Texture
   , atlasDesc_color          :: AlphaColor
   , atlasDesc_modelTransform :: T2D
   , atlasDesc_radius         :: Float
   , atlasDesc_colorMix       :: Float
   , atlasDesc_zindex         :: Word32
   , atlasDesc_part           :: Maybe (Rect Int)
   , atlasDesc_customPage     :: Maybe (PageId, Rect Float)
   } deriving (Generic)
makeFieldsCustom ''AtlasDesc
instance Default AtlasDesc where
    def = AtlasDesc
        { atlasDesc_textureId      = Nothing -- 0 -- noTexture
        , atlasDesc_color          = Color.opaque Color.black
        , atlasDesc_modelTransform = mempty
        , atlasDesc_radius         = 2
        , atlasDesc_colorMix       = 1
        , atlasDesc_zindex         = 0
        , atlasDesc_part           = Nothing -- unitRect
        , atlasDesc_customPage     = Nothing
        }
type instance N AtlasDesc = Float
type instance V AtlasDesc = V2
instance Transformable AtlasDesc where
    transform t = over modelTransform (t <>)

unitBound :: Fractional a => [V2 a]
unitBound = rectToList $ Rect (pure (-0.5)) (pure 1)

instance BoundingPoints AtlasDesc where
    boundingPoints d = transform (d^.modelTransform) unitBound

--------------------------------------------------------------------------------

data SimpleTextDesc = SimpleTextDesc
   { simpleTextDesc_modelTransform :: T2D
   , simpleTextDesc_zindex         :: Word32
   , simpleTextDesc_fontName       :: Maybe FontName
   , simpleTextDesc_fontSize       :: Maybe FontSize
   , simpleTextDesc_color          :: AlphaColor
   , simpleTextDesc_boxAlign       :: BoxAlign
   } deriving (Generic)
makeFieldsCustom ''SimpleTextDesc
type instance N SimpleTextDesc = Float
type instance V SimpleTextDesc = V2
instance Transformable SimpleTextDesc where
    transform t = over modelTransform (t <>)
instance Default SimpleTextDesc where
    def = SimpleTextDesc
        { simpleTextDesc_modelTransform = mempty
        , simpleTextDesc_zindex         = 0
        , simpleTextDesc_fontName       = Nothing
        , simpleTextDesc_fontSize       = Nothing
        , simpleTextDesc_color          = Color.opaque Color.black
        , simpleTextDesc_boxAlign       = Center
        }

--------------------------------------------------------------------------------

-- type TexturesBatch = Seq TextureDesc
type AtlasBatch    = Seq AtlasDesc

data DrawRequest = DrawRequest
   { _requestedAtlas      :: AtlasBatch
   -- , _requestedTextures :: TexturesBatch
   } deriving (Generic)
instance Default DrawRequest
makeLenses ''DrawRequest

type DrawBatch batch = Mat4 -> batch -> IO ()
type DrawProcedure = Mat4 -> DrawRequest -> IO ()
data GraphicsState = GraphicsState
   { _context            :: Context
   , _drawProcedure      :: DrawProcedure
   , _textureAtlas       :: TextureAtlas
   , _fontsManager       :: FontsManager
   , _defaultFontStyle   :: Maybe FontStyle
   }
makeLenses ''GraphicsState

--------------------------------------------------------------------------------

data RenderAction
   = RenderFromAtlas AtlasDesc
   | RenderComposition T2D [RenderAction]
   | RenderSimpleText SimpleTextDesc Text

instance Semigroup RenderAction where
    a <> b  = RenderComposition mempty [a, b]
    sconcat = RenderComposition mempty . toList
    stimes  = stimesIdempotent
instance Monoid RenderAction where
    mempty  = RenderComposition mempty []
    mconcat = RenderComposition mempty

type instance V RenderAction = V2
type instance N RenderAction = Float

instance Transformable RenderAction where
    transform t = \case
        RenderFromAtlas d -> RenderFromAtlas $ over modelTransform (t <>) d
        RenderComposition t0 ds -> RenderComposition (t <> t0) ds
        RenderSimpleText  ds tx -> RenderSimpleText (transform t ds) tx

instance BoundingPoints RenderAction where
    boundingPoints = \case
        RenderFromAtlas     d  -> boundingPoints d
        RenderComposition t ds -> concatMap (transform t . boundingPoints) ds
        RenderSimpleText  _ _  -> mempty --TODO: add bounding points calculation

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data OrthoNorm
   = OrthoNorm_Width
   | OrthoNorm_Height
   | OrthoNorm_Both

pattern Width  = OrthoNorm_Width
pattern Height = OrthoNorm_Height
pattern Both   = OrthoNorm_Both

data OrthoProjectionOpts = OrthoProjectionOpts
   { orthoProjectionOpts_boxAlign      :: BoxAlign
   , orthoProjectionOpts_normalization :: Maybe OrthoNorm
   , orthoProjectionOpts_scale         :: Float
   }
makeFieldsCustom ''OrthoProjectionOpts
instance Default OrthoProjectionOpts where
    def = OrthoProjectionOpts
        { orthoProjectionOpts_boxAlign      = Center
        , orthoProjectionOpts_normalization = Nothing
        , orthoProjectionOpts_scale         = 1.0
        }


