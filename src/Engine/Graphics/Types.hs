-- {-# Language StrictData      #-}
{-# Language TemplateHaskell #-}
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
import Data.Vector (Vector)
import qualified Data.Vector.Storable as Storable

import Diagrams.Transform
import Diagrams.Core.V

import Engine.Backend.Types as Engine.Graphics.Types
import Engine.Graphics.TextureAtlas.Types
import Engine.FontsManager.Types
import Engine.Common.Types
import Engine.Graphics.Buffer.Types
import Engine.Graphics.Buffer.Types as Engine.Graphics.Types (DrawBatch)
import Engine.Graphics.TextureCache.Types
import Engine.Graphics.DrawBatchCache.Types
import Engine.Graphics.TaskManager

--------------------------------------------------------------------------------

type Vec2 = V2 Float
type Vec3 = V3 Float
type Vec4 = V4 Float
type Mat4 = M44 Float

type Color = Colour Float
type AlphaColor = AlphaColour Float


--------------------------------------------------------------------------------

data Img = Img
   { field_texture  :: Texture
   , field_size     :: Size Int
   , field_part     :: Maybe (Rect Int)
   , field_zindex   :: Word32
   , field_color    :: AlphaColor
   , field_colorMix :: Float
   } deriving (Generic)
instance HasSize Img (Size Int)
instance Default Img where
    def = Img
        { field_texture  = noTexture
        , field_size     = pure 0
        , field_part     = Nothing -- unitRect
        , field_zindex   = 0
        , field_color    = Color.transparent
        , field_colorMix = 0
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
   { field_shapeType      :: SimpleShape
   , field_color          :: AlphaColor
   , field_modelTransform :: T2D
   , field_zindex         :: Word32
   } deriving (Generic)
instance Default ShapeDesc where
    def = ShapeDesc
        { field_shapeType      = SimpleCircle
        , field_color          = Color.opaque Color.white
        , field_modelTransform = mempty -- Linear.identity
        , field_zindex         = 0
        }
type instance N ShapeDesc = Float
type instance V ShapeDesc = V2
instance Transformable ShapeDesc where
    transform t = over modelTransform (t <>)

--------------------------------------------------------------------------------

{-
data TextureDesc = TextureDesc
   { field_textureId      :: Texture
   , field_modelTransform :: T2D
   } deriving (Generic)
instance Default TextureDesc where
    def = TextureDesc
        { field_textureId      = 0 -- noTexture
        , field_modelTransform = mempty
        }
type instance N TextureDesc = Float
type instance V TextureDesc = V2
instance Transformable TextureDesc where
    transform t = over modelTransform (t <>)
-}

class BoundingPoints a where
    boundingPoints :: a -> [V2 Float]

data AtlasDesc = AtlasDesc
   { field_textureId      :: Maybe Texture
   , field_color          :: AlphaColor
   , field_modelTransform :: T2D
   , field_radius         :: Float
   , field_colorMix       :: Float
   , field_zindex         :: Word32
   , field_part           :: Maybe (Rect Int)
   , field_customPage     :: Maybe (PageId, Rect Float)
   } deriving (Generic)
instance Default AtlasDesc where
    def = AtlasDesc
        { field_textureId      = Nothing -- 0 -- noTexture
        , field_color          = Color.opaque Color.black
        , field_modelTransform = mempty
        , field_radius         = 2
        , field_colorMix       = 1
        , field_zindex         = 0
        , field_part           = Nothing -- unitRect
        , field_customPage     = Nothing
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
   { field_modelTransform :: T2D
   , field_zindex         :: Word32
   , field_fontName       :: Maybe FontName
   , field_fontSize       :: Maybe FontSize
   , field_color          :: AlphaColor
   , field_boxAlign       :: BoxAlign
   } deriving (Generic)
type instance N SimpleTextDesc = Float
type instance V SimpleTextDesc = V2
instance Transformable SimpleTextDesc where
    transform t = over modelTransform (t <>)
instance Default SimpleTextDesc where
    def = SimpleTextDesc
        { field_modelTransform = mempty
        , field_zindex         = 0
        , field_fontName       = Nothing
        , field_fontSize       = Nothing
        , field_color          = Color.opaque Color.black
        , field_boxAlign       = Center
        }

--------------------------------------------------------------------------------

-- type TexturesBatch = Seq TextureDesc
-- type AtlasBatch    = Seq AtlasDesc

{-
data DrawRequest = DrawRequest
   { _requestedAtlas      :: Vector AtlasDesc
   -- , _requestedTextures :: TexturesBatch
   } deriving (Generic)
instance Default DrawRequest where
    def = DrawRequest
        { _requestedAtlas = mempty
        }
makeLenses ''DrawRequest
-}

type TextCacheKey = (BoxAlign, FontStyle, Text)

data TextLineDesc = TextLineDesc
   { field_size            :: Size Float
   , field_verticalOffset  :: Float
   , field_minSpaceAdvance :: Float
   } deriving (Generic)
instance Default TextLineDesc
instance HasSize TextLineDesc (Size Float)

-- type DrawBatch batch = Mat4 -> batch -> IO ()
-- type DrawProcedure = Mat4 -> DrawRequest -> IO ()
type DrawRequest   = Vector AtlasDesc
type AtlasPrepProc = Vector AtlasDesc -> IO (Storable.Vector AtlasBatchItem)
type AtlasDrawProc = Bool -> Mat4 -> Storable.Vector AtlasBatchItem -> IO ()
data GraphicsState = GraphicsState
   { _context            :: Context
   , _setupProcedure     :: AtlasPrepProc
   , _drawProcedure      :: AtlasDrawProc
   , _textureAtlas       :: TextureAtlas
   , _fontsManager       :: FontsManager
   , _defaultFontStyle   :: Maybe FontStyle
   , _textCache          :: TextureCache
   , _drawBatchCache     :: DrawBatchCache
   , _taskManager        :: TaskManager
   }
makeLenses ''GraphicsState

--------------------------------------------------------------------------------

data RenderAction
   = RenderFromAtlas AtlasDesc
   | RenderComposition T2D [RenderAction]

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

instance BoundingPoints RenderAction where
    boundingPoints = \case
        RenderFromAtlas     d  -> boundingPoints d
        RenderComposition t ds -> concatMap (transform t . boundingPoints) ds

--------------------------------------------------------------------------------

data DrawCharStep = DrawCharStep
   { field_texture        :: Texture
   , field_color          :: AlphaColor
   , field_modelTransform :: T2D
   , field_advance        :: Int
   } deriving (Generic)

-- pattern RenderNothing :: RenderAction
-- pattern RenderNothing = RenderComposition m []

data RenderTextLayout = RenderTextLayout
   { field_size         :: Size AbsoluteSize
   , field_renderAction :: RenderAction
   } deriving (Generic)
instance HasSize RenderTextLayout (Size AbsoluteSize)

--------------------------------------------------------------------------------

data OrthoNorm
   = OrthoNorm_Width
   | OrthoNorm_Height
   | OrthoNorm_Both

pattern Width  = OrthoNorm_Width
pattern Height = OrthoNorm_Height
pattern Both   = OrthoNorm_Both

data OrthoProjectionOpts = OrthoProjectionOpts
   { field_boxAlign      :: BoxAlign
   , field_normalization :: Maybe OrthoNorm
   , field_scale         :: Float
   } deriving (Generic)
instance Default OrthoProjectionOpts where
    def = OrthoProjectionOpts
        { field_boxAlign      = Center
        , field_normalization = Nothing
        , field_scale         = 1.0
        }

