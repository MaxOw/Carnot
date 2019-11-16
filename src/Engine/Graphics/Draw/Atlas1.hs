{-# Language StrictData #-}
{-# Language TypeFamilies #-}
module Engine.Graphics.Draw.Atlas1
    ( AtlasPrepProc, AtlasDrawProc
    , initDrawAtlas
    ) where

import Delude
import Linear
import Prelude as String (unlines)
import Data.Vector (Vector)

import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Algorithms.Radix as M

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))

import Graphics.GL
import Engine.Common.Types
import Engine.Graphics.Utils
import Engine.Graphics.TextureAtlas
import Engine.Graphics.Buffer

import Engine.Graphics.Draw.Common (makeInitDrawCall)

--------------------------------------------------------------------------------

unsafeSortZ
    :: Vector (AtlasLocation, AtlasDesc)
    -> IO (Vector (AtlasLocation, AtlasDesc))
unsafeSortZ vv = do
    v <- Vector.unsafeThaw vv
    let f (_, a) = a^.zindex
    let e = 0 :: Word32
    M.sortBy (M.passes e) (M.size e) (\i a -> M.radix i (f a)) v
    Vector.unsafeFreeze v

dloc :: AtlasLocation
dloc = AtlasLocation
   { field_page      = -1
   , field_offset    = pure 0
   , field_size      = pure 0
   }

batchItemsFromAtlas
    :: TextureAtlas -> Vector AtlasDesc -> IO (Storable.Vector AtlasBatchItem)
batchItemsFromAtlas atlas vv = do
    texLookup <- getLookupAtlasLocation atlas
    let toImgOut :: AtlasDesc -> Maybe (AtlasLocation, AtlasDesc)
        -- toImgOut a = (,a) <$> (texLookup =<< a^.textureId)
        toImgOut a = (,a) <$> (maybe (Just dloc) texLookup $ a^.textureId)

    v <- unsafeSortZ $ Vector.mapMaybe toImgOut vv
    let l = Vector.length v
    -- let dmax = 8388608 -- 2^23
    let maxTexSize = fromIntegral $ atlas^.maxTextureSize
    mv <- M.new (l*4)
    iforM_ v $ \i (loc, a) -> do
        let cp = fromIntegral $ fromMaybe (-1) $ a^?customPage.traverse._1._Wrapped
        let V2 mx my = mkModelVecXY $ a^.modelTransform
        let mkV tc = AtlasBatchItem
                { field_modelX = mx
                , field_modelY = my
                , field_color  = fromAlphaColor $ a^.color
                , field_texCoord = tc
                , field_radius   = a^.radius
                , field_colorMix = a^.colorMix
                , field_customPageNum = cp
                }
        let writeQQ = writeQ mv i mkV
        case a^.textureId of
            Just _ -> do
                let r = makeAtlasTexCoords maxTexSize loc (a^.part)
                let p = fromIntegral (loc^.page)
                writeQQ r p
            Nothing -> case a^.customPage of
                Just (pid, r) -> writeQQ r $ fromIntegral (pid^._Wrapped)
                Nothing       -> writeQQ (Rect (pure 0) (pure 0)) (-1)

    Vector.unsafeFreeze mv

    where
    writeQ mv i mkV r p = do
        let V2 x0 y0 = r^.offset
        let V2 x1 y1 = r^.offset + r^.size._Wrapped
        M.write mv (i*4 + 0) (mkV $ V3 x1 y1 p)
        M.write mv (i*4 + 1) (mkV $ V3 x0 y1 p)
        M.write mv (i*4 + 2) (mkV $ V3 x1 y0 p)
        M.write mv (i*4 + 3) (mkV $ V3 x0 y0 p)

makeAtlasTexCoords
    :: Float -> AtlasLocation -> Maybe (Rect Int) -> Rect Float
makeAtlasTexCoords maxTexSize loc mpart = convRect $ case mpart of
        Nothing -> Rect (loc^.offset) (MkSize $ loc^.size)
        Just rp -> Rect (loc^.offset + rp^.offset) (rp^.size)
    where
    convRect r = Rect vo (MkSize vs)
        where
        vo = ((fromIntegral <$> r^.offset)        + 0.5) ^/ maxTexSize
        vs = ((fromIntegral <$> r^.size._Wrapped) - 1.0) ^/ maxTexSize

--------------------------------------------------------------------------------

initDrawAtlas :: MonadIO m => TextureAtlas -> m (AtlasPrepProc, AtlasDrawProc)
initDrawAtlas atlas = do
    drawCall <- liftIO $ makeInitDrawCall vertexShader fragmentShader

    let setup = batchItemsFromAtlas atlas
    let draw  = \mixColors proj fullBatch ->
          drawCall fullBatch $ \program -> do
            -- logOnce $ show $ (div (Vector.length fullBatch) 4)

            setupAtlas atlas program
            setUniform program "ProjectionMatrix" proj
            -- setUniform program "MixColors" mixColors

            glDisable GL_DEPTH_TEST
            if mixColors
            then do
                glEnable GL_BLEND
                glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                glBlendEquation GL_FUNC_ADD
            else glDisable GL_BLEND
                -- glEnable GL_BLEND
                -- glBlendFunc GL_ONE GL_ZERO
                -- glBlendEquation GL_FUNC_ADD
            -- glBlendFuncSeparate
                -- GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                -- GL_ONE       GL_ONE_MINUS_SRC_ALPHA

    return (setup, draw)
    where
    vertexShader = String.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in vec3 TexCoord;"
        , "in vec4 Color;"
        , "in vec3 ModelX;"
        , "in vec3 ModelY;"
        , "in float Radius;"
        , "in float ColorMix;"
        , "in float CustomPageNum;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec3 vTexCoord;"
        , "out highp vec4 vColor;"
        , "out highp vec2 vPosition;"
        , "out highp float vRadius;"
        , "out highp float vColorMix;"
        , "out highp float vCustomPageNum;"
        , "void main(void){"
        , "  mat4 MM = mat4(1.0);"
        , "  MM[0] = vec4(ModelX.xy, 0, ModelX.z);"
        , "  MM[1] = vec4(ModelY.xy, 0, ModelY.z);"
        , "  mat4 Matrix = ProjectionMatrix * transpose(MM);"
        , "  gl_Position = Matrix * vec4(VertexPosition, 0, 1);"
        , "  vTexCoord = TexCoord;"
        , "  vColor = Color;"
        , "  vPosition = VertexPosition*2;"
        , "  vRadius = Radius;"
        , "  vColorMix = ColorMix;"
        , "  vCustomPageNum = CustomPageNum;"
        , "}" ]

    maxPrimaryPages = 16 :: Int
    maxCustomPages  = 16 :: Int
    fragmentShader = String.unlines $ shaderVersion :
        [ "#define MaxPrimaryPages " <> (show maxPrimaryPages)
        , "#define MaxCustomPages " <> (show maxCustomPages)
        , "uniform sampler2D Primary[MaxPrimaryPages];"
        , "uniform sampler2D Custom[MaxCustomPages];"
        -- , "uniform bool MixColors;"
        , "in highp vec3 vTexCoord;"
        , "in highp vec4 vColor;"
        , "in highp vec2 vPosition;"
        , "in highp float vRadius;"
        , "in highp float vColorMix;"
        , "in highp float vCustomPageNum;"
        , "out vec4 FragColor;"
        , "void main(void){"
        , "  int page = clamp(int(floor(vTexCoord.z)),    0, MaxPrimaryPages-1);"
        , "  int cust = clamp(int(floor(vCustomPageNum)), 0, MaxCustomPages -1);"
        , "  vec4 color;"
        , "  if(vCustomPageNum < 0) "
        , "    color = texture2D(Primary[page], vTexCoord.xy);"
        , "  else "
        , "    color = texture2D(Custom [cust], vTexCoord.xy);"
        , "  FragColor.rgb = mix(color.rgb, vColor.rgb, vColorMix);"
        , "  FragColor.a = color.a * mix(1, vColor.a, vColorMix);"
        -- , "  if(!MixColors) FragColor = color;"
        , "  if(vTexCoord.z < 0) FragColor = vColor;"
        , "  float r = vRadius;"
        , "  float l = length(vPosition);"
        , "  float delta = fwidth(l);"
        , "  float v = smoothstep(r - delta, r + delta, l);"
        , "  if (l > r-delta) FragColor.a *= 1-v;"
        , "}" ]

