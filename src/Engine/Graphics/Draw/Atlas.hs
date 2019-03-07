{-# Language StrictData #-}
{-# Language TypeFamilies #-}
module Engine.Graphics.Draw.Atlas (initDrawAtlas) where

import Delude
import Linear
import Prelude as String (unlines)
import Foreign (nullPtr)
import Data.List.Split (chunksOf)

import qualified Data.Vector.Storable as Vector

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))

import Graphics.GL
import Engine.Common.Types
import Engine.Graphics.Utils
import Engine.Graphics.TextureAtlas
import Engine.Graphics.Buffer
import Engine.Common.Types (rectToList)

batchItemFromAtlas :: MonadIO m => TextureAtlas -> AtlasDesc -> m [AtlasBatchItem]
batchItemFromAtlas atlas d = do
    mlocs <- lookupLoc $ d^.textureId
    case mlocs of
        Nothing -> return []
        Just lc -> do
            let V2 mx my = mkModelVecXY $ d^.modelTransform
            let mkItem x = AtlasBatchItem
                    { atlasBatchItem_texCoord      = x
                    , atlasBatchItem_color         = fromAlphaColor $ d^.color
                    , atlasBatchItem_modelX        = fmap realToFrac mx
                    , atlasBatchItem_modelY        = fmap realToFrac my
                    , atlasBatchItem_radius        = d^.radius
                    , atlasBatchItem_colorMix      = d^.colorMix
                    , atlasBatchItem_customPageNum = cPid
                    }
            return $ map mkItem lc

    where
    cPid = fromIntegral $ fromMaybe (-1) $ d^?customPage.traverse._1._Wrapped
    convTexCoords loc
        -- = map addPage $ rectToList $ subRectOf (d^.part) (loc^.texCoords)
        = map addPage $ makeAtlasTexCoords atlas loc (d^.part)
        where
        addPage (V2 x y) = V3 x y p
        p = fromIntegral $ loc^.page

    lookupLoc (Just tid) = fmap convTexCoords <$> lookupAtlasLocation atlas tid
    lookupLoc Nothing    = case d^.customPage of
        Just (pid, rc) -> return $ Just $ makeCustomTexCoords pid rc
        Nothing -> return $ Just $ replicate 4 (V3 0 0 (-1))

makeCustomTexCoords :: PageId -> Rect Float -> [V3 Float]
makeCustomTexCoords pid = map addPage . rectToList
    where
    addPage (V2 x y) = V3 x y p
    p = pid^._Wrapped.to fromIntegral

makeAtlasTexCoords
    :: TextureAtlas -> AtlasLocation -> Maybe (Rect Int) -> [V2 Float]
makeAtlasTexCoords atlas loc mpart = rectToList $ convRect $ case mpart of
    Nothing -> Rect (loc^.offset) (MkSize $ loc^.size)
    Just rp -> Rect (loc^.offset + rp^.offset) (rp^.size)
    where
    maxTexSize = fromIntegral $ atlas^.maxTextureSize
    convRect r = Rect vo (MkSize vs)
        where
        vo = ((fromIntegral <$> r^.offset)        + 0.5) ^/ maxTexSize
        vs = ((fromIntegral <$> r^.size._Wrapped) - 1.0) ^/ maxTexSize

--------------------------------------------------------------------------------

type AtlasDrawProc = TextureAtlas -> Mat4 -> Seq AtlasDesc -> IO ()
initDrawAtlas :: MonadIO m => m AtlasDrawProc
initDrawAtlas = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    let vs = concat $ replicate maxBatchSize $ concatMap toList defVertices
    let is = concatMap makeIndices [0 .. fromIntegral $ maxBatchSize-1]
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER (vs :: [Float])
    arrayBuffer    <- createArrayBuffer program
    indecesBuffer  <- createBuffer GL_ELEMENT_ARRAY_BUFFER (is :: [Word32])

    let setupDraw atlas proj fullBatch = do
          glEnable GL_BLEND
          glDisable GL_DEPTH_TEST
          glBlendFuncSeparate
              GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
              GL_ONE       GL_ONE_MINUS_SRC_ALPHA

          glUseProgram program
          glBindVertexArray vao
          bindArrayBuffer  program "VertexPosition" verticesBuffer 2
          bindArrayBufferPointers arrayBuffer
          setupAtlas atlas program
          setUniform program "ProjectionMatrix" proj

          bis <- concat <$> mapM (batchItemFromAtlas atlas) fullBatch
          let bs = chunksOf (maxBatchSize*4) bis
          forM_ bs $ \batch -> do
            let vec = Vector.fromList batch
            let elemCount = fromIntegral $ 6 * div (Vector.length vec) 4
            setArrayBufferData arrayBuffer vec
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecesBuffer
            glDrawElements GL_TRIANGLES elemCount GL_UNSIGNED_INT nullPtr

    return setupDraw
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
        , "  if(vTexCoord.z < 0) FragColor = vColor;"
        , "  float r = vRadius;"
        , "  float l = length(vPosition);"
        , "  float delta = fwidth(l);"
        , "  float v = smoothstep(r - delta, r + delta, l);"
        , "  if (l > r-delta) FragColor.a *= 1-v;"
        , "}" ]

    maxBatchSize :: Int
    maxBatchSize = 16384
 -- maxBatchSize = 32768

    defVertices :: [V2 Float]
    defVertices = map (*0.5)
        [ V2   1  (-1)
        , V2 (-1) (-1)
        , V2   1    1
        , V2 (-1)   1 ]

    makeIndices :: Word32 -> [Word32]
    makeIndices i = map (\x ->x+i*4) [0, 1, 2, 2, 1, 3]

