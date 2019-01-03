module Engine.Graphics.Draw.Atlas (initDrawAtlas) where

import Delude
import Linear
import Prelude as String (unlines)
import Foreign (nullPtr)
import Data.List.Split (chunksOf)

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))

import Graphics.GL
import Engine.Graphics.Utils
import Engine.Graphics.TextureAtlas

--------------------------------------------------------------------------------

initDrawAtlas :: MonadIO m => m (TextureAtlas -> DrawBatch AtlasBatch)
initDrawAtlas = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    let vs = concat $ replicate maxBatchSize $ concatMap toList defVertices
    let is = concatMap makeIndices [0 .. fromIntegral $ maxBatchSize-1]
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER (vs :: [Float])
    texCoordBuffer <- glCreateBuffer
    colorBuffer    <- glCreateBuffer
    modelXBuffer   <- glCreateBuffer
    modelYBuffer   <- glCreateBuffer
    indecesBuffer  <- createBuffer GL_ELEMENT_ARRAY_BUFFER (is :: [Word32])
    let setup atlas proj = do
          glEnable GL_BLEND
          glDisable GL_DEPTH_TEST
          glBlendFuncSeparate
              GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
              GL_ONE       GL_ONE_MINUS_SRC_ALPHA

          glUseProgram program
          glBindVertexArray vao
          bindArrayBuffer  program "VertexPosition" verticesBuffer 2
          bindArrayBuffer  program "TexCoord"       texCoordBuffer 3
          bindArrayBuffer  program "Color"          colorBuffer    4
          bindArrayBuffer  program "ModelX"         modelXBuffer   3
          bindArrayBuffer  program "ModelY"         modelYBuffer   3
          setupAtlas atlas program
          setUniform program "ProjectionMatrix" proj

    let draw atlas fullBatch = do
          mlocs <- lookupAtlasLocations atlas $ map (view textureId) fullBatch
          let fb = catMaybes $ zipWith (\ml r -> fmap (,r) ml) mlocs fullBatch
          let bs = map unzip $ chunksOf maxBatchSize fb
          when (length bs > 1) $ logOnce "Big one!"
          forM_ bs $ \(locs, rs) -> do
            let len   = length rs
            let ts    = concatMap combTexCoords locs
            let cs    = concatMap (convColor . view color) rs
            let trans = map (mkModelVecXY . view modelTransform) rs
            let mxs   = concatMap (toModelVec . view _x) trans
            let mys   = concatMap (toModelVec . view _y) trans
            let count = fromIntegral $ len*6
            setBufferData texCoordBuffer GL_ARRAY_BUFFER (ts  :: [Float])
            setBufferData colorBuffer    GL_ARRAY_BUFFER (cs  :: [Float])
            setBufferData modelXBuffer   GL_ARRAY_BUFFER (mxs :: [Float])
            setBufferData modelYBuffer   GL_ARRAY_BUFFER (mys :: [Float])
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecesBuffer
            glDrawElements GL_TRIANGLES count GL_UNSIGNED_INT nullPtr

    return $ \atlas proj rs -> setup atlas proj >> draw atlas rs
    where
    vertexShader = String.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in vec3 TexCoord;"
        , "in vec4 Color;"
        , "in vec3 ModelX;"
        , "in vec3 ModelY;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec3 vTexCoord;"
        , "out highp vec4 vColor;"
        , "void main(void){"
        , "  mat4 MM = mat4(1.0);"
        , "  MM[0] = vec4(ModelX.xy, 0, ModelX.z);"
        , "  MM[1] = vec4(ModelY.xy, 0, ModelY.z);"
        , "  mat4 Matrix = ProjectionMatrix * transpose(MM);"
        , "  gl_Position = Matrix * vec4(VertexPosition, 0, 1);"
        , "  vTexCoord = TexCoord;"
        , "  vColor = Color;"
        , "}" ]

    fragmentShader = String.unlines $ shaderVersion :
        [ "uniform sampler2D Texture[gl_MaxTextureImageUnits];"
        , "in highp vec3 vTexCoord;"
        , "in highp vec4 vColor;"
        , "out vec4 FragColor;"
        , "void main(void){"
        , "  int rp = int(floor(vTexCoord.z));"
        , "  int page = clamp(rp, 0, gl_MaxTextureImageUnits-1);"
        , "  vec4 color = texture2D(Texture[page], vTexCoord.xy);"
        , "  FragColor.rgb = mix(color.rgb, vColor.rgb, vColor.a);"
        , "  FragColor.a = color.a * vColor.a;"
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

    combTexCoords :: AtlasLocation -> [Float]
    combTexCoords loc = concatMap (toList . addPage) $ loc^.texCoords
        where
        addPage (V2 x y) = V3 x y p
        p = fromIntegral $ loc^.page

    convColor :: AlphaColor -> [Float]
    convColor = concatMap (toList . fromAlphaColor) . replicate 4

    toModelVec = map realToFrac . concat . replicate 4 . toList

    makeIndices :: Word32 -> [Word32]
    makeIndices i = map (\x ->x+i*4) [0, 1, 2, 2, 1, 3]


