module Experiments.Draw01 where

import Delude
import Data.List as List (unlines)
import Engine.Lens.Utils
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Context as Context
import Foreign.Storable.Generic (GStorable)

-- import Data.Vector.Mutable (IOVector)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as Vector
-- import Data.Vector (Vector)
-- import qualified Data.Vector as Vector

import Criterion.Main
import Experiments.Common

import Graphics.GL
import Engine.Graphics.Utils
import Engine.Graphics.Buffer

--------------------------------------------------------------------------------

bench_draw01 :: Bool -> Int -> Benchmark
bench_draw01 depthTest count = mkBench "draw01" (initSt depthTest count) runSt

--------------------------------------------------------------------------------

data CustomVertex = CustomVertex
   { field_zindex        :: Float
   }
   deriving (Generic)
instance NFData CustomVertex
instance GStorable CustomVertex

data CustomVertexItem = CustomVertexItem
   { field_zindex        :: Float
   } deriving (Generic)
instance GStorable CustomVertexItem

instance ArrayBufferItem CustomVertexItem where
    arrayBufferItemDesc _ =
        [ Attrib "ZIndex" (Proxy @Float)
        ]

type DrawCall = Vector CustomVertex -> IO ()
data St = St
   { field_context :: Context
   , field_elems   :: !(Vector CustomVertex)
   , field_draw    :: DrawCall
   } deriving (Generic)
instance NFData St

--------------------------------------------------------------------------------

initSt :: Bool -> Int -> IO St
initSt depthTest count = do
    cx <- Context.initWindow "draw01" (400, 400)
    dc <- initDrawCall
    let vd = Vector.fromList $ take count $ map mkVertex [0 :: Int ..]
    GLFW.swapInterval 0

    glDisable GL_BLEND
    when depthTest $ do
        glEnable GL_DEPTH_TEST
        glDepthFunc GL_LESS

    -- GLFW.showWindow cx
    return $ St
        { field_context = cx
        , field_elems   = vd
        , field_draw    = dc
        }
    where
    mkVertex x = CustomVertex $ fromIntegral x / fromIntegral count

runSt :: St -> IO ()
runSt st = do
    let cx = field_context st
    withBuffers cx $ st^.ff#draw $ st^.ff#elems

initDrawCall :: IO DrawCall
initDrawCall = do
    drawCall <- makeInitDrawCall vertexShader fragmentShader toItems
    return $ \fullBatch -> do
        drawCall fullBatch $ const $ return ()

    where
    toItems :: CustomVertex -> Vector CustomVertexItem
    toItems c = Vector.replicate 4 $ CustomVertexItem
        { field_zindex = c^.ff#zindex
        }

    vertexShader = List.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in float ZIndex;"
        , "void main(void){"
        , "  gl_Position = vec4(VertexPosition*2, ZIndex, 1);"
        , "}" ]

    fragmentShader = List.unlines $ shaderVersion :
        [ "out vec4 FragColor;"
        , "void main(void){"
        , "  FragColor = vec4(1,0,0,0);"
        , "}" ]

{-
initDrawCall :: Bool -> IO DrawCall
initDrawCall depthTest = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    let vs = concat $ replicate maxBatchSize $ concatMap toList defVertices
    let is = concatMap makeIndices [0 .. fromIntegral $ maxBatchSize-1]
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER (vs :: [Float])
    arrayBuffer    <- createArrayBuffer program
    indecesBuffer  <- createBuffer GL_ELEMENT_ARRAY_BUFFER (is :: [Word32])
    return $ \fullBatch -> do
        glDisable GL_BLEND
        when depthTest $ do
            glEnable GL_DEPTH_TEST
            glDepthFunc GL_LESS
        glUseProgram program
        glBindVertexArray vao
        bindArrayBuffer program "VertexPosition" verticesBuffer 2
        bindArrayBufferPointers arrayBuffer

        let len = Vector.length fullBatch
        let elemCount = fromIntegral $ 6 * len
        setArrayBufferData arrayBuffer $ Vector.concatMap toItems fullBatch
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecesBuffer
        glDrawElements GL_TRIANGLES elemCount GL_UNSIGNED_INT nullPtr
    where
    toItems :: CustomVertex -> Vector CustomVertexItem
    toItems c = Vector.replicate 4 $ CustomVertexItem
        { field_zindex = c^.ff#zindex
        }

    vertexShader = List.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in float ZIndex;"
        , "void main(void){"
        , "  gl_Position = vec4(VertexPosition, ZIndex, 1);"
        , "}" ]

    fragmentShader = List.unlines $ shaderVersion :
        [ "out vec4 FragColor;"
        , "void main(void){"
        , "  FragColor = vec4(1,0,0,0);"
        , "}" ]

    defVertices :: [V2 Float]
    defVertices =
        [ V2   1  (-1)
        , V2 (-1) (-1)
        , V2   1    1
        , V2 (-1)   1 ]

    makeIndices :: Word32 -> [Word32]
    makeIndices i = map (\x ->x+i*4) [0, 1, 2, 2, 1, 3]

maxBatchSize :: Int
maxBatchSize = 16384
-}
-- maxBatchSize = 32768

