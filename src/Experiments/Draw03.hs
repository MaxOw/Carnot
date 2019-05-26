module Experiments.Draw03 where

import Delude
import Linear
-- import Foreign.Storable (sizeOf)
-- import Data.Proxyless (theSizeOf)
import Data.List as List (unlines, (!!))
import Engine.Lens.Utils
-- import Foreign (nullPtr)
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Context as Context
import Foreign.Storable.Generic (GStorable)

-- import Data.Vector.Mutable (IOVector)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
-- import Data.Vector (Vector)
-- import qualified Data.Vector as Vector

import Criterion.Main
import Experiments.Common

import Graphics.GL
import Engine.Graphics.Utils
import Engine.Graphics.Buffer

--------------------------------------------------------------------------------

bench_draw03 :: Int -> Int -> Benchmark
bench_draw03 squareSize count = mkBench "draw03" (initSt squareSize count) runSt

--------------------------------------------------------------------------------

data CustomVertex = CustomVertex
   { field_zindex        :: Float
   , field_position      :: V2 Float
   , field_color         :: V3 Float
   }
   deriving (Generic)
instance NFData CustomVertex
instance GStorable CustomVertex

data CustomVertexItem = CustomVertexItem
   { field_zindex        :: Float
   , field_modelX        :: V3 Float
   , field_modelY        :: V3 Float
   , field_color         :: V3 Float
   } deriving (Generic)
instance GStorable CustomVertexItem

instance ArrayBufferItem CustomVertexItem where
    arrayBufferItemDesc _ =
        [ Attrib "ZIndex" (Proxy @Float)
        , Attrib "ModelX" (Proxy @(V3 Float))
        , Attrib "ModelY" (Proxy @(V3 Float))
        , Attrib "Color"  (Proxy @(V3 Float))
        ]

type DrawCall = Bool -> Mat4 -> Vector CustomVertex -> IO ()
data St = St
   { field_context    :: Context
   , field_elems      :: !(Vector CustomVertex)
   , field_elemsBlend :: !(Vector CustomVertex)
   , field_draw       :: DrawCall
   , field_projMat    :: Mat4
   } deriving (Generic)
instance NFData St

--------------------------------------------------------------------------------

initSt :: Int -> Int -> IO St
initSt squareSize count = do
    cx <- Context.initWindow "draw03" (400, 400)
    dc <- initDrawCall (fromIntegral squareSize)
    GLFW.swapInterval 0
    GLFW.showWindow cx

    canvasSize <- Context.getFramebufferSize cx
    let (w, h) = over each fromIntegral canvasSize
    -- let vd = Vector.fromList $ take count $ map (mkVertex w h) [0 :: Int ..]
    let vd = Vector.fromList [mkVertex w h 1]
    let vb = Vector.fromList [mkVertex w h 2]

    let ss = fromIntegral squareSize
    let ww = ceiling $ w/ss
    let hh = ceiling $ h/ss
    print (ww*hh :: Int)
    print (div count (ww*hh) :: Int)

    glEnable GL_DEPTH_TEST
    glDepthFunc GL_LESS

    projM <- orthoMat cx
    return $ St
        { field_context    = cx
        , field_elems      = vd
        , field_elemsBlend = vb
        , field_draw       = dc
        , field_projMat    = projM
        }
    where
    mkVertex w h x = CustomVertex
        { field_zindex   = fromIntegral x / fromIntegral count
        , field_position = pos
        , field_color    =
            [ V3 1 0 0, V3 0 1 0, V3 0 0 1
            , V3 1 1 0, V3 0 1 1, V3 1 0 1
            , V3 0 0 0 ] !! (mod x 7)
        }
        where
        distribute = True
        blend = False
        pos | distribute = V2 (wx - w/2 + ss/2 + oo) (hx - h/2 + ss/2 + oo)
            | otherwise  = V2 0 0
        ss = fromIntegral squareSize
        ww = ceiling $ w/ss
        hh = ceiling $ h/ss
        wx = ss * (fromIntegral $ (xx - hn*ww))
        hx = ss * (fromIntegral $ hn)
        hn = div xx ww
        xx = mod x (ww*hh)
        oo = if blend
            then (-ss/2) * (fromIntegral $ mod (div x (ww*hh)) 2)
            else 0

runSt :: St -> IO ()
runSt st = do
    let cx = field_context st
    withBuffers cx $ do
        let projM = field_projMat st

        (st^.ff#draw) False projM (st^.ff#elems)
        (st^.ff#draw) True  projM (st^.ff#elemsBlend)

initDrawCall :: Float -> IO DrawCall
initDrawCall squareSize = do
    drawCall <- makeInitDrawCall vertexShader fragmentShader toItems
    return $ \blend proj fullBatch -> do
        when blend $ do
            glDepthMask GL_FALSE
            glEnable GL_BLEND
            glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
            glBlendEquation GL_FUNC_ADD

        drawCall fullBatch $ \program -> do
         -- setUniform program "Blend"            blend
            setUniform program "ProjectionMatrix" proj

        glDepthMask GL_TRUE
        glDisable GL_BLEND
    where
    toItems :: CustomVertex -> Vector CustomVertexItem
    toItems c = Vector.replicate 4 $ CustomVertexItem
        { field_zindex = c^.ff#zindex
        , field_modelX = V3 s 0 (c^.ff#position._x)
        , field_modelY = V3 0 s (c^.ff#position._y)
        , field_color  = c^.ff#color
        }
        where s = squareSize

    vertexShader = List.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in vec3 Color;"
        , "in vec3 ModelX;"
        , "in vec3 ModelY;"
        , "in float ZIndex;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec3 vColor;"
        , "void main(void){"
        , "  mat4 MM = mat4(1.0);"
        , "  MM[0] = vec4(ModelX.xy, 0, ModelX.z);"
        , "  MM[1] = vec4(ModelY.xy, 0, ModelY.z);"
        , "  mat4 Matrix = ProjectionMatrix * transpose(MM);"
        , "  gl_Position = Matrix * vec4(VertexPosition, 0, 1);"
        , "  gl_Position.z = ZIndex;"
        , "  vColor = Color;"
        , "}" ]

    fragmentShader = List.unlines $ shaderVersion :
        [ "in highp vec3 vColor;"
        , "out vec4 FragColor;"
     -- , "uniform bool Blend;"
        , "void main(void){"
        , "  FragColor.rgb = vColor.rgb;"
        , "  FragColor.a   = 0.1;"
        , "}" ]


