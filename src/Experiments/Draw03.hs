module Experiments.Draw03 where

import Delude
import Linear
-- import Foreign.Storable (sizeOf)
-- import Data.Proxyless (theSizeOf)
import Data.List ((!!))
import qualified Data.List as List
import qualified Data.IntMap as IntMap
import Engine.Lens.Utils
-- import Foreign (nullPtr)
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Context as Context
import Foreign.Storable.Generic (GStorable)

-- import Data.Vector.Mutable (IOVector)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector as V
-- import Data.Vector (Vector)
-- import qualified Data.Vector as Vector

import Criterion.Main
import Experiments.Common

import Graphics.GL
import Engine.Graphics.Utils
import Engine.Graphics.Buffer
import qualified Test.QuickCheck as QC
-- import qualified Data.Vector.Algorithms.Intro as M
-- import qualified Data.Vector.Algorithms.AmericanFlag as M
import qualified Data.Vector.Algorithms.Radix as M
-- import qualified Data.Vector.Mutable as M

-- import Data.GrowVector
-- import qualified Data.ZMap as Z

--------------------------------------------------------------------------------

bench_draw03 :: Int -> Int -> Benchmark
bench_draw03 squareSize count = mkBench "draw03" (initSt squareSize count) runSt

-- * Benchmark sorting & partitioning
-- * Benchmark texture lookup

bench_sortPart :: Int -> Benchmark
bench_sortPart n = env (gvec n) $ \v ->
    -- bench "Sort & Partition" $ nfIO (sortPart v) -- =<< V.thaw v)
    -- bench "Sort & Partition" $ nfIO (sortPart v) -- =<< V.thaw v)
    bench "Texture lookup"   $ nf (V.mapMaybe (flip IntMap.lookup im)) v
    where
    im = IntMap.fromList $ map (\x -> (x,x)) [0..1000]

gvec :: Int -> IO (V.Vector Int)
gvec n = fmap V.fromList $
    -- QC.generate $ QC.vectorOf n $ QC.choose (0, 1000000)
    QC.generate $ QC.vectorOf n $ QC.choose (0, 100)

sortPart :: V.Vector Int -> IO (V.Vector Int, V.Vector Int)
sortPart v = do
    vv <- V.thaw v
    {-
    zm <- Z.new 100
    V.forM_ v $ \x -> do
        Z.add zm x x
    vv <- Z.toVector zm
    -}
    -- return $ over _1 V.reverse $ V.partition even vv -- <$> V.unsafeFreeze v
    M.sort vv
    over _1 V.reverse . V.partition even <$> V.unsafeFreeze vv
    -- . fromList . sort . Vector.toList

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
    cx <- Context.initWindow "draw03" (400, 400) False
    dc <- initDrawCall (fromIntegral squareSize)
    GLFW.swapInterval 0
    -- GLFW.showWindow cx

    canvasSize <- Context.getFramebufferSize cx
    let (w, h) = over each fromIntegral canvasSize

    let ss = fromIntegral squareSize
    let ww = ceiling $ w/ss
    let hh = ceiling $ h/ss
    let layerSize = (ww*hh)
    print (ww*hh :: Int)
    print (div count (ww*hh) :: Int)

{-
    let vd = Vector.fromList $ take layerSize
           $ map (mkVertex 1 False w h) [0 :: Int ..]
    let vb = Vector.fromList $ take count
           $ map (mkVertex 0 True w h) [0 :: Int ..]
-}
    let vb = Vector.empty
    let vd = Vector.fromList $ take count
           $ map (mkVertex 0 False w h) [0 :: Int ..]

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
    dmax = 8388608 -- 2^23
    mkVertex zoff blend w h x = CustomVertex
        { field_zindex   = fromIntegral zx / dmax
        , field_position = pos
        , field_color    =
            [ V3 1 0 0, V3 0 1 0, V3 0 0 1
            , V3 1 1 0, V3 0 1 1, V3 1 0 1
            , V3 0 0 0 ] !! (mod zx 7)
        }
        where
        pos = V2 (wx - w/2 + ss/2 + oo) (hx - h/2 + ss/2 + oo)
        ss = fromIntegral squareSize
        ww = ceiling $ w/ss
        hh = ceiling $ h/ss
        wx = ss * (fromIntegral $ (xx - hn*ww))
        hx = ss * (fromIntegral $ hn)
        hn = div xx ww
        xx = mod x (ww*hh)
        zx = x + zoff*ww*hh
        oo = if blend
            then ss/2
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
        , "void main(void){"
        , "  FragColor.rgb = vColor.rgb;"
        , "  FragColor.a   = 0.3;"
        , "}" ]

