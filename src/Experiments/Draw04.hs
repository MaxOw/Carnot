{-# Language TypeFamilies #-}
{-# Language DuplicateRecordFields #-}
{-# Options_GHC -fno-warn-orphans #-}
module Experiments.Draw04 where

import Delude
import Control.Concurrent (getNumCapabilities)
import qualified Relude.Unsafe as Unsafe
import Linear
import Data.List ((!!))
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Engine.Lens.Utils
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Context as Context
import Foreign.Storable.Generic (GStorable)

import Engine.Graphics (renderImg, renderComposition)
import qualified Diagrams.TwoD.Transform as T
import qualified Diagrams.Transform as T
-- import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Algorithms.Radix as M
-- import qualified Data.Vector         as V
import Data.Vector (Vector)

import Criterion.Main
import Experiments.Common hiding (makeInitDrawCall)
import Engine.Graphics.Draw.Common (makeInitDrawCall)

import Graphics.GL
import Engine.Common.Types
import Engine.Graphics.Utils
import Engine.Graphics.Buffer
-- import qualified Data.Vector.Algorithms.Radix as M

import Engine.Graphics.TextureAtlas

import qualified Data.GrowVector as G

-- * Write batching function from RenderAction [X]
-- * Speedup transformation matrix             [ ]
-- * Replace current implementation with this  [ ]

--------------------------------------------------------------------------------

bench_draw04 :: Int -> Int -> Benchmark
bench_draw04 squareSize count
    = mkBench "draw04" (initSt squareSize count) runSt

bench_write04 :: Int -> Int -> Benchmark
bench_write04 squareSize count
    = mkBench "write04" (initSt squareSize count) runWrite

--------------------------------------------------------------------------------

instance NFData a => NFData (Rect a)

data CustomElem = CustomElem
   { field_zindex        :: Float
   , field_position      :: V2 Float
   , field_color         :: V3 Float
   , field_texture       :: Texture
   , field_part          :: Rect Int
   }
   deriving (Generic)
instance NFData CustomElem
instance GStorable a => GStorable (Size a)
instance GStorable a => GStorable (Rect a)
instance GStorable CustomElem

data WriteImg = WriteImg
   { field_tex    :: Texture
   , field_part   :: Rect Int
   , field_zindex :: Int
   , field_pos    :: V2 Float
   } deriving (Generic)
instance NFData WriteImg

data WriteImgOut = WriteImgOut
   { field_location :: AtlasLocation
   , field_part     :: Rect Int
   , field_zindex   :: Int
   , field_pos      :: V2 Float
   } deriving (Generic)

data CustomElemR = CustomElemR
   { field_zindex        :: Float
   , field_position      :: V2 Float
   , field_color         :: V3 Float
   , field_texCoord0     :: V3 Float
   , field_texCoord1     :: V3 Float
   , field_texCoord2     :: V3 Float
   , field_texCoord3     :: V3 Float
   }
   deriving (Generic)
instance NFData CustomElemR
instance GStorable CustomElemR

data CustomVertexItem = CustomVertexItem
   { field_zindex        :: Float
   , field_modelX        :: V3 Float
   , field_modelY        :: V3 Float
   , field_color         :: V3 Float
   , field_texCoord      :: V3 Float
   } deriving (Generic)
instance GStorable CustomVertexItem

instance ArrayBufferItem CustomVertexItem where
    arrayBufferItemDesc _ =
        [ Attrib "ZIndex"   (Proxy @Float)
        , Attrib "ModelX"   (Proxy @(V3 Float))
        , Attrib "ModelY"   (Proxy @(V3 Float))
        , Attrib "Color"    (Proxy @(V3 Float))
        , Attrib "TexCoord" (Proxy @(V3 Float))
        ]

instance NFData TextureAtlas where
    rnf _ = ()

type DrawCall = Bool -> Mat4 -> Vector AtlasDesc -> IO ()
-- type DrawCall = Bool -> Mat4 -> RenderAction -> IO ()
data St = St
   { field_context    :: Context
   , field_atlas      :: TextureAtlas
   -- , field_elems      :: !(V.Vector AtlasDesc)
   , field_elems      :: RenderAction
   -- , field_elemsBlend :: !(Vector CustomElem)
   , field_draw       :: DrawCall
   , field_projMat    :: Mat4
   } deriving (Generic)
instance NFData St
instance NFData AtlasDesc where rnf _ = ()
instance NFData RenderAction where rnf _ = ()

--------------------------------------------------------------------------------

initSt :: Int -> Int -> IO St
initSt squareSize count = do
    cx <- Context.initWindow "draw04" (400, 400)
    atlas <- newAtlas

    img <- Unsafe.fromJust <$> loadImgToAtlas atlas "imgs/dirt.png"
    fullUpdate atlas

    dc <- initDrawCall atlas
    GLFW.swapInterval 0
    GLFW.showWindow cx

    canvasSize <- Context.getFramebufferSize cx
    let (w, h) = over each fromIntegral canvasSize

    let ss = fromIntegral squareSize
    let ww = ceiling $ w/ss
    let hh = ceiling $ h/ss
    -- let layerSize = (ww*hh)
    print (ww*hh :: Int)
    print (div count (ww*hh) :: Int)

    print =<< lookupAtlasLocation atlas (img^.texture)

    -- let vb = Vector.fromList $ take count
           -- $ map (mkVertex img 0 False w h) [0 :: Int ..]

    -- let vb = Vector.fromList $ take count
           -- $ map (mkVertex img 0 False w h) [0 :: Int ..]
    -- let vb = Vector.fromList $ take count $ map (mkI img w h) [0 :: Int ..]
    let vb = renderComposition $ take count $ map (mkI img w h) [0 :: Int ..]

    glEnable GL_DEPTH_TEST
    glDepthFunc GL_LESS

    putStrLn . ("cap num: " <>) . show =<< getNumCapabilities

    projM <- orthoMat cx
    return $ St
        { field_context    = cx
        , field_atlas      = atlas
        , field_elems      = vb
        -- , field_elemsBlend = vb
        , field_draw       = dc
        , field_projMat    = projM
        }
    where
    mkP x y = Rect (32 *^ (V2 x y)) (pure 32)
    dmax = 8388608 :: Float -- 2^23
    mkI img w h i = renderImg (img & part .~ (Just $ selectPart i))
        & T.scale (ss / 32)
        & T.translate pos
        where
        pos = V2 (wx - w/2 + ss/2) (hx - h/2 + ss/2)
        ss = fromIntegral squareSize
        ww = ceiling $ w/ss
        hh = ceiling $ h/ss
        wx = ss * (fromIntegral $ (xx - hn*ww))
        hx = ss * (fromIntegral $ hn)
        hn = div xx ww
        xx = mod i (ww*hh)

    unp (RenderFromAtlas d) = d
    unp _ = undefined

    selectPart i =
        [ mkP 0 0, mkP 1 0, mkP 2 0
        , mkP 0 1, mkP 1 1, mkP 2 2
        , mkP 0 2 ] !! (mod i 7)

    {-
    mkVertex img zoff blend w h x = CustomElem
        { field_zindex   = fromIntegral zx / dmax
        , field_position = pos
        , field_color    = V3 0 0 0
        , field_part     =
            [ mkP 0 0, mkP 1 0, mkP 2 0
            , mkP 0 1, mkP 1 1, mkP 2 2
            , mkP 0 2 ] !! (mod zx 7)
        , field_texture  = img^.texture
        }
    -}
    {-
    mkVertex img zoff blend w h x = WriteImg
        { field_zindex   = zx
        , field_pos      = pos
        , field_part     =
            [ mkP 0 0, mkP 1 0, mkP 2 0
            , mkP 0 1, mkP 1 1, mkP 2 2
            , mkP 0 2 ] !! (mod x 7)
        , field_tex      = img^.texture
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
            -}

{-
batchRenderAction :: RenderAction -> IO (V.Vector AtlasDesc)
batchRenderAction ra = Vector.fromList . toList <$> go Seq.empty ra
    where
    go r = \case
        RenderFromAtlas   d    -> return $ r |> d
        RenderComposition t rs -> foldlM go r (T.transform t rs)
        _                      -> return r
-}

batchRenderAction :: RenderAction -> IO (Vector AtlasDesc)
batchRenderAction ra = do
    gv <- G.new
    -- Vector.fromList . toList <$> go Seq.empty ra
    go gv ra
    G.unsafeToVector gv
    where
    go gv = \case
        RenderFromAtlas   d    -> G.snoc gv d
        -- RenderComposition t rs -> mapM_ (go gv) (T.transform t rs)
        RenderComposition t rs -> mapM_ (go gv) rs -- (T.transform t rs)
        _                      -> return ()

unsafeSortZ
    :: Vector (AtlasLocation, AtlasDesc)
    -> IO (Vector (AtlasLocation, AtlasDesc))
unsafeSortZ i = do
    v <- Vector.unsafeThaw i
    let f (_, a) = a^.zindex
    let e = 0 :: Word32
    M.sortBy (M.passes e) (M.size e) (\i a -> M.radix i (f a)) v
    Vector.unsafeFreeze v

batchItemsFromAtlas
    :: TextureAtlas -> Vector AtlasDesc -> IO (Storable.Vector AtlasBatchItem)
batchItemsFromAtlas atlas vv = do
    texLookup <- getLookupAtlasLocation atlas
    let toImgOut :: AtlasDesc -> Maybe (AtlasLocation, AtlasDesc)
        toImgOut a = (,a) <$> (texLookup =<< a^.textureId)

    v <- unsafeSortZ $ Vector.mapMaybe toImgOut vv
    let l = Vector.length v
    let dmax = 8388608 -- 2^23
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
                let r = makeAtlasTexCoords maxTexSize loc (a^.ff#part)
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

runWrite :: St -> IO ()
runWrite st =
    evaluateNF_
        =<< batchItemsFromAtlas (st^.ff#atlas)
        =<< batchRenderAction (st^.ff#elems)

runSt :: St -> IO ()
runSt st = do
    -- evaluateNF_ =<< batchItemsFromAtlas 32 (st^.ff#atlas) (st^.ff#elems)
    let cx = field_context st
    withBuffers cx $ do
        let projM = field_projMat st

        (st^.ff#draw) True projM =<< batchRenderAction (st^.ff#elems)
        -- (st^.ff#draw) True  projM (st^.ff#elemsBlend)

initDrawCall :: TextureAtlas -> IO DrawCall
initDrawCall atlas = do
    -- drawCall <- makeInitDrawCall vertexShader fragmentShader toItems
    drawCall <- makeInitDrawCall vertexShader fragmentShader

    return $ \blend proj fullBatch -> do
    {-
        texLookup <- getLookupAtlasLocation atlas
        let maxTexSize = fromIntegral $ atlas^.maxTextureSize
        let toR x = toCustomElemR maxTexSize x <$> texLookup (x^.texture)
    -}

        fullBatchR <- batchItemsFromAtlas atlas fullBatch
        -- let fullBatchR = Vector.mapMaybe toR fullBatch
        logOnce $ show $ (div (Vector.length fullBatchR) 4)
        -- let fullBatchR = fullBatch
        -- let fullBatchR = mapElems fullBatch
        glDisable GL_DEPTH_TEST
        when blend $ do
            -- glDepthMask GL_FALSE
            glEnable GL_BLEND
            glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
            glBlendEquation GL_FUNC_ADD
            -- glBlendFuncSeparate
                -- GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                -- GL_ONE       GL_ONE_MINUS_SRC_ALPHA

        drawCall fullBatchR $ \program -> do
            setupAtlas atlas program
            setUniform program "ProjectionMatrix" proj

        -- glDepthMask GL_TRUE
        -- glDisable GL_BLEND
    where
    {-
    toItems :: CustomElemR -> Vector CustomVertexItem
    -- toItems :: CustomElem -> Vector CustomVertexItem
    toItems c = Vector.fromList $ map (\tc -> CustomVertexItem
    -- toItems c = Vector.replicate 4 CustomVertexItem
        { field_zindex = c^.ff#zindex
        , field_modelX = V3 s 0 (c^.ff#position._x)
        , field_modelY = V3 0 s (c^.ff#position._y)
        , field_color  = c^.ff#color
        -- , field_texCoord = V3 0 0 0
        -- }
        , field_texCoord = tc
        }) [c^.ff#texCoord0, c^.ff#texCoord1, c^.ff#texCoord2, c^.ff#texCoord3]
        -- }) [c^.ff#color, c^.ff#color, c^.ff#color, c^.ff#color]
        -- }) (c^.ff#texCoords)
        where s = squareSize
    -}

    vertexShader = List.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in vec3 TexCoord;"
        , "in vec3 Color;"
        , "in vec3 ModelX;"
        , "in vec3 ModelY;"
        , "in float ZIndex;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec3 vColor;"
        , "out highp vec3 vTexCoord;"
        , "void main(void){"
        , "  mat4 MM = mat4(1.0);"
        , "  MM[0] = vec4(ModelX.xy, 0, ModelX.z);"
        , "  MM[1] = vec4(ModelY.xy, 0, ModelY.z);"
        , "  mat4 Matrix = ProjectionMatrix * transpose(MM);"
        , "  gl_Position = Matrix * vec4(VertexPosition, 0, 1);"
        , "  gl_Position.z = ZIndex;"
        , "  vColor = Color;"
        , "  vTexCoord = TexCoord;"
        , "}" ]

    maxPrimaryPages = 16 :: Int
    fragmentShader = List.unlines $ shaderVersion :
        [ "#define MaxPrimaryPages " <> (show maxPrimaryPages)
        , "uniform sampler2D Primary[MaxPrimaryPages];"
        , "in highp vec3 vTexCoord;"
        , "in highp vec3 vColor;"
        , "out vec4 FragColor;"
        , "void main(void){"
        , "  int page = clamp(int(floor(vTexCoord.z)), 0, MaxPrimaryPages-1);"
        , "  vec4 color;"
        , "  color = texture2D(Primary[page], vTexCoord.xy);"
        -- , "  if (color.a == 0) discard; "
        -- , "  if (color.a < 1.0) discard; "
        , "  FragColor = color;"
        -- , "  FragColor.rgb = color.rgb;"
        -- , "  FragColor.rgb = vec3(1, 0, 0);"
        -- , "  FragColor.rgb = vColor.rgb;"
        -- , "  FragColor.a   = 0.3;"
        , "}" ]

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
