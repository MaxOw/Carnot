{-# Language TypeFamilies #-}
{-# Language DuplicateRecordFields #-}
{-# Options_GHC -fno-warn-orphans #-}
module Experiments.Current where

import Delude hiding (identity)
import Linear
import Data.List ((!!))
import qualified Relude.Unsafe as Unsafe
import Engine.Lens.Utils
-- import qualified Data.Sequence as Seq
import qualified Diagrams.TwoD.Transform as T

import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Context as Context

import Criterion.Main
import Experiments.Common

import Engine.Common.Types
import Engine.Graphics
    (renderImg, renderComposition, batchRenderAction)
import Engine.Graphics.Utils
-- import Engine.Graphics.Draw.Atlas
import Engine.Graphics.TextureAtlas

--------------------------------------------------------------------------------

bench_current :: Int -> Int -> Benchmark
bench_current squareSize count
    = mkBench "current" (initSt squareSize count) runSt

--------------------------------------------------------------------------------

data St = St
   { field_context    :: Context
   , field_elems      :: RenderAction
   -- , field_elems      :: !(Seq AtlasDesc)
   -- , field_draw       :: DrawBatch AtlasBatch
   -- , field_draw       :: DrawProcedure
   , field_projMat    :: Mat4
   } deriving (Generic)
instance NFData RenderAction where rnf _ = ()
instance NFData St
instance NFData AtlasDesc where rnf _ = ()

initSt :: Int -> Int -> IO St
initSt squareSize count = do
    cx <- Context.initWindow "draw04" (400, 400) False
    atlas <- newAtlas

    img <- Unsafe.fromJust <$> loadImgToAtlas atlas "imgs/dirt.png"
    fullUpdate atlas

    canvasSize <- Context.getFramebufferSize cx
    let (w, h) = over each fromIntegral canvasSize

    -- dc <- initDrawAtlas
    GLFW.swapInterval 0
    GLFW.showWindow cx

    let is = renderComposition $ map (mkI img w h) [0..count-1]

    -- proc <- initDrawProcedure atlas
    projM <- orthoMat cx
    return $ St
        { field_context    = cx
     -- , field_elems      = Seq.fromList $ take count $ map (mkA img w h) [0..]
        , field_elems      = is
        -- , field_draw       = dc atlas
        -- , field_draw       = undefined -- proc
        , field_projMat    = projM
        }
    where
    mkI img w h i = renderImg (img & part .~ (Just $ selectPart i))
        & T.scale (ss / 32)
        & T.translate pos
    {-
    -- mkA :: Img -> Int -> AtlasDesc
    mkA img w h i = def
        & textureId .~ (Just $ img^.texture)
        & part      .~ (Just $ selectPart i)
        & color     .~ (img^.color)
        & colorMix  .~ (img^.colorMix)
        & modelTransform .~ (mempty & T.scale ss & T.translate pos)
    -}
        where
        pos = V2 (wx - w/2 + ss/2) (hx - h/2 + ss/2)
        ss = fromIntegral squareSize
        ww = ceiling $ w/ss
        hh = ceiling $ h/ss
        wx = ss * (fromIntegral $ (xx - hn*ww))
        hx = ss * (fromIntegral $ hn)
        hn = div xx ww
        xx = mod i (ww*hh)

    selectPart i =
        [ mkP 0 0, mkP 1 0, mkP 2 0
        , mkP 0 1, mkP 1 1, mkP 2 2
        , mkP 0 2 ] !! (mod i 7)
    mkP x y = Rect (32 *^ (V2 x y)) (pure 32)

runSt :: St -> IO ()
runSt st = do
    let cx = field_context st
    withBuffers cx $ do
        -- let projM = field_projMat st
        -- es <- batchRenderAction (st^.ff#elems)
        -- (st^.ff#draw) projM es
        return ()

