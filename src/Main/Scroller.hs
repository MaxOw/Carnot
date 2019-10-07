module Main.Scroller (runMain) where

import Delude
import Engine.Debug (resetDebugLog)
import Engine hiding (Scroller)
import Engine.Graphics.Utils (mkMatHomo2)
import Linear
import Reload.Utils (reacquire)
import qualified Diagrams.TwoD.Transform as T

import Engine.Lens.Utils (ff)
import Engine.Graphics.Scroller (testCopyOffset)
import qualified Data.Colour as Color
import qualified Data.Colour.Names as Color
import qualified Engine.Graphics.Scroller.Cells as Scroller
import Engine.Graphics.Scroller.TypesCells (Scroller)

--------------------------------------------------------------------------------

data St = St
   { field_renderCopy :: RenderAction
   , field_position   :: V2 Float
   , field_scroller   :: Scroller
   , field_testTex    :: Maybe Img
   } deriving (Generic)

--------------------------------------------------------------------------------

handleEvent :: EventHandler St
handleEvent = \case
    -- EventKey Key'Q _ _ _ -> closeWindow
    EventKey k _ _ _ -> case k of
        Key'J -> move ( 0, -1)
        Key'K -> move ( 0,  1)
        Key'H -> move (-1,  0)
        Key'L -> move ( 1,  0)
        _     -> return ()
    _ -> return ()

move :: (Float, Float) -> Engine St ()
move (x, y) = userState.position += (1 *^ V2 x y)
-- move (x, y) = userState.position += (0.15 *^ V2 x y)

viewScale = 64 -- 64

render :: Renderer St
render _delta s = do
    let renderBarrel = maybe mempty renderImg $ s^.ff#testTex

    let scr = s^.ff#scroller
    pos <- use $ userState.position
    {-
    updateScroller scr 1 pos 64 $ \bb -> do
        return $ renderSquare Color.red
            & T.scaleX 64
            & T.scaleY 64
            -- & T.translate pos
    -}

    let spos = negate <$> pos^*viewScale
    let viewM = mkMatHomo2 (T.translate spos mempty)
    projM <- orthoProjection $ def

    Scroller.update scr pos $ \bb -> do
        return $ renderComposition
            [ renderSquare Color.red
            , renderSquare Color.red & T.translate 4
            , renderSquare Color.blue & T.translateX 8
            ]
            -- & T.scaleX 64
            -- & T.scaleY 64
            -- & T.translate 100
        -- return mempty

    fitViewport
    clearColorWhite
    let viewM = mkMatHomo2 (T.translate spos mempty)
    projM <- orthoProjection $ def
        & set scale 0.3
        -- & boxAlign .~ Center
    -- draw projM $ s^.ff#renderCopy
    let projViewM = projM !*! viewM
    -- ra <- makeRenderScroller scr
    -- draw projViewM ra
    ra <- Scroller.makeRenderAction scr
    draw projViewM ra
    swapBuffers

    where

    renderSquare c
        = renderShape $ def
            & shapeType .~ SimpleSquare
            & color     .~ (Color.opaque c)

initialize :: Engine () St
initialize = do
    scr <- Scroller.new $ def & set scale viewScale
    atlas <- use $ graphics.textureAtlas
    ra <- testCopyOffset atlas
    mtex0 <- loadTextureToAtlas "imgs/barrel_E.png"
    return $ St
        { field_renderCopy = ra
        , field_position   = 0
        , field_scroller   = scr
        , field_testTex    = mtex0
        }

runMain :: IO ()
runMain = do
    ctx <- reacquire 0 $ initWindow "Test" (800, 600) True

    -- diffTest

    igniteEngine ctx $ Ignition
        { initializer  = initialize
        , eventHandler = handleEvent
        , integrator   = const $ return ()
        , renderer     = render
        , finalizer    = resetDebugLog
        }
