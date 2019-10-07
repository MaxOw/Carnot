module Main where

import Protolude hiding ((&))
-- import Data.Default.Class
import Control.Lens
import Graphics.GL
import Data.Default

-- import Engine.Graphics.Utils (mkMatHomo2)
-- import qualified Diagrams.TwoD.Transform as Transform
-- import Diagrams.Angle ((@@), rad)

import Engine
import Linear

import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.FTGL (Font)

integrate :: Integrator us
-- integrate = simpleIntegrator $ \delta state -> worldUpdate state
-- integrate = simpleIntegrator $ \_delta s -> s
integrate _ = return ()

handleEvent :: EventHandler us
handleEvent = \case
    EventKey Key'Q _ _ _ -> closeWindow
    _                    -> return ()

render :: Renderer Font
render _delta s = do
    fitViewport
    glClearColor 0 0 0 0
    glClear GL_COLOR_BUFFER_BIT

    let font = s
    liftIO $ FTGL.renderFont font "Test render" FTGL.All

    projM <- orthoProjection
    draw projM $ renderShape def

    swapBuffers
    where
    -- modelM = mkMatHomo2 $ Transform.scale 0.2 mempty

fitViewport :: Engine us ()
fitViewport = do
    (w, h) <- getFramebufferSize =<< use (graphics.context)
    glViewport 0 0 (fromIntegral w) (fromIntegral h)

orthoProjection :: Engine us Mat4
orthoProjection = do
    canvasSize <- getFramebufferSize =<< use (graphics.context)
    let (w, h) = over each fromIntegral canvasSize
    -- graphics.canvasSize .= V2 (fromIntegral w) (fromIntegral h)
    -- V2 w h <- views (graphics.canvasSize) $ over each realToFrac
    let a = w/h -- (h*s)
    let b = 1 -- /s
    return $ ortho (-a) a (-b) b 0 1

main :: IO ()
main = do
    ctx <- initWindow "Test" (800, 600) False
    f <- font_test
    let initialState = f
    igniteEngine ctx initialState $ Ignition
        { eventHandler = handleEvent
        , integrator   = integrate
        , renderer     = render
        }

--------------------------------------------------------------------------------

font_test :: IO Font
font_test = do
    font <- FTGL.createBufferFont "Arial.ttf"
    void $ FTGL.setFontFaceSize font 72 72
    return font

