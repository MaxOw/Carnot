module Engine.Context
    ( Context
    , getTime
    , initWindow
    , makeContextCurrent
    , getPrimaryDPI
    , setWindowShouldClose
    , windowShouldClose
    , swapBuffers
    , getFramebufferSize
    ) where

import Delude
import Prelude (String)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import System.IO (stdout, hPutStrLn)

type Context = GLFW.Window

getTime :: MonadIO m => m Float
getTime = realToFrac . fromJust <$> liftIO GLFW.getTime

initWindow :: String -> (Int, Int) -> Bool -> IO Context
initWindow title (winWidth, winHeight) visible = do
    GLFW.setErrorCallback (Just stderrErrorCallback)
    successfulInit <- GLFW.init
    putStrLn =<< fromMaybe "" <$> GLFW.getVersionString
    if not successfulInit then error "GLFW Init failed" else do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 5
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
        GLFW.windowHint $ GLFW.WindowHint'Visible visible

        mpri <- GLFW.getPrimaryMonitor
        mmod <- maybe (return Nothing) GLFW.getVideoMode mpri
        -- print mmod
        let w = fromMaybe winWidth  $ GLFW.videoModeWidth  <$> mmod
        let h = fromMaybe winHeight $ GLFW.videoModeHeight <$> mmod
        mwin <- GLFW.createWindow w h title Nothing Nothing
        case mwin of
            Nothing -> do
                GLFW.terminate
                error "Window creation failed"
            Just win -> do
                GLFW.makeContextCurrent mwin
                GLFW.swapInterval 0 --vsync off
                return win
    where
    stderrErrorCallback :: GLFW.ErrorCallback
    -- stderrErrorCallback _ = hPutStrLn stderr
    stderrErrorCallback _ = hPutStrLn stdout

makeContextCurrent :: MonadIO m => Maybe Context -> m ()
makeContextCurrent = liftIO . GLFW.makeContextCurrent

getPrimaryDPI :: MonadIO m => m (Maybe Int)
getPrimaryDPI = do
    mPrimary <- liftIO GLFW.getPrimaryMonitor
    mMode <- join <$> mapM (liftIO . GLFW.getVideoMode) mPrimary
    caseJust ((,) <$> mPrimary <*> mMode) $ \(prim, mode) -> do
        (w, _) <- liftIO $ GLFW.getMonitorPhysicalSize prim
        let modeWidth = fromIntegral $ GLFW.videoModeWidth mode
        let widthMM = fromIntegral w
        let mmToInch = (/(25.4 :: Float))
        let widthInch = mmToInch widthMM
        return $ Just $ floor $ (modeWidth/widthInch)

setWindowShouldClose :: MonadIO m => Context -> Bool -> m ()
setWindowShouldClose ctx val = liftIO $ GLFW.setWindowShouldClose ctx val

windowShouldClose :: MonadIO m => Context -> m Bool
windowShouldClose ctx = liftIO $ GLFW.windowShouldClose ctx

swapBuffers :: MonadIO m => Context -> m ()
swapBuffers ctx = liftIO $ GLFW.swapBuffers ctx

getFramebufferSize :: MonadIO m => Context -> m (Int, Int)
getFramebufferSize ctx = liftIO $ GLFW.getFramebufferSize ctx

