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

initWindow :: String -> (Int, Int) -> IO Context
initWindow title (winWidth, winHeight) = do
    GLFW.setErrorCallback (Just stderrErrorCallback)
    successfulInit <- GLFW.init
    vs <- fromMaybe "" <$> GLFW.getVersionString
    putStrLn vs
    -- if init failed, we exit the program
    if not successfulInit then error "GLFW Init failed" else do
        mw <- tryOpenWindow 4 5 True
        case mw of
            Nothing -> do
                GLFW.terminate
                error "Window creation failed"
            Just win -> do
                GLFW.makeContextCurrent mw
                -- GLFW.swapInterval 1 --vsync
                GLFW.swapInterval 0 --vsync off
                return win

    where
    tryOpenWindow :: Int -> Int -> Bool -> IO (Maybe Context)
    tryOpenWindow majVersion minVersion forwardCompat = do
        printf
            "Opening Window (ContextVersoin %d.%d, ForwardCompatiblity = %s)\n"
            majVersion minVersion (show forwardCompat :: String)

        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor majVersion
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor minVersion
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat forwardCompat
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
        GLFW.windowHint $ GLFW.WindowHint'Visible False

        mpri <- GLFW.getPrimaryMonitor
        mmod <- maybe (return Nothing) GLFW.getVideoMode mpri
        -- print mmod
        let w = fromMaybe winWidth  $ GLFW.videoModeWidth  <$> mmod
        let h = fromMaybe winHeight $ GLFW.videoModeHeight <$> mmod
        mwin <- GLFW.createWindow w h title Nothing Nothing
        return mwin

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

