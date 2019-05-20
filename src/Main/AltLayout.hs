module Main.AltLayout (runMain) where

import Delude
import Engine
import Reload.Utils (reacquire)

import Engine.Lens.Utils (ff)
import Engine.Layout.Alt.Examples
import Engine.Layout.Alt (Layout, drawLayout)

--------------------------------------------------------------------------------

data St = St
   { field_layoutToDisplay :: Layout
   , field_layoutIx        :: Int
   } deriving (Generic)
instance Default St

handleEvent :: EventHandler St
handleEvent = \case
    EventKey Key'Q     _ _                _ -> closeWindow
    EventKey Key'Space _ KeyState'Pressed _ -> nextExample
    _ -> return ()
    where
    nextExample = do
        i <- userState.ff#layoutIx <%= limitExampleIx . (+1)
        userState.ff#layoutToDisplay .= selectExample i

render :: Renderer St
render _delta s = do
    fitViewport
    clearColorWhite
    drawLayout $ s^.ff#layoutToDisplay
    swapBuffers

runMain :: IO ()
runMain = do
    ctx <- reacquire 0 $ initWindow "Test" (800, 600)

    igniteEngine ctx $ Ignition
        { initializer  = initialize
        , eventHandler = handleEvent
        , integrator   = const $ return ()
        , renderer     = render
        , finalizer    = return ()
        }

--------------------------------------------------------------------------------

initialize :: Engine () St
initialize = do
    void $ loadFontFamily "SourceHanSerif" $ def
        & fontBase       .~ "fonts/SourceHanSerif-Regular.otf"

    void $ loadFontFamily "Arial" $ def
        & fontBase       .~ "fonts/Arial.ttf"
        & fontBold       .~ Just "fonts/Arial-Bold.ttf"
        & fontBoldItalic .~ Just "fonts/Arial-Bold-Italic.ttf"
        & fontItalic     .~ Just "fonts/Arial-Italic.ttf"

    return $ def
        -- & ff#layoutToDisplay .~ box1
        -- & ff#layoutToDisplay .~ boxRow1
        -- & ff#layoutToDisplay .~ boxRowSep1
        -- & ff#layoutToDisplay .~ boxRowRel1
        -- & ff#layoutToDisplay .~ complex1
        -- & ff#layoutToDisplay .~ margins1
        -- & ff#layoutToDisplay .~ uniflex1
        -- & ff#layoutToDisplay .~ text1
        & ff#layoutToDisplay .~ textPadding1
        -- & ff#layoutToDisplay .~ bigtext1
        -- & ff#layoutToDisplay .~ offi

