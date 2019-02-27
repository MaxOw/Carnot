{-# Language TemplateHaskell #-}
module Engine.Loop where

import Protolude
import Control.Lens hiding (Context)

import Engine.Types
import Engine.Events
import Engine.Context
import Engine.Graphics

import qualified Engine.Graphics.TextureAtlas as Atlas

{-
modifyUserState :: (us -> us) -> Engine us ()
modifyUserState f = do
    us <- view userState
    liftIO $ atomicModifyIORef' us $ \x -> (f x, ())

updateUserState :: State us a -> Engine us a
updateUserState f = do
    us <- view userState
    liftIO $ atomicModifyIORef' us $ swap . runState f

readUserState :: Engine us us
readUserState = liftIO . readIORef =<< view userState
-}

-- simpleIntegrator :: (Delta -> us -> us) -> Integrator us
-- simpleIntegrator f delta = modifyUserState (f delta)

-- simpleEventHandler :: (Event -> us -> us) -> EventHandler us
-- simpleEventHandler f event = modifyUserState (f event)

mainLoop :: Ignition us -> Engine us ()
mainLoop ignition = do
    -- initializer ignition
    loop 0 =<< getTime
    where
    -- default Config values
    integrationDelta = 0.01
    maxFrameTime     = 0.25
    maxFullTime      = 0.016 -- 0.016 * 60 = 0.96

    loop accumulator lastTime = do
        -- Calc Time Variables -------------------------------------------------
        startTime <- getTime
        let frameTime = startTime - lastTime
        let acc  = accumulator + min frameTime maxFrameTime
        let n    = fst . properFraction $ acc / integrationDelta :: Int
        let acc' = acc - (fromIntegral n)*integrationDelta

        -- Handle events -------------------------------------------------------
        pollEvents
        procEvents $ eventHandler ignition

        -- Integrate -----------------------------------------------------------
        replicateM_ n $ integrator ignition integrationDelta

        -- Render --------------------------------------------------------------
        ctx <- use $ graphics.context
        renderer ignition (acc' / integrationDelta) =<< use userState

        -- Sleep ---------------------------------------------------------------
        -- calculate time left in a frame in seconds :: Float
        let calcTimeLeft = (\t -> maxFullTime - t + startTime) <$> getTime
        let timesUp = (< 0) <$> calcTimeLeft

        -- perform incremental updates on scheduled tasks until time's up.
        incrementalUpdates timesUp
        -- sleep for the rest of the frame
        sleepTime <- calcTimeLeft
        unlessM timesUp $ threadDelaySeconds sleepTime

        close <- windowShouldClose ctx
        unless close $ loop acc' startTime

incrementalUpdates :: Engine us Bool -> Engine us ()
incrementalUpdates timesUp = do
    atlas <- use $ graphics.textureAtlas
    Atlas.incrementalUpdate atlas timesUp

threadDelaySeconds :: MonadIO m => Double -> m ()
threadDelaySeconds = liftIO . threadDelay . floor . (1e6*)

closeWindow :: Engine us ()
closeWindow = do
    ctx <- use $ graphics.context
    setWindowShouldClose ctx True

igniteEngine :: Context -> Ignition us -> IO ()
igniteEngine ctx ignition = do
    makeContextCurrent (Just ctx)
    initialEventQueue    <- initEvents ctx Nothing
    initialGraphicsState <- initGraphics ctx

    let makeEngineState st = EngineState
            { _userState  = st
            , _eventQueue = initialEventQueue
            , _graphics   = initialGraphicsState
            }

    st <- evalStateT (initializer ignition) $ makeEngineState ()
    evalStateT (mainLoop ignition) $ makeEngineState st
