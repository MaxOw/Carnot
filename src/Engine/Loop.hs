module Engine.Loop where

import Relude
import Control.Monad.Catch (finally)
import Control.Concurrent (threadDelay)
import Control.Lens hiding (Context)

import System.Mem

import Engine.Types
import Engine.Events
import Engine.Context
import Engine.Graphics
import Criterion.Measurement (secs)

import qualified Engine.Graphics.TextureAtlas as Atlas
import qualified Engine.Graphics.TaskManager as TaskManager
import qualified Engine.Graphics.DrawBatchCache as DrawBatchCache

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
    -- maxFullTime      = 0.0165 -- 0.0165 * 60 = 0.99
    maxFullTime      = 0.016  -- 0.016 * 60 = 0.94

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
        -- let calcTimeLeft = (\t -> maxFullTime - t + startTime) <$> getTime
        let calcTimeLeft = (\t -> maxFullTime - t + startTime) <$> getTime
        let timesUp = (< 0) <$> calcTimeLeft

        tl <- calcTimeLeft
        when (tl<0) $ putStrLn $ "Spike: " <> (secs $ abs $ realToFrac tl)

        -- when (tl>0.005) $ liftIO $ performMinorGC
        {-
        when (tl > 0.005) $ if tl > 0.010
            then liftIO $ performMajorGC
            else liftIO $ performMinorGC
        -}

        -- perform incremental updates on scheduled tasks until time's up.
        incrementalUpdates timesUp

        DrawBatchCache.endFrame =<< use (graphics.drawBatchCache)

        -- sleep for the rest of the frame
        sleepTime <- calcTimeLeft
        unlessM timesUp $ do
            -- putStrLn $ "Sleep: " <> (secs $ abs $ realToFrac sleepTime)
            threadDelaySeconds sleepTime


        close <- windowShouldClose ctx
        unless close $ loop acc' startTime

withTime :: Engine us () -> Engine us ()
withTime act = do
    start <- getTime
    act
    end <- getTime
    putStrLn . secs . realToFrac $ end - start

mainLoopWithCleanup :: Ignition us -> Engine us ()
mainLoopWithCleanup ignition = mainLoop ignition `finally` cleanup
    where
    cleanup = do
        finalizer ignition
        atlas <- use $ graphics.textureAtlas
        Atlas.done atlas

incrementalUpdates :: Engine us Bool -> Engine us ()
incrementalUpdates timesUp = do
    -- atlas <- use $ graphics.textureAtlas
    -- Atlas.incrementalUpdate atlas timesUp
    tm <- use $ graphics.taskManager
    TaskManager.performUntil tm timesUp

threadDelaySeconds :: MonadIO m => Float -> m ()
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

    let initialEngineState = EngineState
            { _userState  = ()
            , _eventQueue = initialEventQueue
            , _graphics   = initialGraphicsState
            }

    (st, engineState) <- runStateT (initializer ignition) $ initialEngineState
    evalStateT (mainLoopWithCleanup ignition) $ engineState { _userState = st }
