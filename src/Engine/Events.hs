module Engine.Events
   ( module Types
   , initEvents
   , pollEvents
   , procEvents
   ) where

import Protolude
import Control.Lens (use)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
    ( newTQueueIO, writeTQueue, TQueue, tryReadTQueue, atomically )

import Graphics.UI.GLFW hiding (pollEvents)
import qualified Graphics.UI.GLFW as GLFW

import Engine.Context (Context)
import Engine.Types (EventHandler, Engine, eventQueue)
import Engine.Events.Types as Types

initEvents :: Context -> Maybe EventQueue -> IO EventQueue
initEvents win mq = do
    evq <- maybe newTQueueIO pure mq
    let fire = atomically . writeTQueue evq
    let fire0 s e = s win . Just . const $             fire   e
    let fire1 s e = s win . Just . const $ \a       -> fire $ e a
    let fire2 s e = s win . Just . const $ \a b     -> fire $ e a b
    let fire3 s e = s win . Just . const $ \a b c   -> fire $ e a b c
    let fire4 s e = s win . Just . const $ \a b c d -> fire $ e a b c d

    fire2 setWindowPosCallback       EventWindowPos
    fire2 setWindowSizeCallback      EventWindowSize
    fire0 setWindowCloseCallback     EventWindowClose
    fire0 setWindowRefreshCallback   EventWindowRefresh
    fire1 setWindowFocusCallback     EventWindowFocus
 -- fire1 setWindowIconifyCallback   EventWindowIconify
    fire2 setFramebufferSizeCallback EventFramebufferSize
    fire3 setMouseButtonCallback     EventMouseButton
 -- fire2 setCursorPosCallback       EventCursorPos
 -- fire1 setCursorEnterCallback     EventCursorEnter
    fire4 setKeyCallback             EventKey
    fire1 setCharCallback            EventChar

    setScrollCallback win $ Just $ \_ x y ->
        fire $ EventScroll (realToFrac x) (realToFrac y)

    setCursorPosCallback win $ Just $ \_ x y ->
        fire $ EventCursorPos (realToFrac x) (realToFrac y)

    return evq

pollEvents :: MonadIO m => m ()
pollEvents = liftIO GLFW.pollEvents

procEvents :: EventHandler us -> Engine us ()
procEvents handler = mapM_ handler =<< collectTQueueIO =<< use eventQueue

--------------------------------------------------------------------------------

collectTQueueIO :: MonadIO m => TQueue a -> m [a]
collectTQueueIO ch = do
    v <- liftIO . atomically $ tryReadTQueue ch
    case v of
        Nothing -> return []
        Just a  -> (a :) <$> collectTQueueIO ch

