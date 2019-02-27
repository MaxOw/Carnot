{-# Language TemplateHaskell #-}
{-# Language StrictData      #-}
module Engine.Types where

import Protolude
import Control.Lens          (makeLenses)

-- import Engine.Context        (Context)
import Engine.Events.Types   (Event, EventQueue)
import Engine.Graphics.Types (GraphicsState)

-- type Time  = Double
type Delta = Double

-- type Engine us a = ReaderT (EngineState us) IO a
type Engine us a = StateT  (EngineState us) IO a

type EventHandler us = Event -> Engine us ()
type Integrator   us = Delta -> Engine us ()
type Renderer     us = Delta -> us -> Engine us ()

data Ignition userState = Ignition
   { initializer  :: Engine ()    userState
   , eventHandler :: EventHandler userState
   , integrator   :: Integrator   userState
   , renderer     :: Renderer     userState
   }

data EngineState userState = EngineState
   { _userState     :: userState
   , _eventQueue    :: EventQueue
   -- , _context       :: Context
   -- , _canvasSize    :: Vec2
   , _graphics      :: GraphicsState -- userGraphicsState
   }
makeLenses ''EngineState

