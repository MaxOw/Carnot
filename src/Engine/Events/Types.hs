{-# Language StrictData #-}
module Engine.Events.Types
    ( Event(..)
    , EventQueue
    , MouseButton(..)
    , MouseButtonState(..)
    , Key(..)
    , KeyState(..)
    , ModifierKeys(..)
    , defaultModifierKeys
    ) where

import Protolude
import Graphics.UI.GLFW
import Control.Concurrent.STM (TQueue)

-- type Event = EventWith ()

data Event -- With ue
-- | EventError           Error String
   = EventWindowPos       Int Int
   | EventWindowSize      Int Int
   | EventWindowClose
   | EventWindowRefresh
-- | EventWindowFocus     FocusState
-- | EventWindowIconify   IconifyState
   | EventFramebufferSize Int Int
   | EventMouseButton     MouseButton MouseButtonState ModifierKeys
   | EventCursorPos       Float Float
-- | EventCursorEnter     CursorState
   | EventScroll          Double Double
   | EventKey             Key Int KeyState ModifierKeys
   | EventChar            Char
-- | EventUser            ue
   deriving Show

-- type EventQueue ue = TQueue (Event ue)
type EventQueue = TQueue Event

defaultModifierKeys :: ModifierKeys
defaultModifierKeys = ModifierKeys False False False False
