module Engine.HasField where

import Control.Lens (Lens')

class HasBuffer s a | s -> a where buffer :: Lens' s a
class HasSize   s a | s -> a where size   :: Lens' s a

class HasOffset s a | s -> a where offset :: Lens' s a
class HasWidth  s a | s -> a where width  :: Lens' s a
class HasHeight s a | s -> a where height :: Lens' s a

class HasPart   s a | s -> a where part   :: Lens' s a
class HasZindex s a | s -> a where zindex :: Lens' s a
