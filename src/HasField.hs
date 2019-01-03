module HasField where

import Control.Lens (Lens')

class HasBuffer s a | s -> a where buffer :: Lens' s a
class HasSize   s a | s -> a where size   :: Lens' s a

class HasWidth  s a | s -> a where width  :: Lens' s a
class HasHeight s a | s -> a where height :: Lens' s a
