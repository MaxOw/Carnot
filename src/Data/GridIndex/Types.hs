{-# Language TemplateHaskell #-}
module Data.GridIndex.Types where

import Delude
import Linear (V2)
import Data.Array.IO (IOArray)
import Engine.Common.Types

data GridIndexConfig = GridIndexConfig
   { field_gridSize :: Size Int
   , field_cellSize :: Size Float
   } deriving (Eq, Show, Generic)
instance Default GridIndexConfig
instance NFData GridIndexConfig

data GridIndex a = GridIndex
   { _config  :: GridIndexConfig
   , _grid    :: IOArray (V2 Int) [a]
   }
makeLenses ''GridIndex

