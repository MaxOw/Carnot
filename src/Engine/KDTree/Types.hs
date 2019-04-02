module Engine.KDTree.Types where

import Delude
import Linear (V2)
import Data.Vector (Vector)

type KDEntry  a = (V2 Float, a)
type KDVector a = Vector (KDEntry a)

data KDDim = KD_X | KD_Y
    deriving (Generic)
instance NFData KDDim

data KDNode t = KDNode
   { field_smaller  :: Maybe t
   , field_pivot    :: Float
   , field_greater  :: Maybe t
   , field_pivotDim :: KDDim
   } deriving (Generic)
instance NFData t => NFData (KDNode t)

data KDTree a
   = KDTree_Node (KDNode (KDTree  a))
   | KDTree_Leaf (KDVector a)
   deriving (Generic)
instance NFData a => NFData (KDTree a)
