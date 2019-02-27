module Data.SpatialIndex.Types where

import Delude
import Data.GridIndex.Types
import Data.QuadTree.Types

type HashSetIndex a = IORef (HashSet a)
type QuadTreeIndex a = IORef (QuadTree a)

data SpatialIndexConfig
   = SpatialIndexConfig_Grid     GridIndexConfig
   | SpatialIndexConfig_QuadTree QuadTreeConfig
   | SpatialIndexConfig_HashSet

data SpatialIndex a
   = SpatialIndex_Grid (GridIndex a)
   | SpatialIndex_QuadTree (QuadTreeIndex a)
   | SpatialIndex_HashSet (HashSetIndex a)
   -- | SpatialIndex_SparseGrid (SparseGridIndex a)
   -- | SpatialIndex_KDTree (KDTree a)
