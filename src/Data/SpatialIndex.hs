module Data.SpatialIndex
    ( SpatialIndex
    , SpatialIndexConfig (..)

    , create
    , createGrid, createQuadTree, createHashSet
    , insert
    , delete
    , move
    , lookup
    ) where

import Delude
import Engine.Common.Types (BBox)
import qualified Data.HashSet as HashSet
import Data.SpatialIndex.Types
import Data.GridIndex.Types (GridIndexConfig)
import Data.QuadTree.Types  (QuadTreeConfig)
import qualified Data.GridIndex as GridIndex
import qualified Data.QuadTree as QuadTree

--------------------------------------------------------------------------------

create :: MonadIO m => SpatialIndexConfig -> m (SpatialIndex a)
create = \case
    SpatialIndexConfig_Grid c ->
        SpatialIndex_Grid <$> GridIndex.create c
    SpatialIndexConfig_QuadTree c ->
        SpatialIndex_QuadTree <$> newIORef (QuadTree.empty c)
    SpatialIndexConfig_HashSet ->
        SpatialIndex_HashSet <$> newIORef HashSet.empty

createGrid :: MonadIO m => GridIndexConfig -> m (SpatialIndex a)
createGrid = create . SpatialIndexConfig_Grid

createQuadTree :: MonadIO m => QuadTreeConfig -> m (SpatialIndex a)
createQuadTree = create . SpatialIndexConfig_QuadTree

createHashSet :: MonadIO m => m (SpatialIndex a)
createHashSet = create SpatialIndexConfig_HashSet

--------------------------------------------------------------------------------

insert :: (MonadIO m, Hashable a, Ord a)
    => V2 Float -> a -> SpatialIndex a -> m ()
insert p a = \case
    SpatialIndex_Grid     g -> GridIndex.insert p a g
    SpatialIndex_QuadTree q -> modifyIORef q (QuadTree.insert p a)
    SpatialIndex_HashSet  h -> modifyIORef h (HashSet.insert a)

delete :: (MonadIO m, Hashable a, Ord a)
    => V2 Float -> a -> SpatialIndex a -> m ()
delete p a = \case
    SpatialIndex_Grid     g -> GridIndex.delete p a g
    SpatialIndex_QuadTree q -> modifyIORef q (QuadTree.delete p a)
    SpatialIndex_HashSet  h -> modifyIORef h (HashSet.delete a)

move :: (MonadIO m, Hashable a, Ord a)
    => V2 Float -> V2 Float -> a -> SpatialIndex a -> m ()
move p0 p1 a = \case
    SpatialIndex_Grid     g -> GridIndex.move p0 p1 a g
    SpatialIndex_QuadTree q -> modifyIORef q (QuadTree.move p0 p1 a)
    SpatialIndex_HashSet  _ -> return ()

lookup :: MonadIO m => BBox Float -> SpatialIndex a -> m [a]
lookup b = \case
    SpatialIndex_Grid     g -> GridIndex.lookup b g
    SpatialIndex_QuadTree q -> QuadTree.lookup b <$> readIORef q
    SpatialIndex_HashSet  h -> toList <$> readIORef h

