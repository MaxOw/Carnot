{-# Language RankNTypes #-}
module Engine.KDTree
    ( module Engine.KDTree.Types

    , buildKDTree, buildKDTreeDepth
    , lookup
    ) where

import Delude
import Linear
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Select.Intro (select)

import Engine.Common.Types
import Engine.KDTree.Types

--------------------------------------------------------------------------------

median :: Ord a => Vector a -> a
median v = select (Vector.length v `div` 2) v

partitionOn :: KDDim -> Float -> KDVector a -> (KDVector a, KDVector a)
partitionOn dim piv v = Vector.unstablePartition f v
    where f x = entryViaDim dim x < piv

entryViaDim :: KDDim -> KDEntry a -> Float
entryViaDim dim = view (_1.dimToLens dim)

dimToLens :: R2 f => KDDim -> Lens' (f a) a
dimToLens KD_X = _x
dimToLens KD_Y = _y

flipDim :: KDDim -> KDDim
flipDim KD_X = KD_Y
flipDim KD_Y = KD_X

buildKDTree :: KDVector a -> KDTree a
buildKDTree vs = buildKDTreeDepth optimalDepth vs
    where
    optimalDepth = floor @Float $ logBase 2 (fromIntegral (Vector.length vs) / 10)

buildKDTreeDepth :: Int -> KDVector a -> KDTree a
buildKDTreeDepth maxDepth = fromMaybe (KDTree_Leaf Vector.empty) . go KD_X maxDepth
    where
    go dim d ps
        | Vector.null ps                  = Nothing
        | d <= 0 || Vector.length ps == 1 = Just $ KDTree_Leaf ps
        | otherwise  = Just $ KDTree_Node $ mkNode dim d piv sm gt
            where
            (sm, gt) = partitionOn dim piv ps
            piv = median $ Vector.map (entryViaDim dim) ps

    mkNode dim d piv sm gt = KDNode
        { kdNode_smaller  = go (flipDim dim) (d-1) sm
        , kdNode_pivot    = piv
        , kdNode_greater  = go (flipDim dim) (d-1) gt
        , kdNode_pivotDim = dim
        }

lookup :: BBox Float -> KDTree a -> [a]
lookup rg kdt = go id kdt []
    where
    go f k = case k of
        KDTree_Leaf vs -> (toValuesInside vs <>) . f
        KDTree_Node nd -> nodeValues nd f

    toValuesInside = map snd . filter isInside . toList

    isInside :: KDEntry a -> Bool
    isInside e = bboxInside (e^._1) rg

    BBox p0 p1 = rg
    nodeValues nd f
        | p0d <  piv && p1d <  piv = maybe f (go f) (nd^.smaller)
        | p0d >= piv && p1d >= piv = maybe f (go f) (nd^.greater)
        | otherwise = maybe id (go id) (nd^.smaller)
                    . maybe id (go id) (nd^.greater)
                    . f
        where
        piv = nd^.pivot
        p0d = p0^.dimToLens (nd^.pivotDim)
        p1d = p1^.dimToLens (nd^.pivotDim)

{-
bboxSplitVia :: Ord a
    => Lens' (V2 a) a -> a -> BBox a -> (Maybe (BBox a), Maybe (BBox a))
bboxSplitVia dim piv bb = (Nothing, Nothing)
    where
    if bb^.minPoint.dim < piv
    then Just (bb & minPoint.dim .~ piv
    else Nothing
-}
