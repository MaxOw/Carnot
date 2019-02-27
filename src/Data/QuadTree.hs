module Data.QuadTree
    ( QuadTreeConfig
    , QuadTree

    , empty
    , insert
    , delete
    , move
    , lookup
    ) where

import Delude hiding (empty)
import Linear
import qualified Data.Set as Set
import Data.QuadTree.Types
import Engine.Common.Types (BBox(..), maxPoint, minPoint, bboxInside)

--------------------------------------------------------------------------------

empty :: QuadTreeConfig -> QuadTree a
empty conf = QuadTree
   { _config = conf
   , _tree   = Nothing
   }

insert :: Ord a => V2 Float -> a -> QuadTree a -> QuadTree a
insert p@(V2 x y) a q = over tree (go (0 :: Int) 0 fullSize) q
    where
    posPA = Pos p a
    fullSize = q^.config.size
    ss = fullSize / q^.config.minCellSize
    maxDepth = ceiling $ logBase 2 ss
    bucketOverflows s = Set.size s >= q^.config.maxBucketSize

    go !depth !pivot !nodeSize = \case
        Nothing -> Just $ Leaf $ Set.singleton posPA
        Just qt -> Just $ insertTree (depth+1) pivot nodeSize qt

    insertTree !depth !pivot !nodeSize = \case
        Node r -> Node $ insertNode depth pivot nodeSize r
        Leaf s -> if
            | depth >= maxDepth -> Leaf newSet
            | bucketOverflows s -> splitLeaf pivot newSet
            | otherwise         -> Leaf newSet
            where newSet = Set.insert posPA s

    insertNode !depth (V2 px py) !nodeSize =
        if x >= px
        then if y >= py
            then over topRight    $ go depth (V2 (px+s) (py+s)) s
            else over bottomRight $ go depth (V2 (px+s) (py-s)) s
        else if y >= py
            then over topLeft     $ go depth (V2 (px-s) (py+s)) s
            else over bottomLeft  $ go depth (V2 (px-s) (py-s)) s
        where
        s = nodeSize*0.5

    splitLeaf (V2 px py) s =
        let (sM, sP)   = Set.partition (\(Pos pp _) -> pp^._x < px) s  in
        let (sPM, sPP) = Set.partition (\(Pos pp _) -> pp^._y < py) sP in
        let (sMM, sMP) = Set.partition (\(Pos pp _) -> pp^._y < py) sM in
        Node $ def
             & topLeft     .~ packLeaf sMP
             & topRight    .~ packLeaf sPP
             & bottomLeft  .~ packLeaf sMM
             & bottomRight .~ packLeaf sPM

    packLeaf s
        | Set.null s = Nothing
        | otherwise  = Just $ Leaf s

delete :: Ord a => V2 Float -> a -> QuadTree a -> QuadTree a
delete p@(V2 x y) a q = over tree (go 0 fullSize) q
    where
    posPA = Pos p a
    fullSize = q^.config.size

    go !pivot !nodeSize = \case
        Nothing -> Nothing
        Just qt -> let dqt = deleteTree pivot nodeSize qt in if isEmpty dqt
            then Nothing
            else Just dqt

    deleteTree !pivot !nodeSize = \case
        Node r -> Node $ deleteNode pivot nodeSize r
        Leaf s -> Leaf $ Set.delete posPA s

    deleteNode (V2 px py) !nodeSize =
        if x >= px
        then if y >= py
            then over topRight    $ go (V2 (px+s) (py+s)) s
            else over bottomRight $ go (V2 (px+s) (py-s)) s
        else if y >= py
            then over topLeft     $ go (V2 (px-s) (py+s)) s
            else over bottomLeft  $ go (V2 (px-s) (py-s)) s
        where
        s = nodeSize*0.5

    isEmpty = \case
        Leaf s -> Set.null s
        Node r -> all (isNothing . flip view r)
            [ topLeft, topRight, bottomLeft, bottomRight ]

move :: Ord a => V2 Float -> V2 Float -> a -> QuadTree a -> QuadTree a
move p0 p1 a = if p0 == p1 then id else insert p1 a . delete p0 a

lookup :: BBox Float -> QuadTree a -> [a]
lookup bb q = go 0 fullSize (q^.tree) []
    where
    fullSize = q^.config.size
    go !pivot !nodeSize = \case
        Nothing -> mempty
        Just qt -> lookupTree pivot nodeSize qt

    lookupTree !pivot !nodeSize = \case
        Node r -> lookupNode pivot nodeSize r
        Leaf s -> ((map unPos $ filter (flip bboxInside bb . getPos) $ toList s)<>)

    maxP = bb^.maxPoint
    minP = bb^.minPoint

    lookupNode (V2 px py) !nodeSize n =
        mif (px < maxP^._x) (
            mif (py < maxP^._y) (go (V2 (px+s) (py+s)) s (view topRight n)) <>
            mif (py > minP^._y) (go (V2 (px+s) (py-s)) s (view bottomRight n))
        ) <>
        mif (px > minP^._x) (
            mif (py < maxP^._y) (go (V2 (px-s) (py+s)) s (view topLeft n)) <>
            mif (py > minP^._y) (go (V2 (px-s) (py-s)) s (view bottomLeft n))
        )
        where
        s = nodeSize*0.5

    mif True  l = l
    mif False _ = mempty

