module Data.GridIndex
    ( GridIndexConfig, gridSize, cellSize
    , GridIndex

    , create
    , insert
    , delete
    , move
    , lookup
    , gridBBox
    ) where

import Delude
import Linear (V2)

import Data.Ix (range)
import qualified Data.List as List
import qualified Data.Array.MArray as M

import Engine.Common.Types
import Data.GridIndex.Types

--------------------------------------------------------------------------------

gridSize :: Lens' GridIndexConfig (Size Int)
gridSize = field' @"field_gridSize"

cellSize :: Lens' GridIndexConfig (Size Float)
cellSize = field' @"field_cellSize"

--------------------------------------------------------------------------------

create :: MonadIO m => GridIndexConfig -> m (GridIndex a)
create conf = do
    arr <- liftIO $ M.newArray gridRange mempty
    return $ GridIndex
        { _config  = conf
        , _grid    = arr
        }
    where
    gridRange = (pure 0, conf^.gridSize._Wrapped)

insert :: MonadIO m => V2 Float -> a -> GridIndex a -> m ()
insert p v g = liftIO $ do
    let i = pointOnGrid (g^.config) p
    updateGrid i (v:) g

move :: (MonadIO m, Eq a)
    => V2 Float -> V2 Float -> a -> GridIndex a -> m ()
move p_from p_to v g = do
    let p0 = pointOnGrid (g^.config) p_from
    let p1 = pointOnGrid (g^.config) p_to
    if p0 /= p1
    then do
        updateGrid p0 (List.delete v) g
        updateGrid p1 (v:) g
    else return ()

delete :: (MonadIO m, Eq a) => V2 Float -> a -> GridIndex a -> m ()
delete p v g = liftIO $ do
    let i = pointOnGrid (g^.config) p
    updateGrid i (List.delete v) g

updateGrid :: MonadIO m => V2 Int -> ([a] -> [a]) -> GridIndex a -> m ()
updateGrid i f g = liftIO $ do
    vs <- M.readArray (g^.grid) i
    M.writeArray (g^.grid) i (f vs)

lookup :: MonadIO m => BBox Float -> GridIndex a -> m [a]
lookup b g = concat <$> liftIO (mapM (M.readArray (g^.grid)) $ range r)
    where
    r = bboxToRange $ bboxToGridBBox g b

bboxToRange :: BBox a -> (V2 a, V2 a)
bboxToRange (BBox r0 r1) = (r0, r1)

bboxToGridBBox :: GridIndex a -> BBox Float -> BBox Int
bboxToGridBBox g b = BBox r0 r1
    where
    r0 = max 0 . floor <$> (p0 + halfSize) / cs
    r1 = min <$> gs <*> (ceiling <$> (p1 + halfSize) / cs)
    BBox p0 p1 = b

    conf = g^.config
    gs = conf^.gridSize._Wrapped
    cs = conf^.cellSize._Wrapped

    halfSize = fullSize^._Wrapped / 2
    fullSize = (*) <$> (fmap fromIntegral $ conf^.gridSize) <*> conf^.cellSize

pointOnGrid :: GridIndexConfig -> V2 Float -> V2 Int
pointOnGrid conf p = max 0 $ min <$> gs <*> (floor <$> (p + halfSize) / cs)
    where
    gs = conf^.gridSize._Wrapped
    cs = conf^.cellSize._Wrapped
    halfSize = fullSize^._Wrapped / 2
    fullSize = (*) <$> (fmap fromIntegral $ conf^.gridSize) <*> conf^.cellSize

gridBBox :: GridIndexConfig -> BBox Float
gridBBox conf = BBox (-halfSize) halfSize
    where
    halfSize = fullSize^._Wrapped / 2
    fullSize = (*) <$> (fmap fromIntegral $ conf^.gridSize) <*> conf^.cellSize

--------------------------------------------------------------------------------

{-
lookupArrayRange :: Ix i => Array i a -> (i, i) -> [a]
lookupArrayRange a = map (a Array.!) . range

updateArrayRange :: Ix i => (a -> a) -> (i, i) -> Array i a -> Array i a
updateArrayRange f rg a = runSTArray $ do
    ma <- M.thaw a
    forM_ (range rg) $ \i -> do
        s <- M.readArray ma i
        M.writeArray ma i $ f s
    return ma

{-
updateArrayIxList :: Ix i => (a -> a) -> [i] -> Array i a -> Array i a
updateArrayIxList f is a = runSTArray $ do
    ma <- M.thaw a
    forM_ is $ \i -> do
        s <- M.readArray ma i
        M.writeArray ma i $ f s
    return ma
-}

updateArraySets :: (Ix i, Ord a)
    => [(i, Set a)] -> [(i, Set a)] -> Array i (Set a) -> Array i (Set a)
updateArraySets adds dels a = runSTArray $ do
    ma <- M.thaw a
    forM_ dels $ \(i, sd) -> do
        s <- M.readArray ma i
        M.writeArray ma i $ Set.difference s sd
    forM_ adds $ \(i, sa) -> do
        s <- M.readArray ma i
        M.writeArray ma i $ Set.union s sa
    return ma

-}

{-
updateGridIndex :: (Ord a, Hashable a)
    => BBox Float -> a -> GridIndex a -> GridIndex a
updateGridIndex b v g = case mCurrentRange of
    Nothing -> g
        -- & grid    %~ insertNew
        & gridMap %~ HashMap.insert v newRange
    Just rg -> g
        -- & grid    %~ (toDelete rg . toInsert rg)
        & gridMap %~ HashMap.insert v newRange
    where
    mCurrentRange = Set.fromList . range <$> HashMap.lookup v (g^.gridMap)
    newRange = bboxToRange $ bboxToGridBBox g b
    {-
    ent = GridIndexEntry b v
    insertNew = updateArrayRange (Set.insert ent) newRange

    newSet = Set.fromList $ range newRange

    toInsert oldSet
        = updateArrayIxList (Set.insert ent)
        $ toList $ Set.difference newSet oldSet

    toDelete oldSet
        = updateArrayIxList (Set.delete ent)
        $ toList $ Set.difference oldSet newSet
    -}
-}

{-

updateListGridIndex :: (Ord a, Hashable a)
    => [(BBox Float, a)] -> GridIndex a -> GridIndex a
updateListGridIndex vs g = g
    & grid    %~ updateArraySets adds dels
    & gridMap %~ HashMap.union (HashMap.fromList $ map f vs)
    where
    f (bb, a) = (a, bboxToRange $ bboxToGridBBox g bb)
    news = makeChangeMap $ map nfs vs
    nfs (bb, a) = (bboxToRange $ bboxToGridBBox g bb, GridIndexEntry bb a)
    olds = makeChangeMap $ mapMaybe ofs vs
    ofs (bb, a) = (, GridIndexEntry bb a) <$> HashMap.lookup a (g^.gridMap)

    makeChangeMap = HashMap.fromListWith Set.union . concatMap chf
        where chf (b, a) = (,Set.singleton a) <$> range b

    dels = HashMap.toList $ changeDiff olds news
    adds = HashMap.toList $ changeDiff news olds

    changeDiff = HashMap.differenceWith setDiff
    setDiff a b = let d = Set.difference a b in if Set.null d
        then Nothing
        else Just d

deleteGridIndex :: (Ord a, Hashable a) => a -> GridIndex a -> GridIndex a
deleteGridIndex v g = case HashMap.lookup v (g^.gridMap) of
    Nothing -> g
    Just rg -> g
        & grid    %~ deleteRange rg
        & gridMap %~ HashMap.delete v
    where
    deleteRange = updateArrayRange (Set.delete $ bogusEntry v)
    bogusEntry = GridIndexEntry (BBox 0 0)

-}
