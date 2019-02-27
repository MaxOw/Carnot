module Data.VectorIndex
    ( VectorIndex

    , new
    , insert, insertWithKey
    , update
    , delete
    , lookup
    ) where

import Delude
import qualified Data.Vector.Mutable as M

import Data.VectorIndex.Types

new :: MonadIO m => m (VectorIndex a)
new = do
    v <- liftIO $ M.new 100
    fmap VectorIndex $ newMVar $ VectorIndexRep 0 v []

insertWithKey :: MonadIO m => (Int -> a) -> VectorIndex a -> m Int
insertWithKey f (VectorIndex v) = do
    VectorIndexRep vs vi fc <- takeMVar v
    case fc of
        i:is -> do
            liftIO $ M.write vi i (Just $ f i)
            putMVar v $ VectorIndexRep vs vi is
            return i
        [] -> do
            let l = M.length vi
            vg <- if (vs >= l) then liftIO $ M.grow vi (max 1 l) else return vi
            liftIO $ M.write vg vs (Just $ f vs)
            putMVar v $ VectorIndexRep (vs+1) vg []
            return vs

insert :: MonadIO m => a -> VectorIndex a -> m Int
insert a = insertWithKey (const a)

update :: MonadIO m => Int -> Maybe a -> VectorIndex a -> m ()
update i ma (VectorIndex v) = do
    VectorIndexRep vs vi _ <- readMVar v
    when (i >= 0 && i < vs) $ do
        liftIO $ M.write vi i ma

delete :: MonadIO m => Int -> VectorIndex a -> m ()
delete i (VectorIndex v) = do
    vr@(VectorIndexRep vs vi fc) <- takeMVar v
    if (i >= 0 && i < vs)
    then do
        liftIO $ M.write vi i Nothing
        putMVar v $ VectorIndexRep vs vi (i:fc)
    else putMVar v vr

lookup :: MonadIO m => Int -> VectorIndex a -> m (Maybe a)
lookup i (VectorIndex v) = do
    VectorIndexRep vs vi _ <- readMVar v
    if (i >= 0 && i < vs)
    then liftIO $ M.read vi i
    else return Nothing

