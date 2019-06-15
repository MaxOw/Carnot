{-# Language TypeFamilies #-}
{-# Language ScopedTypeVariables #-}
module Data.GrowVector
    ( GrowVector
    , new, clear
    , length
    , snoc

    , sortBy
    , toVector, toList
    , unsafeToVector
    , unsafeConcatFreeze
    ) where

import Relude hiding (length, toList, sortBy)
import qualified Data.Foldable as F
import Control.Exception (bracket_)
import Control.Concurrent.QSem
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Algorithms.Radix as M
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Thread safe, append/grow only vector.

data GrowVector a = GrowVector
   { field_vector :: IORef (IOVector a)
   , field_size   :: IORef Int
   , field_lock   :: QSem
   } -- deriving (Generic)

withLock :: GrowVector a -> IO b -> IO b
withLock gv = id
{-
withLock gv = bracket_ (waitQSem s) (signalQSem s)
    where
    s = field_lock gv
-}

new :: IO (GrowVector a)
new = GrowVector
    <$> (newIORef =<< M.new 256)
    <*> newIORef 0
    <*> newQSem 1

clear :: GrowVector a -> IO ()
clear gv = withLock gv $ do
    v <- readIORef (field_vector gv)
    M.clear v
    writeIORef (field_size   gv) 0

length :: GrowVector a -> IO Int
length gv = readIORef (field_size gv)

snoc :: GrowVector a -> a -> IO ()
snoc gv a = withLock gv $ do
    v <- readIORef (field_vector gv)
    s <- readIORef (field_size gv)
    let l = M.length v
    if s == M.length v
    then do
        nv <- M.grow v (max 1 l)
        M.write nv s a
        writeIORef (field_vector gv) nv
    else do
        M.write v  s a

    modifyIORef' (field_size gv) (+1)

sortBy :: forall e a. M.Radix e => (a -> e) -> GrowVector a -> IO ()
sortBy f gv = do
    v <- readIORef (field_vector gv)
    s <- readIORef (field_size gv)
    let sv = M.take s v
    let e = undefined :: e
    M.sortBy (M.passes e) (M.size e) (\i a -> M.radix i (f a)) sv

toVector :: GrowVector a -> IO (Vector a)
toVector gv = withLock gv $ do
    v <- readIORef (field_vector gv)
    s <- readIORef (field_size gv)
    let sv = M.take s v
    V.freeze sv

unsafeToVector :: GrowVector a -> IO (Vector a)
unsafeToVector gv = do
    v <- readIORef (field_vector gv)
    s <- readIORef (field_size gv)
    let sv = M.take s v
    V.unsafeFreeze sv

toList :: GrowVector a -> IO [a]
toList gv = V.toList <$> toVector gv

-- Warning: This operation is thread unsafe.
unsafeConcatFreeze :: Traversable t => t (GrowVector a) -> IO (Vector a)
unsafeConcatFreeze vvs = do
    sa <- sum <$> mapM (readIORef . field_size) vvs
    ov <- M.new sa
    go ov 0 $ F.toList vvs
    V.unsafeFreeze ov
    where
    go _ _ [] = return ()
    go ov o (v:vs) = do
        s <- readIORef (field_size v)
        vv <- readIORef (field_vector v)
        forM_ [0..s-1] $ \i -> do
            M.write ov (o+i) =<< M.read vv i
        go ov (o+s) vs

