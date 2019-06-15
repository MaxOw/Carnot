module Data.ZMap
    ( ZMap
    , new
    , clear
    , add
    , toVector
    ) where

import Relude
import Control.Exception (bracket_)
import Control.Concurrent.QSem
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.GrowVector (GrowVector)
import qualified Data.GrowVector as G

--------------------------------------------------------------------------------

data ZMap a = ZMap
   { field_vectors :: Vector (GrowVector a)
   , field_lock    :: QSem
   }

withLock :: ZMap a -> IO b -> IO b
withLock gv = bracket_ (waitQSem s) (signalQSem s)
    where
    s = field_lock gv

new :: Int -> IO (ZMap a)
new n = do
    vs <- mapM (const G.new) [0..n-1]
    l <- newQSem 1
    return $ ZMap
        { field_vectors = V.fromList vs
        , field_lock    = l
        }

clear :: ZMap a -> IO ()
clear zm = withLock zm $ mapM_ G.clear (field_vectors zm)

add :: ZMap a -> Int -> a -> IO ()
add zm i = G.snoc v
    where
    v = (V.!) vv ii
    ii = max 0 $ min (V.length vv - 1) i
    vv = field_vectors zm

toVector :: ZMap a -> IO (Vector a)
toVector zm = withLock zm $ G.unsafeConcatFreeze (field_vectors zm)

