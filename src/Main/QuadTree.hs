module Main.QuadTree (runMain) where

import Delude
import Control.Exception (catch)
import Linear
import Engine.Common.Types
import Data.QuadTree as Q
import GHC.Stack (currentCallStack)

values :: Int -> [(V2 Float, Int)]
values n = map f [1..n]
    where
    f x = (V2 (fromIntegral x) (fromIntegral x), x)

runMain :: IO ()
runMain = catch ?? handleEx $ do
    let conf = def @QuadTreeConfig
             & size          .~ 100
             & minCellSize   .~ 2
             & maxBucketSize .~ 4
    let eq = Q.empty conf
    let vs = foldl' (\t (p, i) -> Q.insert p i t) eq $ values 10000
    let ds = foldl' (\t (p, i) -> Q.delete p i t) eq $ values 10000
    -- let vs = foldr (\(p, i) t -> Q.insert p i t) eq $ values 10000
    print $ Q.lookup (mkBBoxCenter (V2 0 0) (Size 5 5)) ds
    return ()

handleEx :: SomeException -> IO ()
handleEx e = do
    ccs <- currentCallStack
    print e
    mapM_ putStrLn ccs
    return ()
