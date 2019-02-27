module Benchmarks.GridIndex (benchGroup_GridIndex) where

-- import Delude
-- import Linear (V2(..))
-- import qualified Data.Set as Set
import Criterion.Main

-- import Engine.Common.Types
-- import Data.GridIndex

benchGroup_GridIndex :: Benchmark
benchGroup_GridIndex = bgroup "GridIndex"
    -- [ bench "create 10"    $ nf createGrid 10
    -- , bench "create 100"   $ nf createGrid 100
    -- , bench "create 1000"  $ nf createGrid 1000

    -- , bench "create let 10"    $ nf createGridLet 10
    -- , bench "create let 100"   $ nf createGridLet 100
    -- , bench "create let 1000"  $ nf createGridLet 1000

{--
    [ bgroup "populate"
        [ bench "10"    $ nf popGrid 10
        , bench "100"   $ nf popGrid 100
        , bench "1000"  $ nf popGrid 1000
        ]
        --}
    []
    {-
    [ env createPopGrids $ \ ~(pop10, pop100, pop1000, bs) -> bgroup "lookup"
        [ bench "base"    $ whnf lookupB (bs     , rcB)
        , bench "S 10"    $ whnf lookupG (pop10  , rcS)
        , bench "S 100"   $ whnf lookupG (pop100 , rcS)
        , bench "S 1000"  $ whnf lookupG (pop1000, rcS)
        , bench "B 10"    $ whnf lookupG (pop10  , rcB)
        , bench "B 100"   $ whnf lookupG (pop100 , rcB)
        , bench "B 1000"  $ whnf lookupG (pop1000, rcB)
        ]
    ]

    where
    popGrid = populateGrid (pure 0) . createGrid

    createPopGrids = do
        let p10   = popGrid 10
        let p100  = popGrid 100
        let p1000 = popGrid 1000
        let bs    = baselineBBs (pure 0)
        return (p10, p100, p1000, bs)

    rcS = rectToBBox $ Rect (pure (-0.5)) (pure 1)
    rcB = rectToBBox $ Rect (pure (-25)) (pure 50)
    lookupG (g,r) = Set.size $ lookupGridIndex g r

    lookupB (bs, r) = length $ filter (bboxIntersects r . view _1) bs

    -- popGrid = populateGrid (pure 0.9) . populateGrid (pure 0) . createGrid
    {-
    createGrid10   = createGrid (10   :: Int)
    createGrid100  = createGrid (100  :: Int)
    createGrid1000 = createGrid (1000 :: Int)

    createGridLet :: Int -> GridIndex Int
    createGridLet 10   = createGrid10
    createGridLet 100  = createGrid100
    createGridLet 1000 = createGrid1000
    createGridLet _    = createGrid1000
    -}

-}

{-
createGrid :: Int -> GridIndex Int
createGrid x = createGridIndex conf
    where
    conf = def
        & gridSize .~ pure x
        & cellSize .~ pure (1000 / fromIntegral x)

populateGrid :: V2 Float -> GridIndex Int -> GridIndex Int
populateGrid off g = updateListGridIndex (baselineBBs off) g
-- populateGrid g = foldr (uncurry updateGridIndex) g bbs
-- populateGrid g = foldl' (\r b -> uncurry updateGridIndex b r) g bbs

-}

{-
baselineBBs :: V2 Float -> [(BBox Float, Int)]
baselineBBs off = [(mkBBox $ V2 x y, x*1000+y) | x <- [0..50], y <- [0..50] ]
    where
    mkBBox :: V2 Int -> BBox Float
    mkBBox x = rectToBBox $ Rect (off + (fmap fromIntegral x - pure 25)) $ pure 1
-}

