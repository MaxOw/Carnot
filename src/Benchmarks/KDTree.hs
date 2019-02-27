{-# Options_GHC -fno-warn-missing-signatures #-}
module Benchmarks.KDTree (benchGroup_KDTree) where

import Delude
import Criterion.Main
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Test.Tasty.QuickCheck (generate)

import Tests.Utils
import Engine.Common.Types
import Engine.KDTree

benchGroup_KDTree :: Benchmark
benchGroup_KDTree = env genBenchData $ \ ~(bb, ios) -> bgroup "KDTree"
    [ benchGroup_compareBaseline bb ios
    ]

type Entry = (V2 Float, Int)
benchGroup_compareBaseline :: BBox Float -> Vector Entry -> Benchmark
benchGroup_compareBaseline bb ios = bgroup (show $ Vector.length ios) $
    [ bench "baseline" $ whnf baseline (bb, ios)
    ] ++
    -- , bench "kdtree : build & lookup" $ whnf kdtreeBL (bb, ios)
    -- [ bench "kdtree : lookup only"    $ whnf kdtreeLO (bb, kdt)
    -- ]
    -- map (bench_depth bb ios) [17..19]
    map (bench_depth bb ios) [18]
    where
    -- mkBench (d,k) = bench ("kdtree-lookup " <> show d) $ whnf kdtreeLO (bb, k)
    baseline (b, vs) = Vector.length $ Vector.filter (flip bboxInside b . view _1) vs
    -- kdtreeBL (b, vs) = length $ lookup b
    --  $ buildKDTreeDepth depth $ Vector.fromList vs
    -- kdtreeLO (b, tr) = length $ lookup b tr
    -- depth = 10

bench_depth :: BBox Float -> Vector Entry -> Int -> Benchmark
bench_depth bb vs d =
    env (genKD vs d) $ \k -> bgroup (show d)
        [ bench "kdtree-build " $ nf kdtreeBL vs
        , bench "kdtree-lookup" $ whnf kdtreeLO (bb, k) ]
    where
    -- kdtreeBL (b, vs) = length $ lookup b $ buildKDTreeDepth d vs
    kdtreeBL = buildKDTreeDepth d
    kdtreeLO (b, tr) = length $ lookup b tr

-- Generate random test data
-- genBenchData :: IO ()
genBenchData = do
    let bb = rectToBBox $ Rect (pure (-50)) (pure 100)
    (ins, ous) <- generate $ genVs 100 999900 bb
    let ios = ins <> ous
    -- let depth = 16
    -- let ks = map (\d -> (d, buildKDTreeDepth d $ Vector.fromList ios)) [8..16]
    return (bb, Vector.fromList ios) -- , ks)

genKD :: Vector Entry -> Int -> IO (KDTree Int)
genKD vs d = return $ buildKDTreeDepth d vs

