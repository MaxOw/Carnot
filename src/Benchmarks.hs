module Benchmarks (runBenchmarks) where

import Relude
import Criterion.Main
-- import Benchmarks.GridIndex
-- import Benchmarks.KDTree
-- import Experiments.Baseline
-- import Experiments.Draw01
-- import Experiments.Draw02
import Experiments.Draw03

runBenchmarks :: IO ()
runBenchmarks = defaultMain
    -- [ benchGroup_GridIndex
    -- [ benchGroup_KDTree

    -- [ bench_baseline
    -- [ bench_draw01 True 1
    -- [ bench_draw01 False 37
    -- [ bench_draw01 True  37
    -- [ bench_draw01 True  440
    --
    -- [ bench_draw02 True 64 100000 False False False
    -- [ bench_draw02 True 64 100000 True False False
    -- [ bench_draw02 True 64 14000 True True False
    -- [ bench_draw02 False 64 30000 True True False
    -- [ bench_draw02 False 64 10000 True False True
    [ bench_draw03 64 10000
    ]

-- baseline             :   0.13 ms
-- draw01 1920x1080x1   :   0.75 ms
-- draw01 1920x1080x1   :   0.95 ms (Depth Test)
-- draw01 1920x1080x37  : ~16.00 ms
-- draw01 1920x1080x37  :   2.17 ms (Depth Test)
-- draw01 1920x1080x440 : ~16.00 ms (Depth Test)

-- draw02 1x1  x140k    : ~15.50 ms (Depth Test)
-- draw02 10x10x140k    : ~16.00 ms (Depth Test)
-- draw02 64x64x100k    : ~13.00 ms (Depth Test)

-- draw02 64x64x100k    : ~15.00 ms (Depth Test, Distributed)
-- draw02 64x64x14k     : ~15.00 ms (Depth Test, Distributed, Discard)
-- draw02 64x64x30k     : ~16.00 ms (Distributed, Discard)
-- draw02 64x64x10k     : ~15.24 ms (Distributed, Blend)

