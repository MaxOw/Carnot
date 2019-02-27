module Benchmarks (runBenchmarks) where

import Relude (IO)
import Criterion.Main
-- import Benchmarks.GridIndex
import Benchmarks.KDTree

runBenchmarks :: IO ()
runBenchmarks = defaultMain
    -- [ benchGroup_GridIndex
    [ benchGroup_KDTree
    ]
