module Experiments.Baseline where

import Delude
-- import Engine.Lens.Utils
-- import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
-- import qualified Engine         as Engine
import qualified Engine.Context as Context

import Criterion.Main

import Experiments.Common

--------------------------------------------------------------------------------

bench_baseline :: Benchmark
bench_baseline = mkBench "baseline" initSt runSt

--------------------------------------------------------------------------------

data St = St
   { field_context :: Context
   } deriving (Generic)
instance NFData St

--------------------------------------------------------------------------------

initSt :: IO St
initSt = do
    cx <- Context.initWindow "baseline" (400, 400)
    GLFW.swapInterval 0
    -- GLFW.showWindow cx
    return $ St
        { field_context = cx
        }

runSt :: St -> IO ()
runSt st = do
    let cx = field_context st
    withBuffers cx $ return ()

