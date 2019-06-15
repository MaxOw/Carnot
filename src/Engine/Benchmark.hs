{-# Options_GHC -fno-warn-orphans #-}
{-# Language RankNTypes #-}
module Engine.Benchmark where

import Delude
import Engine
import Criterion.Main
import qualified Engine.Context as Context

instance NFData a => NFData (EngineState a) where rnf _ = ()

mkBench :: NFData st
    => String
    -> Engine () st
    -> (st -> Engine st ())
    -> Benchmark
mkBench n initSt runSt = env i $ \st -> bench n $ whnfIO (r st)
    where
    i = do
        ctx <- Context.initWindow n (400, 400)
        makeContextCurrent (Just ctx)
        initialEventQueue    <- initEvents ctx Nothing
        initialGraphicsState <- initGraphics ctx

        let initialEngineState = EngineState
                { _userState  = ()
                , _eventQueue = initialEventQueue
                , _graphics   = initialGraphicsState
                }

        runStateT initSt initialEngineState

    r (st, engineState) =
        void $ evalStateT (runSt st) $ engineState { _userState = st }

{-
import Delude
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Criterion.Measurement

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Engine.Lens.Utils

--------------------------------------------------------------------------------

runMain :: IO ()
runMain = putStrLn "Benchmark"

--------------------------------------------------------------------------------

data BenchState = BenchState
   { field_benchStart :: !(Maybe BenchSample)
   , field_cumulative :: !BenchSample
   , field_benchMin   :: !(Maybe BenchSample)
   , field_benchMax   :: !(Maybe BenchSample)
   , field_loopCount  :: !Word64
   } deriving (Generic)

data BenchSample = BenchSample
   { field_time    :: !Double
   , field_cputime :: !Double
   , field_cycles  :: !Word64
   } deriving (Generic)
instance Default BenchSample
instance Default BenchState

data Bench = Bench
   { field_parts :: IOVector BenchState
   } deriving (Generic)

data BenchPart
   = BenchPart_FullLoop
   | BenchPart_Integration
   | BenchPart_Rendering
   deriving (Enum, Bounded)

--------------------------------------------------------------------------------

getBenchSample :: IO BenchSample
getBenchSample = BenchSample
    <$> getTime
    <*> getCPUTime
    <*> getCycles

overBenchSample
    :: (forall a. (Ord a, Num a) => a -> a -> a)
    -> BenchSample -> BenchSample -> BenchSample
overBenchSample dop a b = BenchSample
    { field_time    = field_time    a `dop` field_time    b
    , field_cputime = field_cputime a `dop` field_cputime b
    , field_cycles  = field_cycles  a `dop` field_cycles  b
    }

diffBenchSample :: BenchSample -> BenchSample -> BenchSample
diffBenchSample = overBenchSample (-)

addBenchSample :: BenchSample -> BenchSample -> BenchSample
addBenchSample = overBenchSample (+)

minBenchSample :: BenchSample -> BenchSample -> BenchSample
minBenchSample = overBenchSample (min)

maxBenchSample :: BenchSample -> BenchSample -> BenchSample
maxBenchSample = overBenchSample (max)

--------------------------------------------------------------------------------

initBench :: IO Bench
initBench = do
    initializeTime
    Bench <$> initVec (boundedRange @BenchPart)
    where
    initVec = Vector.unsafeThaw . fromList . map (const def)

startBenchPart :: BenchPart -> Bench -> IO ()
startBenchPart p b = do
    s <- getBenchSample
    updateBenchPart p b $ \x -> x
        { field_benchStart = Just s }

endBenchPart :: BenchPart -> Bench -> IO ()
endBenchPart p b = do
    s <- getBenchSample
    updateBenchPart p b $ \x -> x
        { field_benchStart = Nothing
        , field_cumulative = calc s x $ field_cumulative x
        , field_benchMin   = Just $ updateM minBenchSample s $ field_benchMin x
        , field_benchMax   = Just $ updateM maxBenchSample s $ field_benchMax x
        , field_loopCount  = field_loopCount x + 1
        }
    where
    calc s x = case field_benchStart x of
        Nothing -> id
        Just os -> addBenchSample (diffBenchSample s os)
    updateM f s = \case
        Nothing -> s
        Just os -> f s os

updateBenchPart :: BenchPart -> Bench -> (BenchState -> BenchState) -> IO ()
updateBenchPart p b f = MVector.modify (field_parts b) f (fromEnum p)

prettyBenchState :: BenchState -> Doc
prettyBenchState x = if x^.ff#loopCount <= 0 then "" else vsep
    [ "User time" <+> mkr "0" "0" "0"
    , "CPU  time" <+> mkr "0" "0" "0"
    , "Cycles   " <+> mkr "0" "0" "0"
    ]
    where
    mkr a mn mx = a <+> "(" <> mn <+> ".." <+> mx <> ")"

-}
