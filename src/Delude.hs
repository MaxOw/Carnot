{-# Language RankNTypes #-}
module Delude
    ( module Exports
    , T2D

    , splitWhenKeep

    , callStackFileLine
    , logOnce
    , makeFieldsCustom

    , readRecordTVar

    , caseJust, caseJustM
    ) where

import Relude          as Exports
import Linear          as Exports (V2(..))
import Control.Lens    as Exports hiding (Context, uncons, transform)
import Data.Default    as Exports
import Data.List       as Exports (dropWhileEnd)
import Data.List.Split as Exports (splitOn, splitWhen, split, whenElt)
import Data.Ix         as Exports (Ix)

import Engine.HasField   as Exports
import Engine.HasPattern as Exports

import Engine.TH

import Control.Concurrent.STM.TMVar as Exports (TMVar)

import Diagrams.TwoD.Transform as Exports (T2)
import Diagrams.Angle          as Exports ((@@))

import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack (SrcLoc(..))

--------------------------------------------------------------------------------

splitWhenKeep :: (a -> Bool) -> [a] -> [[a]]
splitWhenKeep = split . whenElt

type T2D = T2 Float

{-# NOINLINE logRef #-}
logRef :: IORef (Set String)
logRef = unsafePerformIO $ newIORef mempty

callStackFileLine :: CallStack -> String
callStackFileLine cs = fromMaybe "?:? " $ fmap prettySrc mSrcLoc
    where
    mSrcLoc = viaNonEmpty head $ map snd $ getCallStack cs
    prettySrc s = srcLocFile s <> ":" <> show (srcLocStartLine s) <> " "

logOnce :: (HasCallStack, MonadIO m) => Text -> m ()
logOnce msg = liftIO $ do
    let cs = callStack
    let srcLoc = callStackFileLine cs
    let css = prettyCallStack callStack
    callStackSet <- readIORef logRef
    unless (Set.member css callStackSet) $ do
        atomicModifyIORef' logRef $ \s -> (Set.insert css s, ())
        -- putStrLn cs
        putStrLn (toText srcLoc <> msg)
        return ()

readRecordTVar :: MonadIO m => s -> Lens' s (TVar a) -> m a
readRecordTVar s len = readTVarIO (s^.len)

caseJust :: Applicative m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
caseJust Nothing  _ = pure Nothing
caseJust (Just v) f = f v

caseJustM :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
caseJustM m f = flip caseJust f =<< m

