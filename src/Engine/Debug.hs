module Engine.Debug
    ( callStackFileLine
    , logOnce
    , resetDebugLog
    ) where

import Relude
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack (SrcLoc(..))

{-# NOINLINE logRef #-}
logRef :: IORef (Set String)
logRef = unsafePerformIO $ newIORef mempty

callStackFileLine :: CallStack -> String
callStackFileLine cs = fromMaybe "?:? " $ fmap prettySrc mSrcLoc
    where
    mSrcLoc = viaNonEmpty head $ map snd $ getCallStack cs
    prettySrc s = srcLocFile s <> ":" <> show (srcLocStartLine s) <> " "

resetDebugLog :: MonadIO m => m ()
resetDebugLog = do
    writeIORef logRef Set.empty

logOnce :: (HasCallStack, MonadIO m) => Text -> m ()
logOnce msg = liftIO $ do
    let cs = callStack
    let srcLoc = callStackFileLine cs
    let css = prettyCallStack callStack
    callStackSet <- readIORef logRef
    unless (Set.member css callStackSet) $ do
        atomicModifyIORef' logRef $ \s -> (Set.insert css s, ())
        putTextLn (toText srcLoc <> msg)

