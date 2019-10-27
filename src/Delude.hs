{-# Options_GHC -fno-warn-orphans #-}
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
    , boundedRange
    ) where

import Relude          as Exports
import Linear          as Exports (V2(..))
import Control.Lens    as Exports hiding (Context, uncons, transform, (??))
import Data.Default    as Exports
import Data.List       as Exports (dropWhileEnd)
import Data.List.Split as Exports (splitOn, splitWhen, split, whenElt)
import Data.Ix         as Exports (Ix)
import Foreign         as Exports (nullPtr)

import Engine.HasField   as Exports
import Engine.HasPattern as Exports

import Engine.Debug

import Engine.TH

import Control.Concurrent.STM.TMVar as Exports (TMVar)
import qualified Data.Colour as Color
import Data.Colour.SRGB

import Diagrams.TwoD.Transform as Exports (T2)
import Diagrams.Angle          as Exports ((@@))

--------------------------------------------------------------------------------

instance (Floating a, Ord a, Hashable a) => Hashable (Color.AlphaColour a) where
    hashWithSalt s c = hashWithSalt s (r, g, b, a)
        where
        RGB r g b = toSRGB $ Color.over c Color.black
        a = Color.alphaChannel c


splitWhenKeep :: (a -> Bool) -> [a] -> [[a]]
splitWhenKeep = split . whenElt

type T2D = T2 Float

readRecordTVar :: MonadIO m => s -> Lens' s (TVar a) -> m a
readRecordTVar s len = readTVarIO (s^.len)

caseJust :: Applicative m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
caseJust Nothing  _ = pure Nothing
caseJust (Just v) f = f v

caseJustM :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
caseJustM m f = flip caseJust f =<< m

boundedRange :: (Enum a, Bounded a) => [a]
boundedRange = [minBound .. maxBound]

