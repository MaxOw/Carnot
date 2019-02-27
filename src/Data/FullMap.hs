module Data.FullMap
    ( FullMap
    , build
    , update
    , lookup
    ) where

import Delude
import qualified Data.Map as Map

newtype FullMap a b = FullMap { unFullMap :: Map a b }

build :: (Enum a, Bounded a, Ord a) => (a -> b) -> FullMap a b
build f = FullMap $ Map.fromList $ map (\a -> (a, f a)) vs
    where
    vs = [minBound .. maxBound]

update :: Ord a => a -> b -> FullMap a b -> FullMap a b
update a b = FullMap . Map.insert a b . unFullMap

lookup :: Ord a => a -> FullMap a b -> b
lookup a (FullMap m) = case Map.lookup a m of
    Just vl -> vl
    Nothing -> error "This should't be possible in a FullMap."
