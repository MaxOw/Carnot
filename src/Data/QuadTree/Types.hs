{-# Language TemplateHaskell #-}
module Data.QuadTree.Types where

import Delude

data QuadTreeConfig = QuadTreeConfig
   { quadTreeConfig_size          :: Float
   , quadTreeConfig_minCellSize   :: Float
   , quadTreeConfig_maxBucketSize :: Int
   } deriving (Show)
instance Default QuadTreeConfig where
    def = QuadTreeConfig
        { quadTreeConfig_size          = 1
        , quadTreeConfig_minCellSize   = 1
        , quadTreeConfig_maxBucketSize = 1
        }

data QuadTree a = QuadTree
   { _config :: QuadTreeConfig
   , _tree   :: Maybe (Tree a)
   } deriving (Show)

data Pos x = Pos { getPos :: (V2 Float), unPos :: x } deriving (Show)
instance Eq  x => Eq  (Pos x) where (Pos _ a) == (Pos _ b) = a == b
instance Ord x => Ord (Pos x) where compare (Pos _ a) (Pos _ b) = compare a b

data Tree a
   = Node (Quad (Maybe (Tree a)))
   | Leaf (Set (Pos a))
   deriving (Show)

data Quad a = Quad
   { _topLeft     :: a
   , _topRight    :: a
   , _bottomLeft  :: a
   , _bottomRight :: a
   } deriving (Generic, Show)
instance Default a => Default (Quad a)

--------------------------------------------------------------------------------

makeFieldsCustom ''QuadTreeConfig
makeLenses ''QuadTree
makeLenses ''Quad
