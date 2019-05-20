{-# Language TemplateHaskell #-}
module Data.QuadTree.Types where

import Delude

data QuadTreeConfig = QuadTreeConfig
   { field_size          :: Float
   , field_minCellSize   :: Float
   , field_maxBucketSize :: Int
   } deriving (Generic, Show)
instance HasSize QuadTreeConfig Float
instance Default QuadTreeConfig where
    def = QuadTreeConfig
        { field_size          = 1
        , field_minCellSize   = 1
        , field_maxBucketSize = 1
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

makeLenses ''QuadTree
makeLenses ''Quad
