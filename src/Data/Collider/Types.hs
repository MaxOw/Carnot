{-# Language TypeFamilies #-}
module Data.Collider.Types where

import Delude

import Diagrams.Transform
import Diagrams.Core.V
import Diagrams.Core.Points

data CircleDesc = CircleDesc
   { field_center :: Point V2 Float
   , field_radius :: Float
   } deriving (Generic)

data CollisionShape
   = Circle CircleDesc

type instance N CollisionShape = Float
type instance V CollisionShape = V2
instance Transformable CollisionShape where
    transform t = \case
      Circle d -> Circle $ over center (transform t) d

