{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
module Data.Collider.Types where

import Delude

import Diagrams.Transform
import Diagrams.Core.V
import Diagrams.Core.Points

data CircleDesc = CircleDesc
   { _center :: Point V2 Float
   , _radius :: Float
   }
makeLenses ''CircleDesc

data CollisionShape
   = Circle CircleDesc

type instance N CollisionShape = Float
type instance V CollisionShape = V2
instance Transformable CollisionShape where
    transform t = \case
      Circle d -> Circle $ over center (transform t) d

