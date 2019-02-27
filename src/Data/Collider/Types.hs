{-# Language TemplateHaskell #-}
module Data.Collider.Types where

import Delude

data CircleDesc = CircleDesc
   { _center :: V2 Float
   , _radius :: Float
   }

data CollisionShape
   = Circle CircleDesc

makeLenses ''CircleDesc
