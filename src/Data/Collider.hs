module Data.Collider
    ( CollisionShape

    , collide
    , circle
    ) where

import Delude
import Linear
import Linear.Affine
import Data.Collider.Types

collide :: CollisionShape -> CollisionShape -> Maybe (V2 Float)
collide a b = case (a, b) of
   (Circle x, Circle y) -> collideCircles x y

collideCircles :: CircleDesc -> CircleDesc -> Maybe (V2 Float)
collideCircles a b
    | rs >= 0   = Nothing
    | otherwise = Just $ nv ^* rs
    where
    v  = a^.center .-. b^.center
    rs = norm v - (a^.radius + b^.radius)
    nv = normalize v

circle :: Point V2 Float -> Float -> CollisionShape
circle v r = Circle $ CircleDesc v r
