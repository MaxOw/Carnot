{-# Language TypeFamilies #-}
module Data.ArrayType where

import Data.Array (Array)
import Data.Array.IO (IOArray)

data RW -- Read-Write grid
data R  -- Read-only  grid

type family ArrayType a  where
            ArrayType R  = Array
            ArrayType RW = IOArray

