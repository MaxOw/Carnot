{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
module Data.VectorIndex.Types where

import Delude
import Data.Vector.Mutable (IOVector)

newtype VectorIndex a = VectorIndex { unVectorIndex :: MVar (VectorIndexRep a) }

data VectorIndexRep a = VectorIndexRep
   { _vectorSize  :: Int
   , _vectorIndex :: IOVector (Maybe a)
   , _freeCells   :: [Int]
   }
makeLenses ''VectorIndexRep
