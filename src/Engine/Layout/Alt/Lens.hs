{-# Language MonoLocalBinds #-}
{-# Language DefaultSignatures #-}
module Engine.Layout.Alt.Lens where

import Control.Lens
import Engine.Lens.Utils
-- import Data.Generics.Product as Engine.HasField (HasField'(field'))

{-
class HasColor s a | s -> a where
    color :: Lens' s a
    default color :: HasField' "field_color" s a => Lens' s a
    color = field' @"field_color"
-}

--------------------------------------------------------------------------------

rect :: HasF "rect" s a => Lens' s a
rect = ff#rect

parentSize :: HasF "parentSize" s a => Lens' s a
parentSize = ff#parentSize

-- parentRect :: HasF "parentRect" s a => Lens' s a
-- parentRect = ff#parentRect

align :: HasF "align" s a => Lens' s a
align = ff#align

alpha :: HasF "alpha" s a => Lens' s a
alpha = ff#alpha

primitive :: HasF "primitive" s a => Lens' s a
primitive = ff#primitive
