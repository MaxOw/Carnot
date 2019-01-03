{-# Language TemplateHaskell #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
module Engine.Common.Types where

import Delude
import Linear

--------------------------------------------------------------------------------

type Percent = Double
type AbsoluteSize = Double -- Size in pixels?
-- newtype AbsoluteSize = AbsoluteSize { sizeInPixels :: Double }
-- newtype Percent      = Percent Double

type FillPart = Double

data Sizing
   = Sizing_ContainerPct Percent      -- [0-1] = Percent of container size
   | Sizing_WindowPct    Percent      -- [0-1] = Percent of window size
   | Sizing_Absolute     AbsoluteSize --       = Absolute value in pixels
   | Sizing_Fill         FillPart     -- [0-*] = Fill leftover proportionally
   deriving (Show)
makePrisms ''Sizing

px :: Prism' Sizing AbsoluteSize
px = _Sizing_Absolute

cpct :: Prism' Sizing Percent
cpct = _Sizing_ContainerPct

wpct :: Prism' Sizing Percent
wpct = _Sizing_ContainerPct

fill :: Prism' Sizing FillPart
fill = _Sizing_Fill

instance Default Sizing where def = Sizing_Absolute 0

--------------------------------------------------------------------------------

newtype Size a = MkSize (V2 a) deriving
    (Num, Functor, Applicative, Traversable, Foldable, Show)
-- instance R1 Size where _x  = _Wrapped._x
-- instance R2 Size where _xy = _Wrapped._xy
makeWrapped ''Size

pattern Size a b = MkSize (V2 a b)
{-# COMPLETE Size #-}

instance HasWidth  (Size a) a where width  = _Wrapped._x
instance HasHeight (Size a) a where height = _Wrapped._y
instance Default a => Default (Size a) where def = Size def def

