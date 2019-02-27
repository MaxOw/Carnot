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
    ( Eq, Ord, Num, Functor, Applicative, Traversable
    , Foldable, Show, Generic, NFData, Fractional)
-- instance R1 Size where _x  = _Wrapped._x
-- instance R2 Size where _xy = _Wrapped._xy
makeWrapped ''Size

pattern Size a b = MkSize (V2 a b)
{-# COMPLETE Size #-}

instance HasWidth  (Size a) a where width  = _Wrapped._x
instance HasHeight (Size a) a where height = _Wrapped._y
instance Default a => Default (Size a) where def = Size def def

--------------------------------------------------------------------------------

data Rect x = Rect
   { rect_offset :: V2 x
   , rect_size   :: Size x
   } deriving (Show)
makeFieldsCustom ''Rect
instance R1 Rect where _x  = offset._x
instance R2 Rect where _xy = offset._xy
instance HasWidth  (Rect x) x where width  = size.width
instance HasHeight (Rect x) x where height = size.height

rectMinPoint :: Rect a -> V2 a
rectMinPoint = view offset

rectMaxPoint :: Num a => Rect a -> V2 a
rectMaxPoint x = x^.offset + x^.size._Wrapped

unitRect :: Num a => Rect a
unitRect = Rect (pure 0) (pure 1)

rectToList :: Num a => Rect a -> [V2 a]
rectToList r =
    [ V2 x1 y1
    , V2 x0 y1
    , V2 x1 y0
    , V2 x0 y0 ]
    where
    V2 x0 y0 = r^.offset
    V2 x1 y1 = r^.offset + r^.size._Wrapped

subRectOf :: Num a => Rect a -> Rect a -> Rect a
subRectOf ra rb = Rect o s
    where
    o = rb^.offset + ra^.offset * rb^.size._Wrapped
    s = ra^.size * rb^.size

data BBox a = BBox
   { bbox_minPoint :: V2 a
   , bbox_maxPoint :: V2 a
   } deriving (Eq, Ord, Show, Generic)
makeFieldsCustom ''BBox
instance NFData a => NFData (BBox a)

bboxFromList :: Ord a => NonEmpty (V2 a) -> BBox a
bboxFromList (a :| as) = foldl' f (BBox a a) as
    where
    f (BBox p0 p1) p = BBox (min <$> p0 <*> p) (max <$> p1 <*> p)

bboxToRect :: Num a => BBox a -> Rect a
bboxToRect b = Rect (b^.minPoint) (MkSize $ b^.maxPoint - b^.minPoint)

rectToBBox :: Num a => Rect a -> BBox a
rectToBBox r = BBox (r^.offset) (r^.offset + r^.size._Wrapped)

bboxCompose :: Ord a => BBox a -> BBox a -> BBox a
bboxCompose a b = BBox
    (min <$> a^.minPoint <*> b^.minPoint)
    (max <$> a^.maxPoint <*> b^.maxPoint)

bboxUnion :: Ord a => NonEmpty (BBox a) -> BBox a
bboxUnion (b :| bs) = foldl' bboxCompose b bs

bboxIntersects :: Ord a => BBox a -> BBox a -> Bool
bboxIntersects a b = testOn _x && testOn _y
    where
    testOn d = (a^.minPoint.d, a^.maxPoint.d) /\ (b^.minPoint.d, b^.maxPoint.d)
    (a0, a1) /\ (b0, b1) = if a0 < b0 then a1 > b0 else a0 < b1

bboxInside :: Ord a => V2 a -> BBox a -> Bool
bboxInside (V2 x y) (BBox p0 p1) =
    x >= p0^._x && x <= p1^._x &&
    y >= p0^._y && y <= p1^._y

-- Make BBox from center point and size
mkBBoxCenter :: Fractional a => V2 a -> Size a -> BBox a
mkBBoxCenter c (MkSize s) = BBox mp (mp+s)
    where mp = c - 0.5*s

bboxInsideBBox :: Ord a => BBox a -> BBox a -> Bool
bboxInsideBBox (BBox minA maxA) (BBox minB maxB)
     = minA^._x >= minB^._x && maxA^._x <= maxB^._x
    && minA^._y >= minB^._y && maxA^._y <= maxB^._y

--------------------------------------------------------------------------------

data VerticalAlign
   = Align_Top
   | Align_Middle
   | Align_Bottom

data HorizontalAlign
   = Align_Left
   | Align_Center
   | Align_Right

data BoxAlign = BoxAlign
   { boxAlign_vertical   :: VerticalAlign
   , boxAlign_horizontal :: HorizontalAlign
   }
makeFieldsCustom ''BoxAlign

pattern BoxAlign_TopLeft   = BoxAlign Align_Top Align_Left
pattern BoxAlign_TopCenter = BoxAlign Align_Top Align_Center
pattern BoxAlign_TopRight  = BoxAlign Align_Top Align_Right

pattern BoxAlign_MiddleLeft   = BoxAlign Align_Middle Align_Left
pattern BoxAlign_MiddleCenter = BoxAlign Align_Middle Align_Center
pattern BoxAlign_MiddleRight  = BoxAlign Align_Middle Align_Right

pattern BoxAlign_BottomLeft   = BoxAlign Align_Bottom Align_Left
pattern BoxAlign_BottomCenter = BoxAlign Align_Bottom Align_Center
pattern BoxAlign_BottomRight  = BoxAlign Align_Bottom Align_Right

pattern BoxAlign_Middle = BoxAlign Align_Middle Align_Center
pattern BoxAlign_Center = BoxAlign Align_Middle Align_Center

instance HasPatternTopLeft BoxAlign where
    _PatternTopLeft = BoxAlign_TopLeft
instance HasPatternTopCenter BoxAlign where
    _PatternTopCenter = BoxAlign_TopCenter
instance HasPatternTopRight BoxAlign where
    _PatternTopRight = BoxAlign_TopRight
instance HasPatternMiddleLeft BoxAlign where
    _PatternMiddleLeft = BoxAlign_MiddleLeft
instance HasPatternMiddleCenter BoxAlign where
    _PatternMiddleCenter = BoxAlign_MiddleCenter
instance HasPatternMiddleRight BoxAlign where
    _PatternMiddleRight = BoxAlign_MiddleRight
instance HasPatternBottomLeft BoxAlign where
    _PatternBottomLeft = BoxAlign_BottomLeft
instance HasPatternBottomCenter BoxAlign where
    _PatternBottomCenter = BoxAlign_BottomCenter
instance HasPatternBottomRight BoxAlign where
    _PatternBottomRight = BoxAlign_BottomRight
instance HasPatternCenter BoxAlign where
    _PatternCenter = BoxAlign_Center

{-
instance HasPatternLeft HorizontalAlign where
    _PatternLeft = AlignLeft
instance HasPatternRight HorizontalAlign where
    _PatternRight = AlignRight
-}

--------------------------------------------------------------------------------

