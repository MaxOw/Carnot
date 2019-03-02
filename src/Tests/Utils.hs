{-# Options_GHC -fno-warn-missing-signatures #-}
module Tests.Utils where

import Delude
import Engine.Common.Types

-- import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

genVs :: Int -> Int -> BBox Float -> Gen ([(V2 Float, Int)], [(V2 Float, Int)])
genVs insCt ousCt bb = do
    ins <- vectorOf insCt $ chooseV2 (p0, p1) `suchThat` (flip bboxInside bb)
    ous <- vectorOf ousCt $ arV2 `suchThat` (not . flip bboxInside bb)
    return (zip ins [0..], zip ous [insCt..])
    where
    BBox p0 p1 = bb
    chooseV2 (V2 x0 y0, V2 x1 y1) = V2 <$> choose (x0, x1) <*> choose (y0, y1)

{-
genVsBound :: Int -> Int
    -> BBox Float
    -> BBox Float
    -> Gen ([(V2 Float, Int)], [(V2 Float, Int)])
genVsBound insCt ousCt bb = do
    ins <- vectorOf insCt $ chooseV2 (p0, p1) `suchThat` (flip bboxInside bb)
    ous <- vectorOf ousCt $ arV2 `suchThat` (not . flip bboxInside bb)
    return (zip ins [0..], zip ous [insCt..])
    where
    BBox p0 p1 = bb
    chooseV2 (V2 x0 y0, V2 x1 y1) = V2 <$> choose (x0, x1) <*> choose (y0, y1)
-}

--------------------------------------------------------------------------------

arV2   = V2 <$> arbitrary <*> arbitrary
arRect = Rect <$> arV2 <*> fmap MkSize (arV2 `suchThat` gtz)
arBBox = rectToBBox <$> arRect
gtz (V2 x y) = x > 0 && y > 0


