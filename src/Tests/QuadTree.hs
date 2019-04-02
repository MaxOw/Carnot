{-# Options_GHC -fno-warn-missing-signatures #-}
module Tests.QuadTree (tests_QuadTree) where

import Delude
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Tests.Utils
-- import Engine.Common.Types
import Data.QuadTree
import qualified Data.QuadTree as QuadTree

--------------------------------------------------------------------------------

tests_QuadTree :: TestTree
tests_QuadTree = testGroup "QuadTree"
    [ testProperty "Create Lookup In/Out" prop_createLookupInOut
    ]

prop_createLookupInOut :: Property
prop_createLookupInOut =
    forAll arBBox $ \bb ->
    forAll (genVs 10 10 bb) $ \(ins, ous) ->
        let vs = Vector.fromList $ ins <> ous in
        let kd = populateQuadTree vs in
        Set.fromList (lookup bb kd) === Set.fromList (map snd ins)

populateQuadTree :: Foldable t => t (V2 Float, Int) -> QuadTree Int
populateQuadTree = foldr (uncurry insert) (QuadTree.empty conf)
    where
    conf = def
        & size          .~ 30
        & minCellSize   .~ 2
        & maxBucketSize .~ 5

