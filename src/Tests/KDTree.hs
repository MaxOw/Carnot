{-# Options_GHC -fno-warn-missing-signatures #-}
module Tests.KDTree (tests_KDTree) where

import Delude
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Tests.Utils
-- import Engine.Common.Types
import Engine.KDTree

--------------------------------------------------------------------------------

tests_KDTree :: TestTree
tests_KDTree = testGroup "KDTree"
    [ testProperty "Create Lookup In/Out" prop_createLookupInOut
    ]

prop_createLookupInOut :: Property
prop_createLookupInOut =
    forAll arBBox $ \bb ->
    forAll (genVs 10 10 bb) $ \(ins, ous) ->
        let vs = Vector.fromList $ ins <> ous in
        let kd = buildKDTree vs in
        Set.fromList (lookup bb kd) === Set.fromList (map snd ins)

