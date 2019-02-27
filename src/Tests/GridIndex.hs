{-# Language StandaloneDeriving #-}
{-# Options_GHC -fno-warn-orphans #-}
{-# Options_GHC -fno-warn-unused-imports #-}
{-# Options_GHC -fno-warn-unused-top-binds #-}
module Tests.GridIndex (tests_GridIndex) where

import Delude
import Linear (V2(..))
-- import Data.Random (Random)
import qualified Data.Set as Set
import Data.Array (Array)
import qualified Data.Array as Array
import Test.Tasty
-- import Test.Tasty.HUnit
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Data.TreeDiff.Class (ToExpr(..))
import Data.TreeDiff.QuickCheck

import Engine.Common.Types
import Engine.GridIndex

tests_GridIndex :: TestTree
tests_GridIndex = testGroup "GridIndex"
    []
{-
    [ QC.testProperty "delete v . add k v == id" $ prop_addDeleteEqNop
    , QC.testProperty "lookup k . add k v == v"  $ prop_addLookup
    , QC.testProperty "  k₁ ∩ k₂  : lookup k₁ . add k₂ v == v" $
        prop_addLookupIntersect
    , QC.testProperty "¬(k₁ ∩ k₂) : lookup k₁ . add k₂ v == ∅" $
        prop_addLookupNotIntersect
    ]

--------------------------------------------------------------------------------

prop_addDeleteEqNop :: Property
prop_addDeleteEqNop =
    forAll genGridConf $ \c ->
    forAll (genEnt c)  $ \(a, r) ->
        delAddCreate a r c =#= createGridIndex c
    where
    delAddCreate a r = deleteGridIndex a . updateGridIndex r a . createGridIndex

prop_addLookup :: Property
prop_addLookup =
    forAll genGridConf $ \c ->
    forAll (genEnt c)  $ \(a, r) ->
        addLookup a r c =#= (Set.singleton a)
    where
    addLookup a r = flip lookupGridIndex r . updateGridIndex r a . createGridIndex

prop_addLookupIntersect :: Property
prop_addLookupIntersect =
    forAll genGridConf $ \c -> do
        (a, r0) <- genEnt c
        (_, r1) <- genEnt c
        return $ bboxIntersects r0 r1 ==>
            (addLookup a r0 r1 c =#= Set.singleton a)
    where
    addLookup a r0 r1 = flip lookupGridIndex r0
        . updateGridIndex r1 a . createGridIndex

prop_addLookupNotIntersect :: Property
prop_addLookupNotIntersect =
    forAll genGridConf $ \c -> do
        (a, r0) <- genEnt c
        (_, r1) <- genEnt c
        return $ not (bboxIntersects r0 r1) ==>
            (addLookup a r0 r1 c =#= Set.empty)
    where
    addLookup a r0 r1 = flip lookupGridIndex r0
        . updateGridIndex r1 a . createGridIndex

-}

--------------------------------------------------------------------------------

(=#=) :: (Eq a, ToExpr a) => a -> a -> Property
(=#=) = ediffEq

genGridConf :: Gen GridIndexConfig
genGridConf = genConf
    where
    gsr = (3, 20)
    gcr = (1, 10)

    genConf :: Gen GridIndexConfig
    genConf = do
        rs <- Size <$> choose gsr <*> choose gsr
        rc <- Size <$> choose gcr <*> choose gcr
        return $ def
            & gridSize .~ rs
            & cellSize .~ rc

genEnt :: GridIndexConfig -> Gen (Int, BBox Float)
genEnt c = do
    aa <- arbitrary
    v0 <- chooseV2 (p0, p1)
    v1 <- chooseV2 (p0, p1)
    return (aa, bboxFromList (v0 :| [v1]))
    where
    BBox p0 p1 = gridBBox c

    -- chooseV2 :: Random a => (V2 a, V2 a) -> Gen (V2 a)
    chooseV2 (V2 x0 y0, V2 x1 y1) = V2 <$> choose (x0, x1) <*> choose (y0, y1)

--------------------------------------------------------------------------------

-- ∀ x₀ x₁ x₂ x₃ x₄ x₅ x₆ x₇ x₈ x₉
-- Tests to write:
-- create -> add n -> delete (shuffle n) == id
-- create -> add r x -> update r' x -> lookup r' == x

-- instance (ToExpr a, ToExpr b) => ToExpr (Array a b)
instance ToExpr a => ToExpr (BBox a)
instance ToExpr a => ToExpr (GridIndexEntry a)
instance ToExpr GridIndexConfig
instance ToExpr a => ToExpr (V2 a)
instance ToExpr a => ToExpr (Size a)
instance (Ix a, ToExpr a, ToExpr b) => ToExpr (Array a b) where
    toExpr = toExpr . Array.assocs
instance ToExpr a => ToExpr (GridIndex a)

