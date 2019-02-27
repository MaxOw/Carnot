module Tests (runTests) where

import Relude
import Test.Tasty

import Tests.GridIndex
-- import Tests.KDTree

runTests :: IO ()
runTests = defaultMain =<< mainGroup

mainGroup :: IO TestTree
mainGroup = do
    return $ testGroup "Tests"
        [ tests_GridIndex
        -- , tests_KDTree
        ]

