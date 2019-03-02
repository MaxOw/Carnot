module Tests (runTests) where

import Relude
import Test.Tasty

-- import Tests.GridIndex
-- import Tests.KDTree
import Tests.QuadTree

runTests :: IO ()
runTests = defaultMain =<< mainGroup

mainGroup :: IO TestTree
mainGroup = do
    return $ testGroup "Tests"
        [ tests_QuadTree
        ]
        -- [ tests_GridIndex
        -- , tests_KDTree
        -- ]

