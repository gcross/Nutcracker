-- @+leo-ver=4-thin
-- @+node:gcross.20091111171052.1608:@thin test.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091111171052.1610:<< Import needed modules >>
import Control.Exception

import Data.Array.Storable
import Data.Array.Unboxed
import Data.Int

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Text.Printf

import System.IO.Unsafe

import VMPS
-- @nonl
-- @-node:gcross.20091111171052.1610:<< Import needed modules >>
-- @nl

-- @+others
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091111171052.1640:<< Tests >>
    -- @+others
    -- @+node:gcross.20091111171052.1649:computeExpectation
    [testGroup "computeExpectation"
        -- @    @+others
        -- @+node:gcross.20091111171052.1650:trivial, all dimensions 1
        [testCase "trivial, all dimensions 1" $
            let left_boundary = trivial_left_boundary
                right_boundary = trivial_right_boundary
                state_site_tensor = StateSiteTensor 1 1 1 trivial_complex_array
                operator_indices = unsafePerformIO $ newArray (1,2) 1
                operator_matrices = unsafePerformIO $ newListArray (1,2) [1.0,0.0]
                operator_site_tensor = OperatorSiteTensor 1 1 1 1 operator_indices operator_matrices
                expectation = computeExpectation left_boundary state_site_tensor operator_site_tensor right_boundary
            in assertEqual "is the correct result returned?" 1 expectation
        -- @-node:gcross.20091111171052.1650:trivial, all dimensions 1
        -- @+node:gcross.20091111171052.1655:trivial, d =2
        ,testCase "trivial, d = 2" $
            assertEqual "are the correct results returned for all calls?"
            [1,1,-1,-1]
            $
            map (\state ->
                let left_boundary = trivial_left_boundary
                    right_boundary = trivial_right_boundary
                    state_site_tensor = StateSiteTensor 1 1 2 . complexArrayFromList $ state
                    operator_indices = unsafePerformIO $ newArray (1,2) 1
                    operator_matrices = unsafePerformIO $ newListArray (1,8) [1,0, 0,0, 0,0, -1,0]
                    operator_site_tensor = OperatorSiteTensor 1 1 2 1 operator_indices operator_matrices
                in computeExpectation left_boundary state_site_tensor operator_site_tensor right_boundary
            ) [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
        -- @-node:gcross.20091111171052.1655:trivial, d =2
        -- @-others
        ]
    -- @-node:gcross.20091111171052.1649:computeExpectation
    -- @+node:gcross.20091111171052.1659:computeOptimalSiteStateTensor
    ,testGroup "computeOptimalSiteStateTensor"
        -- @    @+others
        -- @+node:gcross.20091111171052.1661:trivial, d = 4
        [testCase "trivial, d = 4" $
            let left_boundary = trivial_left_boundary
                right_boundary = trivial_right_boundary
                state_site_tensor = StateSiteTensor 1 1 4 . complexArrayFromList $ replicate (4*2) 1
                operator_indices = unsafePerformIO $ newArray (1,2) 1
                operator_matrices = unsafePerformIO $ newListArray (1,4*4*2)
                    [1,0, 0,0, 0,0, 0,0
                    ,0,0, 1,0, 0,0, 0,0
                    ,0,0, 0,0, 1,0, 0,0
                    ,0,0, 0,0, 0,0,-1,0
                    ]
                operator_site_tensor = OperatorSiteTensor 1 1 4 1 operator_indices operator_matrices
                either_result =
                    computeOptimalSiteStateTensor
                        left_boundary
                        state_site_tensor
                        operator_site_tensor
                        right_boundary
                        SR
                        0
                        10000
            in case either_result of
                Left info -> assertFailure $ "failed with info = " ++ show info
                Right (_, (StateSiteTensor bl br d (ComplexArray actual_array))) -> do
                    let components = elems actual_array
                    assertEqual "is the new left bandwidth dimension the same?" (stateLeftBandwidth state_site_tensor) bl
                    assertEqual "is the new right bandwidth dimension the same?" (stateRightBandwidth state_site_tensor) br
                    assertEqual "is the new physical dimension the same?" (statePhysicalDimension state_site_tensor) d
                    assertBool "are all but the last component of the state zero?" (take 6 components ~= replicate 6 0)
                    assertBool "are either of the last components non-zero?" (any (/~ 0) . drop 6 $ components)
        -- @-node:gcross.20091111171052.1661:trivial, d = 4
        -- @-others
        ]
    -- @-node:gcross.20091111171052.1659:computeOptimalSiteStateTensor
    -- @-others
    -- @-node:gcross.20091111171052.1640:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091111171052.1608:@thin test.hs
-- @-leo
