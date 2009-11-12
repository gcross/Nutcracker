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
        -- @+node:gcross.20091111171052.1655:trivial, all dimensions 1
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
        -- @-node:gcross.20091111171052.1655:trivial, all dimensions 1
        -- @-others
        ]
    -- @-node:gcross.20091111171052.1649:computeExpectation
    -- @-others
    -- @-node:gcross.20091111171052.1640:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091111171052.1608:@thin test.hs
-- @-leo
