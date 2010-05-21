-- @+leo-ver=4-thin
-- @+node:gcross.20091111171052.1608:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113142219.2512:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091113142219.2512:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091111171052.1610:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Spoon

import Data.Array.Storable
import Data.Array.Unboxed
import Data.Complex
import Data.Int
import Data.Vec ((:.)(..))
import Data.Vec.Nat

import Debug.Trace

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

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.Miscellaneous
import VMPS.Models
import VMPS.Operators
import VMPS.Qubits.Models
import VMPS.Qubits.Operators
import VMPS.States
import VMPS.Tensors.Implementation
import VMPS.Wrappers
-- @-node:gcross.20091111171052.1610:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091113142219.2499:Generators
-- @+node:gcross.20091113142219.2500:UnderTenInt
newtype UnderTenInt = UTI Int deriving (Show,Eq)
instance Arbitrary UnderTenInt where
    arbitrary = choose (1,10) >>= return.UTI
-- @-node:gcross.20091113142219.2500:UnderTenInt
-- @+node:gcross.20091113142219.2514:NumberOfSitesInt
newtype NumberOfSitesInt = NOSI Int deriving (Show,Eq)
instance Arbitrary NumberOfSitesInt where
    arbitrary = choose (20,1000) >>= return.NOSI
-- @-node:gcross.20091113142219.2514:NumberOfSitesInt
-- @+node:gcross.20091113142219.2502:BandwidthInt
newtype BandwidthInt = BI Int deriving (Show,Eq)
instance Arbitrary BandwidthInt where
    arbitrary = choose (1,1000) >>= return.BI
-- @-node:gcross.20091113142219.2502:BandwidthInt
-- @+node:gcross.20091113142219.2504:PhysicalDimensionInt
newtype PhysicalDimensionInt = PDI Int deriving (Show,Eq)
instance Arbitrary PhysicalDimensionInt where
    arbitrary = choose (2,4) >>= return.PDI
-- @-node:gcross.20091113142219.2504:PhysicalDimensionInt
-- @+node:gcross.20091116175016.1812:Complex Double
instance Arbitrary (Complex Double) where
    arbitrary = liftM2 (:+) arbitrary arbitrary
-- @-node:gcross.20091116175016.1812:Complex Double
-- @-node:gcross.20091113142219.2499:Generators
-- @+node:gcross.20091113142219.2508:Helpers
-- @+node:gcross.20091114174920.1734:assertAlmostEqual
assertAlmostEqual :: (Show a, AlmostEq a) => String -> a -> a -> Assertion
assertAlmostEqual message x y
    | x ~= y     = return ()
    | otherwise  = assertFailure $ message ++ " (" ++ show x ++ " /~ " ++ show y ++ ")"
-- @-node:gcross.20091114174920.1734:assertAlmostEqual
-- @-node:gcross.20091113142219.2508:Helpers
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091111171052.1640:<< Tests >>
    -- @+others
    -- @+node:gcross.20091116222034.1792:VMPS.Miscellaneous
    [testGroup "VMPS.Miscellaneous"
        -- @    @+others
        -- @+node:gcross.20091116222034.1795:dotArrays
        [testGroup "dotArrays"
            -- @    @+others
            -- @+node:gcross.20091116222034.1796:known correct result
            [testCase "known correct result" $ do
                arr1 <- newListArray (1,4)
                    [( 0.07331006) :+ ( 0.54975918)
                    ,(-0.76662781) :+ ( 0.58918237)
                    ,(-0.58884695) :+ (-0.14558789)
                    ,(-0.51358300) :+ (-0.50234503)
                    ]
                arr2 <- newListArray (1,4)
                    [( 0.46947762) :+ ( 0.35345517)
                    ,(-0.36112978) :+ ( 0.18917933)
                    ,(-0.84684445) :+ ( 0.55999172)
                    ,(-0.69551880) :+ (-0.97433324)
                    ]
                actual_value <-
                    withStorableArray arr1 $ \p1 ->
                    withStorableArray arr2 $ \p2 ->
                        dotArrays 4 p1 p2
                assertAlmostEqual
                    "Was the correct value returned?"
                    (1.88083777 :+ (-0.46647579))
                    actual_value
            -- @-node:gcross.20091116222034.1796:known correct result
            -- @-others
            ]
        -- @-node:gcross.20091116222034.1795:dotArrays
        -- @+node:gcross.20091116222034.1798:normalize
        ,testProperty "normalize -- does it work?" $
            \(numbers :: [Complex Double]) -> ((not . null) numbers) ==>
            unsafePerformIO $ do
                let number_of_numbers = length numbers
                arr <- newListArray (1,number_of_numbers) numbers
                overlap <- withStorableArray arr $ \p -> do
                    normalize number_of_numbers p
                    dotArrays number_of_numbers p p
                return (overlap ~= 1)
        -- @-node:gcross.20091116222034.1798:normalize
        -- @+node:gcross.20091116222034.1800:orthogonalize2 -- does it work?
        ,testProperty "orthogonalize2 -- does it work?" $
            \(numbers :: [(Complex Double,Complex Double)]) -> (length numbers >= 2) ==>
            unsafePerformIO $ do
                let number_of_numbers = length numbers
                    (numbers1, numbers2) = unzip numbers
                arr1 <- newListArray (1,number_of_numbers) numbers1
                arr2 <- newListArray (1,number_of_numbers) numbers2
                (norm1,norm2,overlap1,overlap2) <-
                    withStorableArray arr1 $ \p1 ->
                    withStorableArray arr2 $ \p2 -> do
                        orthogonalize2 number_of_numbers p1 p2
                        liftM4 (,,,)
                            (dotArrays number_of_numbers p1 p1)
                            (dotArrays number_of_numbers p2 p2)
                            (dotArrays number_of_numbers p1 p2)
                            (dotArrays number_of_numbers p2 p1)
                return $ and [(norm1 ~= 1),(norm2 ~= 1),(overlap1 ~= 0),(overlap2 ~= 0)]
        -- @-node:gcross.20091116222034.1800:orthogonalize2 -- does it work?
        -- @-others
        ]
    -- @-node:gcross.20091116222034.1792:VMPS.Miscellaneous
    -- @+node:gcross.20091113142219.1700:VMPS.Wrapper
    ,testGroup "VMPS.Wrapper"
        -- @    @+others
        -- @+node:gcross.20091111171052.1649:computeExpectation
        [testGroup "computeExpectation"
            -- @    @+others
            -- @+node:gcross.20091111171052.1650:trivial, all dimensions 1
            [testCase "trivial, all dimensions 1" $
                let left_boundary = trivial_left_boundary
                    right_boundary = trivial_right_boundary
                    state_site_tensor = UnnormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    operator_indices = unsafePerformIO $ newArray ((1,1),(1,2)) 1
                    operator_matrices = unsafePerformIO $ newListArray ((1,1,1),(1,1,1)) [1.0 + 0.0]
                    operator_site_tensor = OperatorSiteTensor 1 1 1 1 operator_indices operator_matrices
                    expectation = computeExpectation left_boundary state_site_tensor operator_site_tensor right_boundary
                in assertEqual "is the correct result returned?" (1 :+ 0) expectation
            -- @nonl
            -- @-node:gcross.20091111171052.1650:trivial, all dimensions 1
            -- @+node:gcross.20091113142219.1683:trivial, all dimensions 1, imaginary
            ,testCase "trivial, all dimensions 1, imaginary" $
                let left_boundary = trivial_left_boundary
                    right_boundary = trivial_right_boundary
                    state_site_tensor = UnnormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    operator_indices = unsafePerformIO $ newArray ((1,1),(1,2)) 1
                    operator_matrices = unsafePerformIO $ newListArray ((1,1,1),(1,1,1)) [0 :+ 1]
                    operator_site_tensor = OperatorSiteTensor 1 1 1 1 operator_indices operator_matrices
                    expectation = computeExpectation left_boundary state_site_tensor operator_site_tensor right_boundary
                in assertEqual "is the correct result returned?" (0 :+ 1) expectation
            -- @nonl
            -- @-node:gcross.20091113142219.1683:trivial, all dimensions 1, imaginary
            -- @+node:gcross.20091111171052.1655:trivial, d =2
            ,testCase "trivial, d = 2" $
                assertEqual "are the correct results returned for all calls?"
                [1,1,-1,-1]
                $
                map (\state ->
                    let left_boundary = trivial_left_boundary
                        right_boundary = trivial_right_boundary
                        state_site_tensor = UnnormalizedStateSiteTensor . StateSiteTensor 2 1 1 . complexTensorFromList (2,1,1) $ state
                        operator_site_tensor = makeOperatorSiteTensorFromSpecification 1 1 [(1 --> 1) pZ]
                    in computeExpectation left_boundary state_site_tensor operator_site_tensor right_boundary
                ) [[1,0],[0:+1,0],[0,1],[0,0:+1]]
            -- @nonl
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
                    state_site_tensor = UnnormalizedStateSiteTensor . StateSiteTensor 4 1 1 . complexTensorFromList (4,1,1) $ replicate (4*2) 1
                    operator_site_tensor =
                        makeOperatorSiteTensorFromSpecification 1 1
                            [(1 --> 1) $
                              (SingleSiteOperator $
                                (1 :. 0 :. 0 :.   0 :. ()) :.
                                (0 :. 1 :. 0 :.   0 :. ()) :.
                                (0 :. 0 :. 1 :.   0 :. ()) :.
                                (0 :. 0 :. 0 :. (-1):. ()) :.
                              () :: SingleSiteOperator N4 )
                             ]
                    optimizer_result = 
                        computeOptimalSiteStateTensor
                            left_boundary
                            state_site_tensor
                            operator_site_tensor
                            right_boundary
                            NullProjectorMatrix
                            SR
                            0
                            10000
                in case optimizer_result of
                    Left failure_reason -> assertFailure $ "Optimizer failed: " ++ (show failure_reason)
                    Right (_, eigenvalue, (UnnormalizedStateSiteTensor (StateSiteTensor d bl br actual_tensor))) -> do
                        let components = toListOfComplexNumbers actual_tensor
                        assertEqual "is the new left bandwidth dimension the same?" (leftBandwidthOfState state_site_tensor) bl
                        assertEqual "is the new right bandwidth dimension the same?" (rightBandwidthOfState state_site_tensor) br
                        assertEqual "is the new physical dimension the same?" (physicalDimensionOfState state_site_tensor) d
                        assertBool "are all but the last component of the state zero?" (take 3 components ~= replicate 3 0)
                        assertBool "is the last components non-zero?" (last components /~ 0)
                        assertBool "is the eigenvalue correct?" (eigenvalue ~= (-1))
            -- @-node:gcross.20091111171052.1661:trivial, d = 4
            -- @-others
            ]
        -- @-node:gcross.20091111171052.1659:computeOptimalSiteStateTensor
        -- @+node:gcross.20091112145455.1629:contractSOSLeft
        ,testGroup "contractSOSLeft"
            -- @    @+others
            -- @+node:gcross.20091112145455.1630:trivial, all dimensions 1
            [testCase "trivial, all dimensions 1" $
                let left_boundary = trivial_left_boundary
                    state_site_tensor = LeftAbsorptionNormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    operator_indices = unsafePerformIO $ newArray ((1,1),(1,2)) 1
                    operator_matrices = unsafePerformIO $ newListArray ((1,1,1),(1,1,1)) [1]
                    operator_site_tensor = OperatorSiteTensor 1 1 1 1 operator_indices operator_matrices
                    LeftBoundaryTensor (BoundaryTensor cr br actual_tensor) = contractSOSLeft left_boundary state_site_tensor operator_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the new state bandwidth dimension correct?" (rightBandwidthOfState state_site_tensor) br
                    assertEqual "is the new operator bandwidth dimension correct?" (operatorRightBandwidth operator_site_tensor) cr
                    assertBool "are the components correct?" (components ~= [1])
            -- @nonl
            -- @-node:gcross.20091112145455.1630:trivial, all dimensions 1
            -- @+node:gcross.20091112145455.1633:trivial, c = 2
            ,testCase "trivial, c = 2" $
                let left_boundary = trivial_left_boundary
                    state_site_tensor = LeftAbsorptionNormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    operator_indices = unsafePerformIO $ newListArray ((1,1),(1,2)) [1,2]
                    operator_matrices = unsafePerformIO $ newListArray ((1,1,1),(1,1,1)) [0 :+ 1]
                    operator_site_tensor = OperatorSiteTensor 1 2 1 1 operator_indices operator_matrices
                    LeftBoundaryTensor (BoundaryTensor cr br actual_tensor) = contractSOSLeft left_boundary state_site_tensor operator_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the new state bandwidth dimension correct?" (rightBandwidthOfState state_site_tensor) br
                    assertEqual "is the new operator bandwidth dimension correct?" (operatorRightBandwidth operator_site_tensor) cr
                    assertBool "are the components correct?" (components ~= [0,0 :+ 1])
            -- @nonl
            -- @-node:gcross.20091112145455.1633:trivial, c = 2
            -- @+node:gcross.20091112145455.1641:contractSOSRight
            ,testGroup "contractSOSRight"
                -- @    @+others
                -- @+node:gcross.20091112145455.1642:trivial, all dimensions 1
                [testCase "trivial, all dimensions 1" $
                    let right_boundary = trivial_right_boundary
                        state_site_tensor = RightAbsorptionNormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                        operator_indices = unsafePerformIO $ newArray ((1,1),(1,2)) 1
                        operator_matrices = unsafePerformIO $ newListArray ((1,1,1),(1,1,1)) [1]
                        operator_site_tensor = OperatorSiteTensor 1 1 1 1 operator_indices operator_matrices
                        RightBoundaryTensor (BoundaryTensor cr br actual_tensor) = contractSOSRight right_boundary state_site_tensor operator_site_tensor
                        components = toListOfComplexNumbers actual_tensor
                    in do
                        assertEqual "is the new state bandwidth dimension correct?" (leftBandwidthOfState state_site_tensor) br
                        assertEqual "is the new operator bandwidth dimension correct?" (operatorLeftBandwidth operator_site_tensor) cr
                        assertBool "are the components correct?" (components ~= [1.0,0.0])
                -- @nonl
                -- @-node:gcross.20091112145455.1642:trivial, all dimensions 1
                -- @+node:gcross.20091112145455.1643:trivial, c = 2
                ,testCase "trivial, c = 2" $
                    let right_boundary = trivial_right_boundary
                        state_site_tensor = RightAbsorptionNormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                        operator_indices = unsafePerformIO $ newListArray ((1,1),(1,2)) [2,1]
                        operator_matrices = unsafePerformIO $ newListArray ((1,1,1),(1,1,1)) [0 :+ 1]
                        operator_site_tensor = OperatorSiteTensor 2 1 1 1 operator_indices operator_matrices
                        RightBoundaryTensor (BoundaryTensor cl bl actual_tensor) = contractSOSRight right_boundary state_site_tensor operator_site_tensor
                        components = toListOfComplexNumbers actual_tensor
                    in do
                        assertEqual "is the new state bandwidth dimension correct?" (leftBandwidthOfState state_site_tensor) bl
                        assertEqual "is the new operator bandwidth dimension correct?" (operatorLeftBandwidth operator_site_tensor) cl
                        assertBool "are the components correct?" (components ~= [0,0 :+ 1])
                -- @-node:gcross.20091112145455.1643:trivial, c = 2
                -- @-others
                ]
            -- @-node:gcross.20091112145455.1641:contractSOSRight
            -- @-others
            ]
        -- @-node:gcross.20091112145455.1629:contractSOSLeft
        -- @+node:gcross.20091116175016.1776:contractSSLeft
        ,testGroup "contractSSLeft"
            -- @    @+others
            -- @+node:gcross.20091116175016.1777:trivial, all dimensions 1
            [testCase "trivial, all dimensions 1" $
                let left_boundary = trivial_left_overlap_boundary
                    overlap_site_tensor = LeftAbsorptionNormalizedOverlapSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    state_site_tensor = LeftAbsorptionNormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    LeftOverlapBoundaryTensor (OverlapBoundaryTensor ob nb actual_tensor) = contractSSLeft state_site_tensor left_boundary overlap_site_tensor 
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the old state bandwidth dimension correct?" (rightBandwidthOfState overlap_site_tensor) ob
                    assertEqual "is the new state bandwidth dimension correct?" (rightBandwidthOfState state_site_tensor) nb
                    assertBool "are the components correct?" (components ~= [1])
            -- @-node:gcross.20091116175016.1777:trivial, all dimensions 1
            -- @+node:gcross.20091116175016.1780:trivial, physical dimensions 2
            ,testCase "trivial, physical dimensions 2" $
                let left_boundary = trivial_left_overlap_boundary
                    overlap_site_tensor =
                        LeftAbsorptionNormalizedOverlapSiteTensor
                        .
                        StateSiteTensor 2 1 1
                        .
                        complexTensorFromList (2,1,1)
                        $
                        [5,-1]
                    state_site_tensor =
                        LeftAbsorptionNormalizedStateSiteTensor
                        .
                        StateSiteTensor 2 1 1
                        .
                        complexTensorFromList (2,1,1)
                        $
                        [1,1]
                    LeftOverlapBoundaryTensor (OverlapBoundaryTensor ob nb actual_tensor) = contractSSLeft state_site_tensor left_boundary overlap_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the old state bandwidth dimension correct?" (rightBandwidthOfState overlap_site_tensor) ob
                    assertEqual "is the new state bandwidth dimension correct?" (rightBandwidthOfState state_site_tensor) nb
                    assertAlmostEqual "are the components correct?" [4] components
            -- @-node:gcross.20091116175016.1780:trivial, physical dimensions 2
            -- @+node:gcross.20091116175016.1782:trivial, varied bandwidth dimensions
            ,testCase "trivial, varied bandwidth dimensions" $
                let left_boundary =
                        LeftOverlapBoundaryTensor
                        .
                        OverlapBoundaryTensor 3 2
                        .
                        complexTensorFromList (3,2)
                        $
                        [2,2
                        ,1,1
                        ,3,3
                        ]
                    overlap_site_tensor =
                        LeftAbsorptionNormalizedOverlapSiteTensor
                        .
                        StateSiteTensor 1 3 2
                        .
                        complexTensorFromList (1,3,2)
                        $
                        [1,-1,1
                        ,1,1,-1]
                    state_site_tensor =
                        LeftAbsorptionNormalizedStateSiteTensor
                        .
                        StateSiteTensor 1 2 2
                        .
                        complexTensorFromList (1,2,2)
                        $
                        [1,1
                        ,2,-2
                        ]
                    LeftOverlapBoundaryTensor (OverlapBoundaryTensor ob nb actual_tensor) = contractSSLeft state_site_tensor left_boundary overlap_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the old state bandwidth dimension correct?" (rightBandwidthOfState overlap_site_tensor) ob
                    assertEqual "is the new state bandwidth dimension correct?" (rightBandwidthOfState state_site_tensor) nb
                    assertAlmostEqual "are the components correct?" [12,-4,0,0] components
            -- @-node:gcross.20091116175016.1782:trivial, varied bandwidth dimensions
            -- @-others
            ]
        -- @-node:gcross.20091116175016.1776:contractSSLeft
        -- @+node:gcross.20091116175016.1789:contractSSRight
        ,testGroup "contractSSRight"
            -- @    @+others
            -- @+node:gcross.20091116175016.1791:trivial, all dimensions 1
            [testCase "trivial, all dimensions 1" $
                let right_boundary = trivial_right_overlap_boundary
                    overlap_site_tensor = RightAbsorptionNormalizedOverlapSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    state_site_tensor = RightAbsorptionNormalizedStateSiteTensor $ StateSiteTensor 1 1 1 trivial_complex_tensor
                    RightOverlapBoundaryTensor (OverlapBoundaryTensor ob nb actual_tensor) = contractSSRight state_site_tensor right_boundary overlap_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the old state bandwidth dimension correct?" (leftBandwidthOfState overlap_site_tensor) ob
                    assertEqual "is the new state bandwidth dimension correct?" (leftBandwidthOfState state_site_tensor) nb
                    assertBool "are the components correct?" (components ~= [1])
            -- @-node:gcross.20091116175016.1791:trivial, all dimensions 1
            -- @+node:gcross.20091116175016.1793:trivial, physical dimensions 2
            ,testCase "trivial, physical dimensions 2" $
                let right_boundary = trivial_right_overlap_boundary
                    overlap_site_tensor =
                        RightAbsorptionNormalizedOverlapSiteTensor
                        .
                        StateSiteTensor 2 1 1
                        .
                        complexTensorFromList (2,1,1)
                        $
                        [5,-1]
                    state_site_tensor =
                        RightAbsorptionNormalizedStateSiteTensor
                        .
                        StateSiteTensor 2 1 1
                        .
                        complexTensorFromList (2,1,1)
                        $
                        [1,1]
                    RightOverlapBoundaryTensor (OverlapBoundaryTensor ob nb actual_tensor) = contractSSRight state_site_tensor right_boundary overlap_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the old state bandwidth dimension correct?" (leftBandwidthOfState overlap_site_tensor) ob
                    assertEqual "is the new state bandwidth dimension correct?" (leftBandwidthOfState state_site_tensor) nb
                    assertAlmostEqual "are the components correct?" [4] components
            -- @-node:gcross.20091116175016.1793:trivial, physical dimensions 2
            -- @+node:gcross.20091116175016.1795:trivial, varied bandwidth dimensions
            ,testCase "trivial, varied bandwidth dimensions" $
                let right_boundary =
                        RightOverlapBoundaryTensor
                        .
                        OverlapBoundaryTensor 3 2
                        .
                        complexTensorFromList (3,2)
                        $
                        [2,1,3
                        ,2,1,3
                        ]
                    overlap_site_tensor =
                        RightAbsorptionNormalizedOverlapSiteTensor
                        .
                        StateSiteTensor 1 2 3
                        .
                        complexTensorFromList (1,2,3)
                        $
                        [ 1, 1
                        ,-1, 1
                        , 1,-1
                        ]
                    state_site_tensor =
                        RightAbsorptionNormalizedStateSiteTensor
                        .
                        StateSiteTensor 1 2 2
                        .
                        complexTensorFromList (1,2,2)
                        $
                        [1, 2
                        ,1,-2
                        ]
                    RightOverlapBoundaryTensor (OverlapBoundaryTensor ob nb actual_tensor) = contractSSRight state_site_tensor right_boundary overlap_site_tensor
                    components = toListOfComplexNumbers actual_tensor
                in do
                    assertEqual "is the old state bandwidth dimension correct?" (leftBandwidthOfState overlap_site_tensor) ob
                    assertEqual "is the new state bandwidth dimension correct?" (leftBandwidthOfState state_site_tensor) nb
                    assertAlmostEqual "are the components correct?" [12,0,-4,0] components
            -- @-node:gcross.20091116175016.1795:trivial, varied bandwidth dimensions
            -- @-others
            ]
        -- @-node:gcross.20091116175016.1789:contractSSRight
        -- @+node:gcross.20091112145455.1660:generateRandomizedStateTensor
        ,testGroup "generateRandomizedUnnormalizedSiteStateTensor"
            -- @    @+others
            -- @+node:gcross.20091112145455.1667:unnormalized
            [testGroup "unnormalized"
                -- @    @+others
                -- @+node:gcross.20091112145455.1661:selected dimensions
                [testCase "bl = 1, br = 2, d = 3" $ do
                    state_site_tensor <- generateRandomizedStateSiteTensor 3 1 2 :: IO (UnnormalizedStateSiteTensor)
                    assertEqual "is the left bandwidth dimension correct?" 1 (leftBandwidthOfState state_site_tensor)
                    assertEqual "is the right bandwidth dimension correct?" 2 (rightBandwidthOfState state_site_tensor)
                    assertEqual "is the physical bandwidth dimension correct?" 3 (physicalDimensionOfState state_site_tensor)
                -- @-node:gcross.20091112145455.1661:selected dimensions
                -- @-others
                ]
            -- @-node:gcross.20091112145455.1667:unnormalized
            -- @+node:gcross.20091112145455.1665:normalized
            ,testGroup "normalized"
                -- @    @+others
                -- @+node:gcross.20091112145455.1666:selected dimensions
                [testCase "bl = 1, br = 4, d = 8" $ do
                    state_site_tensor <- generateRandomizedStateSiteTensor 8 1 4 :: IO (RightAbsorptionNormalizedStateSiteTensor)
                    assertEqual "is the left bandwidth dimension correct?" 1 (leftBandwidthOfState state_site_tensor)
                    assertEqual "is the right bandwidth dimension correct?" 4 (rightBandwidthOfState state_site_tensor)
                    assertEqual "is the physical bandwidth dimension correct?" 8 (physicalDimensionOfState state_site_tensor)
                    let normalization =
                            sum
                            .
                            map ((** 2) . magnitude)
                            .
                            toListOfComplexNumbers
                            .
                            stateData
                            .
                            unwrapRightAbsorptionNormalizedStateSiteTensor
                            $
                            state_site_tensor
                    assertBool "is the state site tensor properly normalized?" (1 ~= normalization)
                -- @-node:gcross.20091112145455.1666:selected dimensions
                -- @-others
                ]
            -- @-node:gcross.20091112145455.1665:normalized
            -- @-others
            ]
        -- @-node:gcross.20091112145455.1660:generateRandomizedStateTensor
        -- @+node:gcross.20100521141104.1774:generateRandomizedProjectorMatrix
        ,testProperty "generateRandomizedProjectorMatrix" $ do
            projector_length <- choose (1,10)
            number_of_projectors <- choose (0,projector_length-1)
            return $
                case (unsafePerformIO $ generateRandomizedProjectorMatrix projector_length number_of_projectors) of
                    NullProjectorMatrix ->
                        number_of_projectors == 0
                    ProjectorMatrix number_of_projectors_ projector_length_ _ ->
                        (projector_length_ == projector_length)
                        &&
                        (number_of_projectors_ == number_of_projectors)
        -- @-node:gcross.20100521141104.1774:generateRandomizedProjectorMatrix
        -- @+node:gcross.20091116222034.1801:formProjectorMatrix
        ,testGroup "formProjectorMatrix" $
            let 
            -- @nonl
            -- @<< checkMatrixOrthonormality >>
            -- @+node:gcross.20091116222034.1808:<< checkMatrixOrthonormality >>
            checkMatrixOrthonormality :: ComplexTensor (Int,Int) -> Assertion
            checkMatrixOrthonormality (ComplexTensor raw_data) =
                withStorableArray raw_data $ \p_raw_data -> do
                    (number_of_projectors,projector_length) <-
                        fmap snd $ getBounds raw_data
                    let increment = projector_length * sizeOf (undefined :: Complex Double)
                        projectors = map (plusPtr p_raw_data . (increment *)) [0..number_of_projectors-1]
                        checkProjectors [] = return ()
                        checkProjectors (projector:rest_projectors) = do
                                dotArrays projector_length projector projector
                                    >>= assertAlmostEqual "is the projector normalized?" 1
                                forM rest_projectors $ \other_projector ->
                                    dotArrays projector_length projector other_projector
                                        >>= assertAlmostEqual "are the other projectors orthogonal?" 0
                                checkProjectors rest_projectors
                    checkProjectors projectors
            -- @-node:gcross.20091116222034.1808:<< checkMatrixOrthonormality >>
            -- @nl
            in
               -- @       @+others
               -- @+node:gcross.20091116222034.1802:null case
               [testCase "null case" $ 
                   case formProjectorMatrix [] of
                       NullProjectorMatrix -> return ()
                       _ -> assertFailure "non-null projector returned when applied to the empty list"
               -- @-node:gcross.20091116222034.1802:null case
               -- @+node:gcross.20091116222034.1803:trivial case
               ,testCase "trivial, all dimensions 1" $
                   let left_boundary = trivial_left_overlap_boundary
                       right_boundary = trivial_right_overlap_boundary
                       overlap_site_tensor =
                           UnnormalizedOverlapSiteTensor
                           .
                           StateSiteTensor 1 1 1
                           $
                           complexTensorFromList (1,1,1) [0.5]
                       projector_matrix = formProjectorMatrix
                           [(left_boundary,right_boundary,overlap_site_tensor)]       
                   in case projector_matrix of
                       NullProjectorMatrix -> assertFailure "null projector matrix returned"
                       ProjectorMatrix number_of_projectors projector_length projector_matrix -> do
                           assertEqual "is the projector count correct?" 1 number_of_projectors
                           assertEqual "is the projector length correct?" 1 projector_length
                           assertAlmostEqual "is the overlap matrix correct?"
                               [1]
                               (toListOfComplexNumbers projector_matrix)
               -- @-node:gcross.20091116222034.1803:trivial case
               -- @+node:gcross.20091116222034.1805:d = 4, one projector
               ,testCase "d = 4, one projector" $
                   let left_boundary = trivial_left_overlap_boundary
                       right_boundary = trivial_right_overlap_boundary
                       overlap_site_tensor =
                           UnnormalizedOverlapSiteTensor
                           .
                           StateSiteTensor 4 1 1
                           $
                           complexTensorFromList (4,1,1) [1,1,1,1]
                       projector_matrix = formProjectorMatrix
                           [(left_boundary,right_boundary,overlap_site_tensor)]       
                   in case projector_matrix of
                       NullProjectorMatrix -> assertFailure "null projector matrix returned"
                       ProjectorMatrix number_of_projectors projector_length projector_matrix -> do
                           assertEqual "is the projector count correct?" 1 number_of_projectors
                           assertEqual "is the projector length correct?" 4 projector_length
                           assertAlmostEqual "is the overlap matrix correct?"
                               [0.5,0.5,0.5,0.5]
                               ((\lst@(x:_) -> let sign = abs x/x in map (*sign) lst) . toListOfComplexNumbers $ projector_matrix)
               -- @-node:gcross.20091116222034.1805:d = 4, one projector
               -- @+node:gcross.20091116222034.1807:d = 4, two projectors
               ,testCase "d = 4, two projectors" $
                   let projector_matrix =
                           formProjectorMatrix
                           .
                           map (
                               (,,)
                                   trivial_left_overlap_boundary
                                   trivial_right_overlap_boundary           
                               .
                               UnnormalizedOverlapSiteTensor
                               .
                               StateSiteTensor 4 1 1
                               .
                               complexTensorFromList (4,1,1)
                           )
                           $            
                           [[1, 1,1,1]
                           ,[1,-1,1,1]
                           ]
                   in case projector_matrix of
                       NullProjectorMatrix -> assertFailure "null projector matrix returned"
                       ProjectorMatrix number_of_projectors projector_length projector_matrix -> do
                           assertEqual "is the projector count correct?" 2 number_of_projectors
                           assertEqual "is the projector length correct?" 4 projector_length
                           checkMatrixOrthonormality projector_matrix
               -- @-node:gcross.20091116222034.1807:d = 4, two projectors
               -- @+node:gcross.20091116222034.1810:d = 4, three projectors
               ,testCase "d = 4, three projectors" $
                   let projector_matrix =
                           formProjectorMatrix
                           .
                           map (
                               (,,)
                                   trivial_left_overlap_boundary
                                   trivial_right_overlap_boundary           
                               .
                               UnnormalizedOverlapSiteTensor
                               .
                               StateSiteTensor 4 1 1
                               .
                               complexTensorFromList (4,1,1)
                           )
                           $            
                           [[1, 1, 1,1]
                           ,[1,-1, 1,1]
                           ,[1,-1,-1,1]
                           ]
                   in case projector_matrix of
                       NullProjectorMatrix -> assertFailure "null projector matrix returned"
                       ProjectorMatrix number_of_projectors projector_length projector_matrix -> do
                           assertEqual "is the projector count correct?" 3 number_of_projectors
                           assertEqual "is the projector length correct?" 4 projector_length
                           checkMatrixOrthonormality projector_matrix
               -- @-node:gcross.20091116222034.1810:d = 4, three projectors
               -- @-others
               ]
        -- @-node:gcross.20091116222034.1801:formProjectorMatrix
        -- @+node:gcross.20100521141104.1778:applyProjectorMatrix
        ,testProperty "computeOverlapWithProjectors . applyProjectorMatrix = 0" $ do
            d <- choose (1,4)
            bl <- choose (1,4)
            br <- choose (1,4)
            let projector_length = d*bl*br
            number_of_projectors <- choose (0,projector_length-1)
            let projector_matrix = unsafePerformIO $ generateRandomizedProjectorMatrix projector_length number_of_projectors
            return $
                (~= 0)
                .
                echo
                .
                computeOverlapWithProjectors projector_matrix
                .
                applyProjectorMatrix projector_matrix
                .
                unsafePerformIO
                $
                generateRandomizedStateSiteTensor d bl br
        -- @-node:gcross.20100521141104.1778:applyProjectorMatrix
        -- @-others
        ]
    -- @-node:gcross.20091113142219.1700:VMPS.Wrapper
    -- @+node:gcross.20091113142219.1701:VMPS.EnergyMinimizationChain
    ,testGroup "VMPS.EnergyMinimizationChain"
        -- @    @+others
        -- @+node:gcross.20091113142219.1853:computeBandwidthDimensionSequence
        [testGroup "computeBandwidthDimensionSequence"
            -- @    @+others
            -- @+node:gcross.20091113142219.2507:has the right number of entries
            [testProperty "has the right number of entries" $ do
                requested_bandwidth_dimension <- choose (1,100000)
                number_of_sites <- choose (20,1000)
                physical_dimensions <- vectorOf number_of_sites (choose (2,10))
                return $
                    length (computeBandwidthDimensionSequence requested_bandwidth_dimension physical_dimensions) == number_of_sites + 1
            -- @-node:gcross.20091113142219.2507:has the right number of entries
            -- @+node:gcross.20091113142219.2505:gets there eventually
            ,testProperty "gets to the requested bandwidth eventually" $ do
                requested_bandwidth_dimension <- choose (1,100000)
                number_of_sites <- choose (20,1000)
                physical_dimensions <- vectorOf number_of_sites (choose (2,10))
                return $
                    maximum (computeBandwidthDimensionSequence requested_bandwidth_dimension physical_dimensions) == requested_bandwidth_dimension
            -- @-node:gcross.20091113142219.2505:gets there eventually
            -- @-others
            ]
        -- @-node:gcross.20091113142219.1853:computeBandwidthDimensionSequence
        -- @+node:gcross.20091113142219.2532:computeSiteDimensionSequence
        ,testGroup "computeSiteDimensionSequence"
            -- @    @+others
            -- @+node:gcross.20091113142219.2534:has the right number of entries
            [testProperty "has the right number of entries" $ do
                requested_bandwidth_dimension <- choose (1,100000)
                number_of_sites <- choose (20,1000)
                physical_dimensions <- vectorOf number_of_sites (choose (2,10))
                return $
                    length (computeSiteDimensionSequence requested_bandwidth_dimension physical_dimensions) == number_of_sites
            -- @-node:gcross.20091113142219.2534:has the right number of entries
            -- @+node:gcross.20091113142219.2516:doesn't grow too fast from the left
            ,testProperty "doesn't grow too fast from the left" $ do
                requested_bandwidth_dimension <- choose (1,100000)
                number_of_sites <- choose (20,1000)
                physical_dimensions <- vectorOf number_of_sites (choose (2,10))
                return . all (\(d,bl,br) -> br <= d*bl) $
                    computeSiteDimensionSequence requested_bandwidth_dimension physical_dimensions
            -- @-node:gcross.20091113142219.2516:doesn't grow too fast from the left
            -- @+node:gcross.20091113142219.2518:doesn't grow too fast from the right
            ,testProperty "doesn't grow too fast from the right" $ do
                requested_bandwidth_dimension <- choose (1,100000)
                number_of_sites <- choose (20,1000)
                physical_dimensions <- vectorOf number_of_sites (choose (2,10))
                return . all (\(d,bl,br) -> bl <= d*br) $
                    computeSiteDimensionSequence requested_bandwidth_dimension physical_dimensions
            -- @-node:gcross.20091113142219.2518:doesn't grow too fast from the right
            -- @+node:gcross.20091113142219.2511:complains if too large
            ,testProperty "complains if too large" $ do
                number_of_sites <- choose (10,20)
                physical_dimensions <- vectorOf number_of_sites (choose (2,10))
                let requested_bandwidth_dimension = 2 * product physical_dimensions
                return . (== Nothing) . spoon $
                    computeSiteDimensionSequence requested_bandwidth_dimension physical_dimensions
            -- @-node:gcross.20091113142219.2511:complains if too large
            -- @-others
            ]
        -- @-node:gcross.20091113142219.2532:computeSiteDimensionSequence
        -- @+node:gcross.20091114174920.1728:Chain energy invariant under movement
        ,testGroup "Chain energy invariant under movement" $
            let 
                -- @        @+others
                -- @+node:gcross.20091114174920.1738:createEnergyInvarianceTest
                createEnergyInvarianceTest :: OperatorDimension n => SingleSiteOperator n -> Int -> Int -> Assertion
                createEnergyInvarianceTest identity_operator number_of_sites bandwidth_dimension = do
                    chain <-
                        generateRandomizedChain bandwidth_dimension
                        .
                        replicate number_of_sites
                        .
                        makeOperatorSiteTensorFromSpecification 1 1 
                        $
                        [(1 --> 1) identity_operator]
                    let chains_going_right =
                            take number_of_sites
                            .
                            iterate activateRightNeighbor
                            $
                            chain
                        chains_going_left =
                            take number_of_sites
                            .
                            iterate activateLeftNeighbor
                            .
                            last
                            $
                            chains_going_right
                        correct_energy = chainEnergy chain
                    forM_ (zip [1..] . map chainEnergy $ chains_going_right) $ \(site_number::Int,energy) ->
                        assertAlmostEqual
                            (printf "Did the energy change after moving right to site %i?" site_number)
                            correct_energy energy
                    forM_ (zip [number_of_sites,number_of_sites-1..] . map chainEnergy $ chains_going_left) $ \(site_number,energy) ->
                        assertAlmostEqual
                            (printf "Did the energy change after moving left to site %i?" site_number)
                            correct_energy energy
                -- @-node:gcross.20091114174920.1738:createEnergyInvarianceTest
                -- @+node:gcross.20100505180122.1723:runTestsForIdentityOperator
                runTestsForIdentityOperator :: OperatorDimension n => SingleSiteOperator n -> Test.Framework.Test
                runTestsForIdentityOperator identity_operator =
                    let physical_dimension = physicalDimensionOfSingleSiteOperator identity_operator
                    in testGroup (printf "physical dimension = %i" physical_dimension) $
                       map (\(number_of_sites,bandwidth_dimension) ->
                            testCase (printf "%i sites, bandwidth = %i" number_of_sites bandwidth_dimension) $ 
                                createEnergyInvarianceTest identity_operator number_of_sites bandwidth_dimension
                       ) $
                       concat
                        [[ ( 2,b) | b <- [1,2]]
                        ,[ ( 3,b) | b <- [1,2]]
                        ,[ ( 4,b) | b <- [1,2,4]]
                        ,[ ( 5,b) | b <- [1,2,4]]
                        ,[ (10,b) | b <- [1,2,4,8,16]]
                        ]
                -- @-node:gcross.20100505180122.1723:runTestsForIdentityOperator
                -- @-others
            in  [runTestsForIdentityOperator (identity :: SingleSiteOperator N2)
                ,runTestsForIdentityOperator (identity :: SingleSiteOperator N3)
                ,runTestsForIdentityOperator (identity :: SingleSiteOperator N4)
                ,runTestsForIdentityOperator (identity :: SingleSiteOperator N5)
                ]
        -- @-node:gcross.20091114174920.1728:Chain energy invariant under movement
        -- @+node:gcross.20091115105949.1747:Chain energy invariant under bandwidth increase
        ,testGroup "Chain energy invariant under bandwidth increase" $
            let
                -- @        @+others
                -- @+node:gcross.20091115105949.1748:createBandwidthIncreaseTest
                createBandwidthIncreaseTest :: OperatorDimension n => SingleSiteOperator n -> Int -> Int -> Int -> Assertion
                createBandwidthIncreaseTest operator number_of_sites old_bandwidth_dimension new_bandwidth_dimension = do
                    original_chain <-
                        generateRandomizedChain old_bandwidth_dimension
                        .
                        replicate number_of_sites
                        .
                        makeOperatorSiteTensorFromSpecification 1 1
                        $
                        [(1 --> 1) operator]
                    chain <- increaseChainBandwidth new_bandwidth_dimension original_chain
                    let chains_going_right =
                            take number_of_sites
                            .
                            iterate activateRightNeighbor
                            $
                            chain
                        chains_going_left =
                            take number_of_sites
                            .
                            iterate activateLeftNeighbor
                            .
                            last
                            $
                            chains_going_right
                        correct_energy = chainEnergy original_chain
                    forM_ (zip [1..] . map chainEnergy $ chains_going_right) $ \(site_number::Int,energy) ->
                        assertAlmostEqual
                            (printf "Did the energy change at site %i?" site_number)
                            correct_energy energy
                    forM_ (zip [number_of_sites,number_of_sites-1..] . map chainEnergy $ chains_going_left) $ \(site_number,energy) ->
                        assertAlmostEqual
                            (printf "Did the energy change at site %i?" site_number)
                            correct_energy energy
                -- @-node:gcross.20091115105949.1748:createBandwidthIncreaseTest
                -- @+node:gcross.20100505180122.1725:runTestsForOperator
                runTestsForOperator :: OperatorDimension n => SingleSiteOperator n -> Test.Framework.Test
                runTestsForOperator operator =
                    let physical_dimension = physicalDimensionOfSingleSiteOperator operator
                    in testGroup (printf "physical dimension = %i" physical_dimension) $
                       map (\(number_of_sites,old_bandwidth_dimension,new_bandwidth_dimension) ->
                            testCase (printf "%i sites, bandwidth => %i -> %i" number_of_sites old_bandwidth_dimension new_bandwidth_dimension) $ 
                                createBandwidthIncreaseTest operator number_of_sites old_bandwidth_dimension new_bandwidth_dimension
                       ) $
                       concat
                        [[ ( 2,old_b,new_b) | (old_b,new_b) <- [(1,2)]]
                        ,[ ( 3,old_b,new_b) | (old_b,new_b) <- [(1,2)]]
                        ,[ ( 4,old_b,new_b) | (old_b,new_b) <- [(1,2),(1,3),(1,4),(2,4)]]
                        ,[ ( 5,old_b,new_b) | (old_b,new_b) <- [(1,2),(1,4),(2,4)]]
                        ,[ (10,old_b,new_b) | (old_b,new_b) <- [(1,2),(2,4),(4,8),(8,16)]]
                        ]
                -- @-node:gcross.20100505180122.1725:runTestsForOperator
                -- @-others
            in  [runTestsForOperator (
                    SingleSiteOperator $
                    (0 :. (0 :+ 1) :. ()) :.
                    ((0 :+ (-1)) :. 0 :. ()) :.
                    () :: SingleSiteOperator N2
                 )
                ,runTestsForOperator (
                    SingleSiteOperator $
                    (0 :. 1 :. 3 :. ()) :.
                    (1 :. 2 :. 1 :. ()) :.
                    (3 :. 1 :. 0 :. ()) :.
                    () :: SingleSiteOperator N3
                 )
                ]
        -- @nonl
        -- @-node:gcross.20091115105949.1747:Chain energy invariant under bandwidth increase
        -- @-others
        ]
    -- @-node:gcross.20091113142219.1701:VMPS.EnergyMinimizationChain
    -- @+node:gcross.20091123113033.1636:VMPS.States
    ,testGroup "VMPS.States"
        -- @    @+others
        -- @+node:gcross.20091123113033.1637:expectationOf
        [testCase "expectationOf" $ do
            let number_of_sites = 20
                bandwidth_dimension = 8
                operator_site_tensors = makeExternalFieldOperatorSiteTensors pZ number_of_sites
            chain <- generateRandomizedChain bandwidth_dimension operator_site_tensors
            let chain_expectation = (computeEnergy chain :+ 0)
                state_expectation = expectationOf operator_site_tensors . getCanonicalStateRepresentation $ chain
            assertAlmostEqual "Was the expectation consistent with the chain energy?"
                chain_expectation
                state_expectation
        -- @-node:gcross.20091123113033.1637:expectationOf
        -- @-others
        ]
    -- @-node:gcross.20091123113033.1636:VMPS.States
    -- @+node:gcross.20091211120042.1694:VMPS.Operators
    ,testGroup "VMPS.Operators"
        -- @    @+others
        -- @+node:gcross.20091211120042.1695:bit-flip test
        [testCase "bit-flip test" $ do
            let number_of_sites = 20
                bandwidth_dimension = 8
                operator_site_tensors = makeExternalFieldOperatorSiteTensors pZ number_of_sites
            chain <- generateRandomizedChain bandwidth_dimension operator_site_tensors
            let chain_expectation = (computeEnergy chain :+ 0)
                new_state_expectation =
                    expectationOf operator_site_tensors
                    .
                    applySingleSiteOperators (repeat pX)
                    .
                    getCanonicalStateRepresentation
                    $
                    chain
            assertAlmostEqual "Did flipping the bits negate the energy?"
                chain_expectation
                (-new_state_expectation)
        -- @-node:gcross.20091211120042.1695:bit-flip test
        -- @+node:gcross.20091211120042.1698:phase-flip test
        ,testCase "phase-flip test" $ do
            let number_of_sites = 20
                bandwidth_dimension = 8
                operator_site_tensors = makeExternalFieldOperatorSiteTensors pZ number_of_sites
            chain <- generateRandomizedChain bandwidth_dimension operator_site_tensors
            let chain_expectation = (computeEnergy chain :+ 0)
                new_state_expectation =
                    expectationOf operator_site_tensors
                    .
                    applySingleSiteOperators (repeat pZ)
                    .
                    getCanonicalStateRepresentation
                    $
                    chain
            assertAlmostEqual "Did flipping the phase keep the energy constant?"
                chain_expectation
                new_state_expectation
        -- @-node:gcross.20091211120042.1698:phase-flip test
        -- @+node:gcross.20091211120042.1700:both-flip test
        ,testCase "both-flip test" $ do
            let number_of_sites = 20
                bandwidth_dimension = 8
                operator_site_tensors = makeExternalFieldOperatorSiteTensors pZ number_of_sites
            chain <- generateRandomizedChain bandwidth_dimension operator_site_tensors
            let chain_expectation = (computeEnergy chain :+ 0)
                new_state_expectation =
                    expectationOf operator_site_tensors
                    .
                    applySingleSiteOperators (repeat pY)
                    .
                    getCanonicalStateRepresentation
                    $
                    chain
            assertAlmostEqual "Did flipping the phase and the bit negate the energy?"
                chain_expectation
                (-new_state_expectation)
        -- @-node:gcross.20091211120042.1700:both-flip test
        -- @-others
        ]
    -- @-node:gcross.20091211120042.1694:VMPS.Operators
    -- @+node:gcross.20091118213523.1816:VMPS.Algorithms
    ,testGroup "VMPS.Algorithm"
        -- @    @+others
        -- @+node:gcross.20091118213523.1827:performOptimizationSweep
        [testGroup "performOptimizationSweep"
            -- @    @+others
            -- @+node:gcross.20091114174920.1741:external field
            [testGroup "external field" $
                let runExternalFieldTests :: OperatorDimension n => SingleSiteOperator n -> Test.Framework.Test
                    runExternalFieldTests field_operator =
                        let physical_dimension = physicalDimensionOfSingleSiteOperator field_operator
                            createExternalFieldTest number_of_sites bandwidth_dimension =
                                generateRandomizedChain bandwidth_dimension (makeExternalFieldOperatorSiteTensors field_operator number_of_sites)
                                >>=
                                return . chainEnergy . performOptimizationSweep_
                                >>=
                                assertAlmostEqual "Is the optimal energy correct?" (-(toEnum number_of_sites))
                        in testGroup (printf "physical dimension = %i" physical_dimension) $
                           map (\(number_of_sites,bandwidth_dimension) ->
                                testCase (printf "%i sites, bandwidth = %i" number_of_sites bandwidth_dimension) $ 
                                    createExternalFieldTest number_of_sites bandwidth_dimension
                           ) $
                           concat
                            [[ ( 2,b) | b <- [2]]
                            ,[ ( 3,b) | b <- [2]]
                            ,[ ( 4,b) | b <- [2,4]]
                            ,[ ( 5,b) | b <- [2,4]]
                            ,[ (10,b) | b <- [2,4,8,16]]
                            ]
                in [runExternalFieldTests $
                      (SingleSiteOperator $
                        (1 :.   0 :. ()) :.
                        (0 :. (-1):. ()) :.
                      () :: SingleSiteOperator N2 )
                   ,runExternalFieldTests $
                      (SingleSiteOperator $
                        (1 :. 0 :.   0 :. ()) :.
                        (0 :. 1 :.   0 :. ()) :.
                        (0 :. 0 :. (-1):. ()) :.
                      () :: SingleSiteOperator N3 )
                   ,runExternalFieldTests $
                      (SingleSiteOperator $
                        (1 :. 0 :. 0 :.   0 :. ()) :.
                        (0 :. 1 :. 0 :.   0 :. ()) :.
                        (0 :. 0 :. 1 :.   0 :. ()) :.
                        (0 :. 0 :. 0 :. (-1):. ()) :.
                      () :: SingleSiteOperator N4 )
                   ]
            -- @-node:gcross.20091114174920.1741:external field
            -- @-others
            ]
        -- @-node:gcross.20091118213523.1827:performOptimizationSweep
        -- @+node:gcross.20091119150241.1839:performRepeatedSweepsUntilConvergence
        ,testGroup "performRepeatedSweepsUntilConvergence"
            -- @    @+others
            -- @+node:gcross.20091118213523.1830:transverse ising model
            [testGroup "transverse ising model" $
                let 
                -- @nonl
                -- @<< createTransverseIsingModelTest >>
                -- @+node:gcross.20091118213523.1831:<< createTransverseIsingModelTest >>
                createTransverseIsingModelTest number_of_sites
                                               bandwidth_dimension
                                               perturbation_strength
                                               correct_ground_state_energy =
                    generateRandomizedChain bandwidth_dimension
                        (makeTransverseIsingModelOperatorSiteTensors perturbation_strength number_of_sites)
                    >>=
                    return . chainEnergy . performRepeatedSweepsUntilConvergence_
                    >>=
                    assertAlmostEqual "Is the optimal energy correct?" correct_ground_state_energy
                -- @-node:gcross.20091118213523.1831:<< createTransverseIsingModelTest >>
                -- @nl
                in map (\(number_of_sites,bandwidth_dimension,perturbation_strength,correct_ground_state_energy) ->
                        testCase (printf "%i sites, bandwidth = %i, perturbation strength = %f" number_of_sites bandwidth_dimension (realPart perturbation_strength)) $ 
                            createTransverseIsingModelTest number_of_sites
                                                           bandwidth_dimension
                                                           perturbation_strength
                                                           correct_ground_state_energy
                ) $ [( 2,2,0.1,-2.00249843945)
                    ,( 2,2,1.0,-2.2360679775)
                    ,( 4,2,0.1,-4.00750155855)
                    ,( 4,4,1.0,-4.75877048314)
                    ,(10,2,0.1,-10.0225109571)
                    ,(10,6,1.0,-12.3814899997)
                    ]
            -- @-node:gcross.20091118213523.1830:transverse ising model
            -- @-others
            ]
        -- @-node:gcross.20091119150241.1839:performRepeatedSweepsUntilConvergence
        -- @+node:gcross.20091119150241.1843:increaseBandwidthAndSweepUntilConvergence
        ,testGroup "increaseBandwidthAndSweepUntilConvergence"
            -- @    @+others
            -- @+node:gcross.20091119150241.1844:transverse ising model
            [testGroup "transverse ising model" $
                let 
                -- @nonl
                -- @<< createTransverseIsingModelTest >>
                -- @+node:gcross.20091119150241.1845:<< createTransverseIsingModelTest >>
                createTransverseIsingModelTest number_of_sites
                                               bandwidth_dimension
                                               perturbation_strength
                                               correct_ground_state_energy =
                    generateRandomizedChain bandwidth_dimension
                        (makeTransverseIsingModelOperatorSiteTensors perturbation_strength number_of_sites)
                    >>=
                    increaseBandwidthAndSweepUntilConvergence_
                    >>=
                    return . chainEnergy
                    >>=
                    assertAlmostEqual "Is the optimal energy correct?" correct_ground_state_energy
                -- @-node:gcross.20091119150241.1845:<< createTransverseIsingModelTest >>
                -- @nl
                in map (\(number_of_sites,bandwidth_dimension,perturbation_strength,correct_ground_state_energy) ->
                        testCase (printf "%i sites, bandwidth = %i, perturbation strength = %f" number_of_sites bandwidth_dimension (realPart perturbation_strength)) $ 
                            createTransverseIsingModelTest number_of_sites
                                                           bandwidth_dimension
                                                           perturbation_strength
                                                           correct_ground_state_energy
                ) $ [(10,2,1.0,-12.3814899997)
                    ]
            -- @-node:gcross.20091119150241.1844:transverse ising model
            -- @-others
            ]
        -- @-node:gcross.20091119150241.1843:increaseBandwidthAndSweepUntilConvergence
        -- @+node:gcross.20091119150241.1861:solveForMultipleLevels
        ,testGroup "solveForMultipleLevels"
            -- @    @+others
            -- @+node:gcross.20091119150241.1874:external field
            [testGroup "external field" $
                let
                    -- @        @+others
                    -- @+node:gcross.20091119150241.1875:createExternalFieldTest
                    createExternalFieldTest :: OperatorDimension n => SingleSiteOperator n -> Int -> [Double] -> Assertion
                    createExternalFieldTest field_operator number_of_sites correct_energy_levels =
                        solveForMultipleLevels_
                            (length correct_energy_levels)
                            (makeExternalFieldOperatorSiteTensors field_operator number_of_sites)
                            []
                        >>=
                        assertAlmostEqual "Is the optimal energy correct?" correct_energy_levels
                            .
                            map (\(x,_,_) -> x)
                    -- @-node:gcross.20091119150241.1875:createExternalFieldTest
                    -- @+node:gcross.20100505180122.1726:runTestsForFieldOperator
                    runTestsForFieldOperator :: OperatorDimension n => SingleSiteOperator n -> Test.Framework.Test
                    runTestsForFieldOperator field_operator =
                        let physical_dimension = physicalDimensionOfSingleSiteOperator field_operator
                        in testGroup (printf "physical dimension = %i" physical_dimension) $
                           map (\(number_of_sites,correct_energy_levels) ->
                            testCase (printf "%i sites" number_of_sites) $ 
                                createExternalFieldTest field_operator number_of_sites correct_energy_levels
                           ) $ [( 4,[ -4,-2,-2,-2])
                               ,( 6,[ -6,-4,-4,-4])
                               ,(10,[-10,-8,-8])
                               ]
                    -- @-node:gcross.20100505180122.1726:runTestsForFieldOperator
                    -- @-others
                in [runTestsForFieldOperator pZ
                   ,runTestsForFieldOperator
                      (SingleSiteOperator $
                        (1 :. 0 :.   0 :. ()) :.
                        (0 :. 1 :.   0 :. ()) :.
                        (0 :. 0 :. (-1):. ()) :.
                        () :: SingleSiteOperator N3 )
                   ]
            -- @-node:gcross.20091119150241.1874:external field
            -- @+node:gcross.20091119150241.1889:transverse ising model
            ,testGroup "transverse ising model" $
                let 
                -- @nonl
                -- @<< createTransverseIsingModelTest >>
                -- @+node:gcross.20091119150241.1890:<< createTransverseIsingModelTest >>
                createMagneticFieldTest number_of_sites coupling_strength correct_energy_levels =
                    solveForMultipleLevels_
                        (length correct_energy_levels)
                        (makeTransverseIsingModelOperatorSiteTensors coupling_strength number_of_sites)
                        []
                    >>=
                    assertAlmostEqual "Is the optimal energy correct?" correct_energy_levels
                        .
                        map (\(x,_,_) -> x)
                -- @-node:gcross.20091119150241.1890:<< createTransverseIsingModelTest >>
                -- @nl
                in map (\(number_of_sites,coupling_strength,correct_energy_levels) ->
                        testCase (printf "%i sites" number_of_sites) $ 
                            createMagneticFieldTest number_of_sites coupling_strength correct_energy_levels
                       ) $ [(10,0.1,[-10.0225109571,-8.2137057257,-8.18819723717])
                           ]
            -- @-node:gcross.20091119150241.1889:transverse ising model
            -- @-others
            ]
        -- @-node:gcross.20091119150241.1861:solveForMultipleLevels
        -- @-others
        ]
    -- @-node:gcross.20091118213523.1816:VMPS.Algorithms
    -- @-others
    -- @-node:gcross.20091111171052.1640:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091111171052.1608:@thin test.hs
-- @-leo
