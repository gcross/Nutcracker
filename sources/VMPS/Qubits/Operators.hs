-- @+leo-ver=4-thin
-- @+node:gcross.20091201134050.1634:@thin Operators.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100504143114.1704:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- @-node:gcross.20100504143114.1704:<< Language extensions >>
-- @nl

module VMPS.Qubits.Operators where

-- @<< Import needed modules >>
-- @+node:gcross.20100504143114.1698:<< Import needed modules >>
import Control.Exception

import Data.Array.Storable
import Data.Complex
import Data.Int
import Data.Vec ((:.)(..),Vec4)
import qualified Data.Vec as Vec

import Foreign.Ptr

import System.IO.Unsafe

import VMPS.States
import VMPS.Tensors
import VMPS.Tensors.Implementation (OperatorSiteTensor(..))
import VMPS.Miscellaneous
-- @-node:gcross.20100504143114.1698:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100504143114.1692:Types
-- @+node:gcross.20100504143114.1697:SingleQubitOperator
newtype SingleQubitOperator = SQO (Vec4 (Complex Double)) deriving (Num,Eq,Show)
-- @-node:gcross.20100504143114.1697:SingleQubitOperator
-- @+node:gcross.20100504143114.1699:OperatorSpecification
type OperatorSpecification = [((Int32,Int32),SingleQubitOperator)]
-- @nonl
-- @-node:gcross.20100504143114.1699:OperatorSpecification
-- @-node:gcross.20100504143114.1692:Types
-- @+node:gcross.20100504143114.1693:Instances
-- @+node:gcross.20100504143114.1694:Pinnable SingleQubitOperator
instance Pinnable SingleQubitOperator where
    withPinnedTensor (SQO (    p11 :. p12
                            :. p21 :. p22
                            :. () )) thunk = do
        operator <- newArray ((1,1),(2,2)) 0
        writeArray operator (1,1) p11
        writeArray operator (1,2) p12
        writeArray operator (2,1) p21
        writeArray operator (2,2) p22
        withStorableArray operator thunk
-- @-node:gcross.20100504143114.1694:Pinnable SingleQubitOperator
-- @-node:gcross.20100504143114.1693:Instances
-- @+node:gcross.20100504143114.1695:Functions
-- @+node:gcross.20100504143114.1696:(*:)
infix 5 *:
(*:) :: Complex Double -> SingleQubitOperator -> SingleQubitOperator
(*:) c (SQO v) = SQO (Vec.map (c*) v)
-- @nonl
-- @-node:gcross.20100504143114.1696:(*:)
-- @+node:gcross.20100504143114.1703:applySingleSiteOperator
foreign import ccall unsafe "apply_single_site_operator" apply_single_site_operator :: 
    Int -> -- rightmost bandwidth dimension
    Int -> -- leftmost bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- input: state site tensor
    Ptr (Complex Double) -> -- input: operator
    Ptr (Complex Double) -> -- output: new state site tensor
    IO ()

applySingleSiteOperator ::
    (Pinnable a, Creatable a (Int,Int,Int), StateSiteTensorClass a) =>
    SingleQubitOperator ->
    a ->
    a
applySingleSiteOperator operator state_site_tensor =
    let br = rightBandwidthOfState state_site_tensor
        bl = leftBandwidthOfState state_site_tensor
        d = physicalDimensionOfState state_site_tensor
    in snd . unsafePerformIO $
            withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
            withPinnedTensor operator $ \p_operator ->
            withNewPinnedTensor (d,bl,br) $ \p_new_state_site_tensor ->
                apply_single_site_operator
                    br
                    bl
                    d
                    p_state_site_tensor
                    p_operator
                    p_new_state_site_tensor
-- @-node:gcross.20100504143114.1703:applySingleSiteOperator
-- @+node:gcross.20100504143114.1701:applySingleSiteOperators
applySingleSiteOperators :: [SingleQubitOperator] -> CanonicalStateRepresentation -> CanonicalStateRepresentation
applySingleSiteOperators operators old_state = assert (length operators >= canonicalStateNumberOfSites old_state) $
    old_state
        {   canonicalStateFirstSiteTensor =
                applySingleSiteOperator
                    (head operators)
                    (canonicalStateFirstSiteTensor old_state)
        ,   canonicalStateRestSiteTensors =
                zipWith applySingleSiteOperator
                    (tail operators)
                    (canonicalStateRestSiteTensors old_state)
        }
-- @-node:gcross.20100504143114.1701:applySingleSiteOperators
-- @+node:gcross.20100504143114.1691:makeOperatorSiteTensorFromSpecification
makeOperatorSiteTensorFromSpecification ::
    Int ->
    Int ->
    OperatorSpecification ->
    OperatorSiteTensor
makeOperatorSiteTensorFromSpecification left_bandwidth right_bandwidth elements =
    let number_of_elements = length elements
    in unsafePerformIO $ do
        operator_indices <- newArray ((1,1),(number_of_elements,2)) 0
        operator_matrices <- newArray ((1,1,1),(number_of_elements,2,2)) 0
        let go :: Int -> [((Int32,Int32),SingleQubitOperator)] -> IO ()
            go _ [] = return ()
            go index (((left_index,right_index),pauli):rest) =
                let SQO (    p11 :. p12
                          :. p21 :. p22
                          :. () ) = pauli
                in do
                    writeArray operator_indices (index,1) left_index
                    writeArray operator_indices (index,2) right_index
                    writeArray operator_matrices (index,1,1) p11
                    writeArray operator_matrices (index,1,2) p12
                    writeArray operator_matrices (index,2,1) p21
                    writeArray operator_matrices (index,2,2) p22
                    go (index+1) rest
        go 1 elements
        return OperatorSiteTensor
            {   operatorLeftBandwidth = left_bandwidth
            ,   operatorRightBandwidth = right_bandwidth
            ,   operatorPhysicalDimension = 2
            ,   operatorNumberOfMatrices = number_of_elements
            ,   operatorIndices = operator_indices
            ,   operatorMatrices = operator_matrices
            }
-- @-node:gcross.20100504143114.1691:makeOperatorSiteTensorFromSpecification
-- @-node:gcross.20100504143114.1695:Functions
-- @-others
-- @-node:gcross.20091201134050.1634:@thin Operators.hs
-- @-leo
