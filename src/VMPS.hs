-- @+leo-ver=4-thin
-- @+node:gcross.20091110205054.1969:@thin VMPS.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091111171052.1600:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091111171052.1600:<< Language extensions >>
-- @nl

module VMPS where

-- @<< Import needed modules >>
-- @+node:gcross.20091111171052.1599:<< Import needed modules >>
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.Array.Unboxed
import Data.Function
import Data.Int

import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Text.Printf

import System.IO.Unsafe
-- @-node:gcross.20091111171052.1599:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091111171052.1593:Utilitity Functions
-- @+node:gcross.20091111171052.1594:withPinnedArray
withPinnedArray :: (Ix i, Storable e, IArray UArray e) => UArray i e -> (Ptr e -> StorableArray i e -> IO a) -> IO a
withPinnedArray arr thunk = do
    newListArray (bounds arr) (elems arr)
    >>=
    \arr -> withStorableArray arr (\ptr -> thunk ptr arr)
-- @-node:gcross.20091111171052.1594:withPinnedArray
-- @+node:gcross.20091111171052.1658:unpinArray
unpinArray :: (Ix i, Storable e, IArray UArray e) => StorableArray i e -> IO (UArray i e)
unpinArray arr = liftM2 listArray (getBounds arr) (getElems arr)
-- @-node:gcross.20091111171052.1658:unpinArray
-- @-node:gcross.20091111171052.1593:Utilitity Functions
-- @+node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091111171052.1596:ComplexArray
newtype ComplexArray = ComplexArray { unwrapComplexArray :: UArray Int Double } deriving (Show)

withPinnedComplexArray = withPinnedArray . unwrapComplexArray

unpinComplexArray = unpinArray >=> return . ComplexArray

unboxedArrayFromList list = listArray (1,length list) list

complexArrayFromList = ComplexArray . unboxedArrayFromList

trivial_complex_array = complexArrayFromList [1.0,0.0]
-- @-node:gcross.20091111171052.1596:ComplexArray
-- @+node:gcross.20091111171052.1595:Left/Right Boundaries
data BoundaryTensor = BoundaryTensor
    {   boundaryStateBandwidth :: !Int
    ,   boundaryOperatorBandwidth :: !Int
    ,   boundaryData :: !ComplexArray
    }

withPinnedBoundaryTensor :: BoundaryTensor -> (Ptr Double -> StorableArray Int Double -> IO a) -> IO a
withPinnedBoundaryTensor = withPinnedComplexArray . boundaryData

newtype LeftBoundaryTensor = LeftBoundaryTensor { unwrapLeftBoundaryTensor :: BoundaryTensor }
withPinnedLeftBoundaryTensor = withPinnedBoundaryTensor . unwrapLeftBoundaryTensor

newtype RightBoundaryTensor = RightBoundaryTensor { unwrapRightBoundaryTensor :: BoundaryTensor }
withPinnedRightBoundaryTensor = withPinnedBoundaryTensor . unwrapRightBoundaryTensor

trivial_boundary = BoundaryTensor 1 1 trivial_complex_array
trivial_left_boundary = LeftBoundaryTensor trivial_boundary
trivial_right_boundary = RightBoundaryTensor trivial_boundary
-- @-node:gcross.20091111171052.1595:Left/Right Boundaries
-- @+node:gcross.20091111171052.1597:State Site Tensor
data StateSiteTensor = StateSiteTensor
    {   stateLeftBandwidth :: !Int
    ,   stateRightBandwidth :: !Int
    ,   statePhysicalDimension :: !Int
    ,   stateData :: !ComplexArray
    }

withPinnedStateSiteTensor :: StateSiteTensor -> (Ptr Double -> StorableArray Int Double -> IO a) -> IO a
withPinnedStateSiteTensor = withPinnedComplexArray . stateData
-- @-node:gcross.20091111171052.1597:State Site Tensor
-- @+node:gcross.20091111171052.1598:Operator Site Tensor
data OperatorSiteTensor = OperatorSiteTensor
    {   operatorLeftBandwidth :: !Int
    ,   operatorRightBandwidth :: !Int
    ,   operatorPhysicalDimension :: !Int
    ,   operatorNumberOfMatrices :: !Int
    ,   operatorIndices :: StorableArray Int Int32
    ,   operatorMatrices :: StorableArray Int Double
    }

withPinnedOperatorSiteTensor :: OperatorSiteTensor -> (Ptr Int -> Ptr Int32 -> Ptr Double -> IO a) -> IO a
withPinnedOperatorSiteTensor operator_site_tensor thunk = 
    (with . operatorNumberOfMatrices) operator_site_tensor $ \p_number_of_matrices ->
    (withStorableArray . operatorIndices) operator_site_tensor $ \p_indices ->
    (withStorableArray . operatorMatrices) operator_site_tensor $ \p_matrices ->
    thunk p_number_of_matrices p_indices p_matrices
-- @-node:gcross.20091111171052.1598:Operator Site Tensor
-- @+node:gcross.20091111171052.1657:SelectionStrategy
data SelectionStrategy = LM | SR

instance Show SelectionStrategy where
    show LM = "LM"
    show SR = "SR"

withStrategyAsCString :: SelectionStrategy -> (CString -> IO a) -> IO a
withStrategyAsCString = withCString . show
-- @-node:gcross.20091111171052.1657:SelectionStrategy
-- @-node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091111171052.1601:Classes
-- @+node:gcross.20091111171052.1602:Connected
class Connected a b where
    (<-?->) :: a -> b -> Int

instance Connected LeftBoundaryTensor StateSiteTensor where
    (<-?->) left_boundary_tensor state_site_tensor =
        let d1 = boundaryStateBandwidth . unwrapLeftBoundaryTensor $ left_boundary_tensor
            d2 = stateLeftBandwidth $ state_site_tensor
        in if d1 == d2 then d1 else error $ (printf "Left boundary and state site tensors disagree over the bandwidth dimension! (%i != %i)\n" d1 d2)

instance Connected OperatorSiteTensor StateSiteTensor where
    (<-?->) operator_site_tensor state_site_tensor =
        let d1 = operatorPhysicalDimension $ operator_site_tensor
            d2 = statePhysicalDimension $ state_site_tensor
        in if d1 == d2 then d1 else error $ (printf "Operator and state site tensors disagree over the physical dimension! (%i != %i)\n" d1 d2)

instance Connected StateSiteTensor RightBoundaryTensor where
    (<-?->) state_site_tensor right_boundary_tensor =
        let d1 = stateRightBandwidth $ state_site_tensor
            d2 = boundaryStateBandwidth . unwrapRightBoundaryTensor $ right_boundary_tensor
        in if d1 == d2 then d1 else error $ (printf "Right boundary and state site tensors disagree over the bandwidth dimension! (%i != %i)\n" d1 d2)

instance Connected LeftBoundaryTensor OperatorSiteTensor where
    (<-?->) left_boundary_tensor operator_site_tensor =
        let d1 = boundaryStateBandwidth . unwrapLeftBoundaryTensor $ left_boundary_tensor
            d2 = operatorLeftBandwidth $ operator_site_tensor
        in if d1 == d2 then d1 else error $ (printf "Left boundary and state site tensors disagree over the bandwidth dimension! (%i != %i)\n" d1 d2)

instance Connected OperatorSiteTensor RightBoundaryTensor where
    (<-?->) operator_site_tensor right_boundary_tensor =
        let d1 = operatorRightBandwidth $ operator_site_tensor
            d2 = boundaryStateBandwidth . unwrapRightBoundaryTensor $ right_boundary_tensor
        in if d1 == d2 then d1 else error $ (printf "Right boundary and state site tensors disagree over the bandwidth dimension! (%i != %i)\n" d1 d2)
-- @-node:gcross.20091111171052.1602:Connected
-- @+node:gcross.20091111171052.1664:AlmostEq
class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = abs (x-y) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

x /~ y = not (x ~= y)
-- @-node:gcross.20091111171052.1664:AlmostEq
-- @-node:gcross.20091111171052.1601:Classes
-- @+node:gcross.20091111171052.1588:Foreign imports

-- @+node:gcross.20091111171052.1589:computeExpectation
foreign import ccall unsafe "compute_expectation_" compute_expectation :: 
    Ptr Int -> -- state left bandwidth dimension
    Ptr Int -> -- state right bandwidth dimension
    Ptr Int -> -- operator left bandwidth dimension
    Ptr Int -> -- operator right bandwidth dimension
    Ptr Int -> -- physical dimension
    Ptr Double -> -- left environment
    Ptr Double -> -- state site tensor
    Ptr Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr Double -> -- sparse operator matrices
    Ptr Double -> -- right environment
    Double -- expectation

computeExpectation :: LeftBoundaryTensor -> StateSiteTensor -> OperatorSiteTensor -> RightBoundaryTensor -> Double
computeExpectation left_boundary_tensor state_site_tensor operator_site_tensor right_boundary_tensor =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedLeftBoundaryTensor left_boundary_tensor $ \p_left_boundary _ ->
        withPinnedStateSiteTensor state_site_tensor $ \p_state_site_tensor _ ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \p_number_of_matrices p_operator_indices p_operator_matrices ->
        withPinnedRightBoundaryTensor right_boundary_tensor $ \p_right_boundary _ ->
        with bl $ \p_bl ->
        with br $ \p_br ->
        with cl $ \p_cl ->
        with cr $ \p_cr ->
        with d  $ \p_d  ->
        return $
            compute_expectation
                p_bl p_br
                p_cl p_cr
                p_d
                p_left_boundary
                p_state_site_tensor
                p_number_of_matrices p_operator_indices p_operator_matrices
                p_right_boundary
-- @-node:gcross.20091111171052.1589:computeExpectation
-- @+node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
foreign import ccall unsafe "optimize_" optimize ::
    Ptr Int -> -- state left bandwidth dimension
    Ptr Int -> -- state right bandwidth dimension
    Ptr Int -> -- operator left bandwidth dimension
    Ptr Int -> -- operator right bandwidth dimension
    Ptr Int -> -- physical dimension
    Ptr Double -> -- left environment
    Ptr Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr Double -> -- sparse operator matrices
    Ptr Double -> -- right environment
    CString -> -- which eigenvectors to select
    Ptr Double -> -- tolerance
    Ptr Int -> -- cap on the number of iterations / number of iterations that were used
    Ptr Double -> -- initial guess for the state site tensor / where the result will be written
    IO Int32

computeOptimalSiteStateTensor ::
    LeftBoundaryTensor ->
    StateSiteTensor ->
    OperatorSiteTensor ->
    RightBoundaryTensor ->
    SelectionStrategy ->
    Double ->
    Int ->
    Either Int32 (Int,StateSiteTensor)
computeOptimalSiteStateTensor
    left_boundary_tensor
    state_site_tensor
    operator_site_tensor
    right_boundary_tensor
    strategy
    tolerance
    maximum_number_of_iterations
    =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedLeftBoundaryTensor left_boundary_tensor $ \p_left_boundary _ ->
        withPinnedStateSiteTensor state_site_tensor $ \p_state_site_tensor state_site_tensor_storable_array ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \p_number_of_matrices p_operator_indices p_operator_matrices ->
        withPinnedRightBoundaryTensor right_boundary_tensor $ \p_right_boundary _ ->
        with bl $ \p_bl ->
        with br $ \p_br ->
        with cl $ \p_cl ->
        with cr $ \p_cr ->
        with d  $ \p_d  ->
        withStrategyAsCString strategy $ \p_strategy ->
        with tolerance $ \p_tolerance ->
        with maximum_number_of_iterations  $ \p_number_of_iterations -> do
            optimize
                p_bl p_br
                p_cl p_cr
                p_d
                p_left_boundary
                p_number_of_matrices p_operator_indices p_operator_matrices
                p_right_boundary
                p_strategy
                p_tolerance
                p_number_of_iterations
                p_state_site_tensor
            >>=
            \result ->
                if result < 0
                    then return (Left result)
                    else do
                        number_of_iterations <- peek p_number_of_iterations
                        state_site_tensor_array <- unpinComplexArray state_site_tensor_storable_array
                        return $ Right (number_of_iterations,StateSiteTensor bl br d state_site_tensor_array)
-- @-node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
-- @-node:gcross.20091111171052.1588:Foreign imports
-- @-others
-- @-node:gcross.20091110205054.1969:@thin VMPS.hs
-- @-leo
