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
withPinnedArray :: (Ix i, Storable e, IArray UArray e) => UArray i e -> (Ptr e -> IO a) -> IO a
withPinnedArray arr thunk = do
    newListArray (bounds arr) (elems arr)
    >>=
    (`withStorableArray` thunk)
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

withPinnedBoundaryTensor :: BoundaryTensor -> (Ptr Double -> IO a) -> IO a
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

withPinnedStateSiteTensor :: StateSiteTensor -> (Ptr Double -> IO a) -> IO a
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

withPinnedOperatorSiteTensor :: OperatorSiteTensor -> (Int -> Ptr Int32 -> Ptr Double -> IO a) -> IO a
withPinnedOperatorSiteTensor operator_site_tensor thunk = 
    (withStorableArray . operatorIndices) operator_site_tensor $ \p_indices ->
    (withStorableArray . operatorMatrices) operator_site_tensor $ \p_matrices ->
    thunk (operatorNumberOfMatrices operator_site_tensor) p_indices p_matrices
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
foreign import ccall unsafe "compute_expectation" compute_expectation :: 
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr Double -> -- left environment
    Ptr Double -> -- state site tensor
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr Double -> -- sparse operator matrices
    Ptr Double -> -- right environment
    IO Double -- expectation

computeExpectation :: LeftBoundaryTensor -> StateSiteTensor -> OperatorSiteTensor -> RightBoundaryTensor -> Double
computeExpectation left_boundary_tensor state_site_tensor operator_site_tensor right_boundary_tensor =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedLeftBoundaryTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedStateSiteTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        withPinnedRightBoundaryTensor right_boundary_tensor $
            compute_expectation
                bl
                br
                cl
                cr
                d
                p_left_boundary
                p_state_site_tensor
                number_of_matrices p_operator_indices p_operator_matrices
-- @-node:gcross.20091111171052.1589:computeExpectation
-- @+node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
foreign import ccall unsafe "optimize" optimize ::
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr Double -> -- left environment
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr Double -> -- sparse operator matrices
    Ptr Double -> -- right environment
    CString -> -- which eigenvectors to select
    Double -> -- tolerance
    Ptr Int -> -- cap on the number of iterations / number of iterations that were used
    Ptr Double -> -- initial guess for the state site tensor
    Ptr Double -> -- where the result will be written
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
        withPinnedLeftBoundaryTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedStateSiteTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        withPinnedRightBoundaryTensor right_boundary_tensor $ \p_right_boundary ->
        withStrategyAsCString strategy $ \p_strategy ->
        with maximum_number_of_iterations  $ \p_number_of_iterations ->
        do
            state_site_tensor_storable_array <- newArray_ (1,2*br*bl*d)
            info <- withStorableArray state_site_tensor_storable_array $
                optimize
                    bl
                    br
                    cl
                    cr
                    d
                    p_left_boundary
                    number_of_matrices p_operator_indices p_operator_matrices
                    p_right_boundary
                    p_strategy
                    tolerance
                    p_number_of_iterations
                    p_state_site_tensor
            if info < 0
                then return (Left info)
                else do
                    number_of_iterations <- peek p_number_of_iterations
                    state_site_tensor <- fmap (StateSiteTensor bl br d) (unpinComplexArray state_site_tensor_storable_array)
                    return $ Right (number_of_iterations, state_site_tensor)
-- @-node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
-- @+node:gcross.20091112145455.1625:contractSOSLeft
foreign import ccall unsafe "contract_sos_left" contract_sos_left :: 
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr Double -> -- left environment
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr Double -> -- sparse operator matrices
    Ptr Double -> -- state site tensor
    Ptr Double -> -- new left environment
    IO ()

contractSOSLeft :: LeftBoundaryTensor -> StateSiteTensor -> OperatorSiteTensor -> LeftBoundaryTensor
contractSOSLeft left_boundary_tensor state_site_tensor operator_site_tensor =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = stateRightBandwidth state_site_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operatorRightBandwidth operator_site_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedLeftBoundaryTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedStateSiteTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        do
            state_site_tensor_storable_array <- newArray_ (1,2*br*br*cr)
            withStorableArray state_site_tensor_storable_array $
                contract_sos_left
                    bl
                    br
                    cl
                    cr
                    d
                    p_left_boundary
                    number_of_matrices p_operator_indices p_operator_matrices
                    p_state_site_tensor
            fmap (LeftBoundaryTensor . BoundaryTensor br cr) (unpinComplexArray state_site_tensor_storable_array)
-- @-node:gcross.20091112145455.1625:contractSOSLeft
-- @+node:gcross.20091112145455.1635:contractSOSRight
foreign import ccall unsafe "contract_sos_right" contract_sos_right :: 
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr Double -> -- left environment
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr Double -> -- sparse operator matrices
    Ptr Double -> -- state site tensor
    Ptr Double -> -- new left environment
    IO ()

contractSOSRight :: RightBoundaryTensor -> StateSiteTensor -> OperatorSiteTensor -> RightBoundaryTensor
contractSOSRight right_boundary_tensor state_site_tensor operator_site_tensor =
    let bl = stateLeftBandwidth state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = operatorLeftBandwidth operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedRightBoundaryTensor right_boundary_tensor $ \p_right_boundary ->
        withPinnedStateSiteTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        do
            state_site_tensor_storable_array <- newArray_ (1,2*bl*bl*cl)
            withStorableArray state_site_tensor_storable_array $
                contract_sos_right
                    bl
                    br
                    cl
                    cr
                    d
                    p_right_boundary
                    number_of_matrices p_operator_indices p_operator_matrices
                    p_state_site_tensor
            fmap (RightBoundaryTensor . BoundaryTensor bl cl) (unpinComplexArray state_site_tensor_storable_array)
-- @-node:gcross.20091112145455.1635:contractSOSRight
-- @-node:gcross.20091111171052.1588:Foreign imports
-- @-others
-- @-node:gcross.20091110205054.1969:@thin VMPS.hs
-- @-leo
