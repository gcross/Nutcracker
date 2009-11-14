-- @+leo-ver=4-thin
-- @+node:gcross.20091113125544.1653:@thin Wrappers.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113125544.1655:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091113125544.1655:<< Language extensions >>
-- @nl

module VMPS.Wrappers where

-- @<< Import needed modules >>
-- @+node:gcross.20091113125544.1659:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.Array.Unboxed
import Data.Complex
import Data.Int

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Text.Printf

import System.IO.Unsafe

import VMPS.Tensors
-- @-node:gcross.20091113125544.1659:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091113125544.1660:Types
-- @+node:gcross.20091111171052.1657:SelectionStrategy
data OptimizerSelectionStrategy = LM | SR

instance Show OptimizerSelectionStrategy where
    show LM = "LM"
    show SR = "SR"

withStrategyAsCString :: OptimizerSelectionStrategy -> (CString -> IO a) -> IO a
withStrategyAsCString = withCString . show
-- @-node:gcross.20091111171052.1657:SelectionStrategy
-- @-node:gcross.20091113125544.1660:Types
-- @+node:gcross.20091113142219.1680:Instances
-- @+node:gcross.20091113142219.1681:Storable (Complex a)
complexPtrToRealAndImagPtrs :: Ptr (Complex Double) -> (Ptr Double, Ptr Double)
complexPtrToRealAndImagPtrs p_complex =
    let p_real_part = castPtr p_complex
        p_imag_part = plusPtr p_real_part . sizeOf $ (undefined :: Double)
    in (p_real_part, p_imag_part)

instance Storable (Complex Double) where
    sizeOf _ = 2 * sizeOf (undefined :: Double)
    alignment _ = 2 * alignment (undefined :: Double)
    peek = uncurry (liftM2 (:+)) . (peek *** peek) . complexPtrToRealAndImagPtrs
    poke p_complex (real_part :+ imag_part) =
        let (p_real_part, p_imag_part) = complexPtrToRealAndImagPtrs p_complex
        in poke p_real_part real_part >> poke p_imag_part imag_part
-- @-node:gcross.20091113142219.1681:Storable (Complex a)
-- @-node:gcross.20091113142219.1680:Instances
-- @+node:gcross.20091113125544.1661:Wrapper functions
-- @+node:gcross.20091113125544.1656:Contractors
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
    Ptr (Complex Double) -> -- expectation
    IO ()

computeExpectation :: LeftBoundaryTensor -> UnnormalizedStateSiteTensor -> OperatorSiteTensor -> RightBoundaryTensor -> Complex Double
computeExpectation left_boundary_tensor state_site_tensor operator_site_tensor right_boundary_tensor =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
        alloca $ \p_expectation ->
            compute_expectation
                bl
                br
                cl
                cr
                d
                p_left_boundary
                p_state_site_tensor
                number_of_matrices p_operator_indices p_operator_matrices
                p_right_boundary
                p_expectation
            >>
            peek p_expectation
-- @-node:gcross.20091111171052.1589:computeExpectation
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

contractSOSLeft :: LeftBoundaryTensor -> LeftAbsorptionNormalizedStateSiteTensor -> OperatorSiteTensor -> LeftBoundaryTensor
contractSOSLeft left_boundary_tensor state_site_tensor operator_site_tensor =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = rightBandwidthOfState state_site_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operatorRightBandwidth operator_site_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
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

contractSOSRight :: RightBoundaryTensor -> RightAbsorptionNormalizedStateSiteTensor -> OperatorSiteTensor -> RightBoundaryTensor
contractSOSRight right_boundary_tensor state_site_tensor operator_site_tensor =
    let bl = leftBandwidthOfState state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = operatorLeftBandwidth operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in unsafePerformIO $
        withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
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
-- @-node:gcross.20091113125544.1656:Contractors
-- @+node:gcross.20091113125544.1664:Normalizers
-- @+node:gcross.20091113125544.1667:makeNormalizedForAbsorbingLeft
foreign import ccall unsafe "norm_denorm_going_right" norm_denorm_going_right :: 
    Int -> -- leftmost bandwidth dimension
    Int -> -- middle bandwidth dimension
    Int -> -- rightmost bandwidth dimension
    Int -> -- left site physical dimension
    Int -> -- right site physical dimension
    Ptr Double -> -- left (normalized) state site tensor
    Ptr Double -> -- right (unnormalized) state site tensor
    Ptr Double -> -- output left (unnormalized) state site tensor
    Ptr Double -> -- output right (normalized) state site tensor
    IO Int

makeNormalizedForAbsorbingLeft ::
    UnnormalizedStateSiteTensor ->
    RightAbsorptionNormalizedStateSiteTensor ->
    (LeftAbsorptionNormalizedStateSiteTensor,UnnormalizedStateSiteTensor)
makeNormalizedForAbsorbingLeft tensor_to_normalize tensor_to_denormalize =
    let bl  = leftBandwidthOfState tensor_to_normalize
        br  = tensor_to_normalize <-?-> tensor_to_denormalize
        brr = rightBandwidthOfState tensor_to_denormalize
        d   = physicalDimensionOfState tensor_to_normalize
        dr  = physicalDimensionOfState tensor_to_denormalize
    in unsafePerformIO $
        withPinnedTensor tensor_to_denormalize $ \p_tensor_to_denormalize ->
        withPinnedTensor tensor_to_normalize $ \p_tensor_to_normalize ->
        do
            normalized_state_site_tensor_storable_array <- newArray_ (1,2*bl*br*d)
            denormalized_state_site_tensor_storable_array <- newArray_ (1,2*br*brr*dr)
            info <-
                withStorableArray normalized_state_site_tensor_storable_array $ \p_normalized_state_site_tensor_storable_array ->
                withStorableArray denormalized_state_site_tensor_storable_array $
                    norm_denorm_going_right
                        bl
                        br
                        brr
                        d
                        dr
                        p_tensor_to_normalize
                        p_tensor_to_denormalize
                        p_normalized_state_site_tensor_storable_array
            when (info < 0) $ fail "unable to normalize tensor!"
            normalized_state_site_tensor <- fmap 
                (LeftAbsorptionNormalizedStateSiteTensor . StateSiteTensor bl br d)
                (unpinComplexArray normalized_state_site_tensor_storable_array)
            denormalized_state_site_tensor <- fmap 
                (UnnormalizedStateSiteTensor . StateSiteTensor br brr dr)
                (unpinComplexArray denormalized_state_site_tensor_storable_array)
            return (normalized_state_site_tensor,denormalized_state_site_tensor)
-- @-node:gcross.20091113125544.1667:makeNormalizedForAbsorbingLeft
-- @+node:gcross.20091113125544.1665:makeNormalizedForAbsorbingRight
foreign import ccall unsafe "norm_denorm_going_left" norm_denorm_going_left :: 
    Int -> -- leftmost bandwidth dimension
    Int -> -- middle bandwidth dimension
    Int -> -- rightmost bandwidth dimension
    Int -> -- left site physical dimension
    Int -> -- right site physical dimension
    Ptr Double -> -- left (normalized) state site tensor
    Ptr Double -> -- right (unnormalized) state site tensor
    Ptr Double -> -- output left (unnormalized) state site tensor
    Ptr Double -> -- output right (normalized) state site tensor
    IO Int

makeNormalizedForAbsorbingRight ::
    LeftAbsorptionNormalizedStateSiteTensor ->
    UnnormalizedStateSiteTensor ->
    (UnnormalizedStateSiteTensor,RightAbsorptionNormalizedStateSiteTensor)
makeNormalizedForAbsorbingRight tensor_to_denormalize tensor_to_normalize =
    let bll = leftBandwidthOfState tensor_to_denormalize
        bl  = tensor_to_denormalize <-?-> tensor_to_normalize
        br  = rightBandwidthOfState tensor_to_normalize
        dl  = physicalDimensionOfState tensor_to_denormalize
        d   = physicalDimensionOfState tensor_to_normalize
    in unsafePerformIO $
        withPinnedTensor tensor_to_denormalize $ \p_tensor_to_denormalize ->
        withPinnedTensor tensor_to_normalize $ \p_tensor_to_normalize ->
        do
            denormalized_state_site_tensor_storable_array <- newArray_ (1,2*bll*bl*dl)
            normalized_state_site_tensor_storable_array <- newArray_ (1,2*bl*br*d)
            info <-
                withStorableArray denormalized_state_site_tensor_storable_array $ \p_denormalized_state_site_tensor_storable_array ->
                withStorableArray normalized_state_site_tensor_storable_array $
                    norm_denorm_going_left
                        bll
                        bl
                        br
                        dl
                        d
                        p_tensor_to_denormalize
                        p_tensor_to_normalize
                        p_denormalized_state_site_tensor_storable_array
            when (info < 0) $ fail "unable to normalize tensor!"
            denormalized_state_site_tensor <- fmap 
                (UnnormalizedStateSiteTensor . StateSiteTensor bll bl dl)
                (unpinComplexArray denormalized_state_site_tensor_storable_array)
            normalized_state_site_tensor <- fmap 
                (RightAbsorptionNormalizedStateSiteTensor . StateSiteTensor bl br d)
                (unpinComplexArray normalized_state_site_tensor_storable_array)
            return (denormalized_state_site_tensor,normalized_state_site_tensor)
-- @-node:gcross.20091113125544.1665:makeNormalizedForAbsorbingRight
-- @-node:gcross.20091113125544.1664:Normalizers
-- @+node:gcross.20091112145455.1656:Randomizers
class RandomizableStateSiteTensor a where
    generateRandomizedStateSiteTensor :: Int -> Int -> Int -> IO a

applyRandomizerAndReturnStateSiteTensor :: (StateSiteTensor -> a) -> (Int -> Int -> Int -> Ptr Double -> IO ()) -> Int -> Int -> Int -> IO a
applyRandomizerAndReturnStateSiteTensor wrapper randomizer physical_dimension left_bandwidth_dimension right_bandwidth_dimension =
        let d  = physical_dimension
            bl = left_bandwidth_dimension
            br = right_bandwidth_dimension
        in do
            state_site_tensor_storable_array <- newArray_ (1,2*br*bl*d)
            withStorableArray state_site_tensor_storable_array $
                randomizer
                    br
                    bl
                    d
            fmap (wrapper . StateSiteTensor bl br d) (unpinComplexArray state_site_tensor_storable_array)
-- @+node:gcross.20091112145455.1674:unnormalized
foreign import ccall unsafe "randomize_state_site_tensor" randomize_state_site_tensor :: 
    Int -> -- state right bandwidth dimension
    Int -> -- state left bandwidth dimension
    Int -> -- physical dimension
    Ptr Double -> -- state site tensor
    IO ()

instance RandomizableStateSiteTensor UnnormalizedStateSiteTensor where
    generateRandomizedStateSiteTensor = applyRandomizerAndReturnStateSiteTensor UnnormalizedStateSiteTensor randomize_state_site_tensor
-- @-node:gcross.20091112145455.1674:unnormalized
-- @+node:gcross.20091112145455.1675:normalized
foreign import ccall unsafe "rand_norm_state_site_tensor" rand_norm_state_site_tensor :: 
    Int -> -- state right bandwidth dimension
    Int -> -- state left bandwidth dimension
    Int -> -- physical dimension
    Ptr Double -> -- state site tensor
    IO ()

instance RandomizableStateSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    generateRandomizedStateSiteTensor = applyRandomizerAndReturnStateSiteTensor RightAbsorptionNormalizedStateSiteTensor rand_norm_state_site_tensor
-- @-node:gcross.20091112145455.1675:normalized
-- @-node:gcross.20091112145455.1656:Randomizers
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
    Ptr (Complex Double) -> -- the minimal eigenvalue
    IO Int32

computeOptimalSiteStateTensor ::
    LeftBoundaryTensor ->
    UnnormalizedStateSiteTensor ->
    OperatorSiteTensor ->
    RightBoundaryTensor ->
    OptimizerSelectionStrategy ->
    Double ->
    Int ->
    (Int, Complex Double, UnnormalizedStateSiteTensor)
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
        withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
        withStrategyAsCString strategy $ \p_strategy ->
        with maximum_number_of_iterations  $ \p_number_of_iterations ->
        alloca $ \p_eigenvalue ->
        do
            state_site_tensor_storable_array <- newArray_ (1,2*br*bl*d)
            info <- withStorableArray state_site_tensor_storable_array $ \p_result ->
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
                    p_result
                    p_eigenvalue
            when (info /= 0) $ error $ "Failed to converge! info = " ++ show info
            number_of_iterations <- peek p_number_of_iterations
            eigenvalue <- peek p_eigenvalue
            state_site_tensor <- fmap (UnnormalizedStateSiteTensor . StateSiteTensor bl br d) (unpinComplexArray state_site_tensor_storable_array)
            return $ (number_of_iterations, eigenvalue, state_site_tensor)
-- @-node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
-- @-node:gcross.20091113125544.1661:Wrapper functions
-- @-others
-- @-node:gcross.20091113125544.1653:@thin Wrappers.hs
-- @-leo
