-- @+leo-ver=4-thin
-- @+node:gcross.20091113125544.1653:@thin Wrappers.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113125544.1655:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

import VMPS.Miscellaneous
import VMPS.Operators
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
-- @+node:gcross.20091119150241.1880:OptimizerFailure
data OptimizerFailureReason = OptimizerUnableToConverge
    deriving (Show)
-- @-node:gcross.20091119150241.1880:OptimizerFailure
-- @-node:gcross.20091113125544.1660:Types
-- @+node:gcross.20091113125544.1661:Wrapper functions
-- @+node:gcross.20091113125544.1656:Contractors
-- @+node:gcross.20091111171052.1589:computeExpectation
foreign import ccall unsafe "compute_expectation" compute_expectation :: 
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- left environment
    Ptr (Complex Double) -> -- state site tensor
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr (Complex Double) -> -- sparse operator matrices
    Ptr (Complex Double) -> -- right environment
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
    Ptr (Complex Double) -> -- left environment
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr (Complex Double) -> -- sparse operator matrices
    Ptr (Complex Double) -> -- state site tensor
    Ptr (Complex Double) -> -- new left environment
    IO ()

contractSOSLeft :: LeftBoundaryTensor -> LeftAbsorptionNormalizedStateSiteTensor -> OperatorSiteTensor -> LeftBoundaryTensor
contractSOSLeft left_boundary_tensor state_site_tensor operator_site_tensor =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = rightBandwidthOfState state_site_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operatorRightBandwidth operator_site_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in snd . unsafePerformIO $
        withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        withNewPinnedTensor (cr,br) $
            contract_sos_left
                bl
                br
                cl
                cr
                d
                p_left_boundary
                number_of_matrices p_operator_indices p_operator_matrices
                p_state_site_tensor
-- @-node:gcross.20091112145455.1625:contractSOSLeft
-- @+node:gcross.20091112145455.1635:contractSOSRight
foreign import ccall unsafe "contract_sos_right" contract_sos_right :: 
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- left environment
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr (Complex Double) -> -- sparse operator matrices
    Ptr (Complex Double) -> -- state site tensor
    Ptr (Complex Double) -> -- new left environment
    IO ()

contractSOSRight :: RightBoundaryTensor -> RightAbsorptionNormalizedStateSiteTensor -> OperatorSiteTensor -> RightBoundaryTensor
contractSOSRight right_boundary_tensor state_site_tensor operator_site_tensor =
    let bl = leftBandwidthOfState state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = operatorLeftBandwidth operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
    in snd . unsafePerformIO $
        withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
        withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
        withNewPinnedTensor (cl,bl) $
            contract_sos_right
                bl
                br
                cl
                cr
                d
                p_right_boundary
                number_of_matrices p_operator_indices p_operator_matrices
                p_state_site_tensor
-- @-node:gcross.20091112145455.1635:contractSOSRight
-- @+node:gcross.20091116175016.1770:contractSSLeft
foreign import ccall unsafe "contract_ss_left" contract_ss_left :: 
    Int -> -- old state left bandwidth dimension
    Int -> -- old state right bandwidth dimension
    Int -> -- new state left bandwidth dimension
    Int -> -- new state right bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- left environment
    Ptr (Complex Double) -> -- old state site tensor
    Ptr (Complex Double) -> -- new state site tensor
    Ptr (Complex Double) -> -- new left environment
    IO ()

contractSSLeft ::
    LeftAbsorptionNormalizedStateSiteTensor ->
    LeftOverlapBoundaryTensor ->
    LeftAbsorptionNormalizedOverlapSiteTensor ->
    LeftOverlapBoundaryTensor
contractSSLeft state_site_tensor left_boundary_tensor overlap_site_tensor =
    let b_left_old = left_boundary_tensor <-?-> overlap_site_tensor
        b_right_old = rightBandwidthOfState overlap_site_tensor
        b_left_new = left_boundary_tensor <-?-> state_site_tensor
        b_right_new = rightBandwidthOfState state_site_tensor
        d = overlap_site_tensor <-?-> state_site_tensor
    in snd . unsafePerformIO $
        withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
        withPinnedTensor overlap_site_tensor $ \p_overlap_site_tensor ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
        withNewPinnedTensor (b_right_old,b_right_new) $
            contract_ss_left
                b_left_old b_right_old
                b_left_new b_right_new
                d
                p_left_boundary
                p_overlap_site_tensor
                p_state_site_tensor
-- @-node:gcross.20091116175016.1770:contractSSLeft
-- @+node:gcross.20091116175016.1788:contractSSRight
foreign import ccall unsafe "contract_ss_right" contract_ss_right :: 
    Int -> -- old state left bandwidth dimension
    Int -> -- old state right bandwidth dimension
    Int -> -- new state left bandwidth dimension
    Int -> -- new state right bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- right environment
    Ptr (Complex Double) -> -- old state site tensor
    Ptr (Complex Double) -> -- new state site tensor
    Ptr (Complex Double) -> -- new right environment
    IO ()

contractSSRight ::
    RightAbsorptionNormalizedStateSiteTensor ->
    RightOverlapBoundaryTensor ->
    RightAbsorptionNormalizedOverlapSiteTensor ->
    RightOverlapBoundaryTensor
contractSSRight state_site_tensor right_boundary_tensor overlap_site_tensor  =
    let b_left_old = leftBandwidthOfState overlap_site_tensor
        b_right_old = overlap_site_tensor <-?-> right_boundary_tensor
        b_left_new = leftBandwidthOfState state_site_tensor
        b_right_new =  state_site_tensor <-?-> right_boundary_tensor
        d = overlap_site_tensor <-?-> state_site_tensor
    in snd . unsafePerformIO $
        withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
        withPinnedTensor overlap_site_tensor $ \p_overlap_site_tensor ->
        withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
        withNewPinnedTensor (b_left_old,b_left_new) $
            contract_ss_right
                b_left_old b_right_old
                b_left_new b_right_new
                d
                p_right_boundary
                p_overlap_site_tensor
                p_state_site_tensor
-- @-node:gcross.20091116175016.1788:contractSSRight
-- @-node:gcross.20091113125544.1656:Contractors
-- @+node:gcross.20091113125544.1664:Normalizers
-- @+node:gcross.20091113125544.1667:makeNormalizedForAbsorbingLeft
foreign import ccall unsafe "norm_denorm_going_right" norm_denorm_going_right :: 
    Int -> -- leftmost bandwidth dimension
    Int -> -- middle bandwidth dimension
    Int -> -- rightmost bandwidth dimension
    Int -> -- left site physical dimension
    Int -> -- right site physical dimension
    Ptr (Complex Double) -> -- left (normalized) state site tensor
    Ptr (Complex Double) -> -- right (unnormalized) state site tensor
    Ptr (Complex Double) -> -- output left (unnormalized) state site tensor
    Ptr (Complex Double) -> -- output right (normalized) state site tensor
    IO Int

makeNormalizedForAbsorbingLeft ::
    UnnormalizedStateSiteTensor ->
    RightAbsorptionNormalizedStateSiteTensor ->
    (LeftAbsorptionNormalizedStateSiteTensor,UnnormalizedStateSiteTensor)
makeNormalizedForAbsorbingLeft tensor_to_normalize tensor_to_denormalize =
    let bl = leftBandwidthOfState tensor_to_normalize
        bm = tensor_to_normalize <-?-> tensor_to_denormalize
        br = rightBandwidthOfState tensor_to_denormalize
        dl = physicalDimensionOfState tensor_to_normalize
        dr = physicalDimensionOfState tensor_to_denormalize
        ((info,denormalized_state_site_tensor),normalized_state_site_tensor) = unsafePerformIO $
            withPinnedTensor tensor_to_denormalize $ \p_tensor_to_denormalize ->
            withPinnedTensor tensor_to_normalize $ \p_tensor_to_normalize ->
            withNewPinnedTensor (dl,bl,bm) $ \p_normalized_state_site_tensor_storable_array ->
            withNewPinnedTensor (dr,bm,br) $
                norm_denorm_going_right
                    bl
                    bm
                    br
                    dl
                    dr
                    p_tensor_to_normalize
                    p_tensor_to_denormalize
                    p_normalized_state_site_tensor_storable_array
    in if info /= 0
        then error "unable to normalize tensor!"
        else (normalized_state_site_tensor,denormalized_state_site_tensor)
-- @-node:gcross.20091113125544.1667:makeNormalizedForAbsorbingLeft
-- @+node:gcross.20091113125544.1665:makeNormalizedForAbsorbingRight
foreign import ccall unsafe "norm_denorm_going_left" norm_denorm_going_left :: 
    Int -> -- leftmost bandwidth dimension
    Int -> -- middle bandwidth dimension
    Int -> -- rightmost bandwidth dimension
    Int -> -- left site physical dimension
    Int -> -- right site physical dimension
    Ptr (Complex Double) -> -- left (normalized) state site tensor
    Ptr (Complex Double) -> -- right (unnormalized) state site tensor
    Ptr (Complex Double) -> -- output left (unnormalized) state site tensor
    Ptr (Complex Double) -> -- output right (normalized) state site tensor
    IO Int

makeNormalizedForAbsorbingRight ::
    LeftAbsorptionNormalizedStateSiteTensor ->
    UnnormalizedStateSiteTensor ->
    (UnnormalizedStateSiteTensor,RightAbsorptionNormalizedStateSiteTensor)
makeNormalizedForAbsorbingRight tensor_to_denormalize tensor_to_normalize =
    let bl = leftBandwidthOfState tensor_to_denormalize
        bm = tensor_to_denormalize <-?-> tensor_to_normalize
        br = rightBandwidthOfState tensor_to_normalize
        dl = physicalDimensionOfState tensor_to_denormalize
        dr = physicalDimensionOfState tensor_to_normalize
        ((info,normalized_state_site_tensor),denormalized_state_site_tensor) = unsafePerformIO $
            withPinnedTensor tensor_to_normalize $ \p_tensor_to_normalize ->
            withPinnedTensor tensor_to_denormalize $ \p_tensor_to_denormalize ->
            withNewPinnedTensor (dl,bl,bm) $ \p_denormalized_state_site_tensor_storable_array ->
            withNewPinnedTensor (dr,bm,br) $
                norm_denorm_going_left
                    bl
                    bm
                    br
                    dl
                    dr
                    p_tensor_to_denormalize
                    p_tensor_to_normalize
                    p_denormalized_state_site_tensor_storable_array
    in if info /= 0
        then error "unable to normalize tensor!"
        else (denormalized_state_site_tensor,normalized_state_site_tensor)
-- @-node:gcross.20091113125544.1665:makeNormalizedForAbsorbingRight
-- @-node:gcross.20091113125544.1664:Normalizers
-- @+node:gcross.20091112145455.1656:Randomizers
class RandomizableStateSiteTensor a where
    generateRandomizedStateSiteTensor :: Int -> Int -> Int -> IO a

applyRandomizerAndReturnStateSiteTensor :: (Creatable a (Int,Int,Int)) => (Int -> Int -> Int -> Ptr (Complex Double) -> IO ()) -> Int -> Int -> Int -> IO a
applyRandomizerAndReturnStateSiteTensor randomizer physical_dimension left_bandwidth_dimension right_bandwidth_dimension =
    let d  = physical_dimension
        bl = left_bandwidth_dimension
        br = right_bandwidth_dimension
    in fmap (snd) $ withNewPinnedTensor (d,bl,br) $ randomizer br bl d
-- @+node:gcross.20091112145455.1674:unnormalized
foreign import ccall unsafe "rand_unnorm_state_site_tensor" rand_unnorm_state_site_tensor :: 
    Int -> -- state right bandwidth dimension
    Int -> -- state left bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- state site tensor
    IO ()

instance RandomizableStateSiteTensor UnnormalizedStateSiteTensor where
    generateRandomizedStateSiteTensor = applyRandomizerAndReturnStateSiteTensor rand_unnorm_state_site_tensor
-- @-node:gcross.20091112145455.1674:unnormalized
-- @+node:gcross.20091112145455.1675:normalized
foreign import ccall unsafe "rand_norm_state_site_tensor" rand_norm_state_site_tensor :: 
    Int -> -- state right bandwidth dimension
    Int -> -- state left bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- state site tensor
    IO ()

instance RandomizableStateSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    generateRandomizedStateSiteTensor = applyRandomizerAndReturnStateSiteTensor rand_norm_state_site_tensor
-- @-node:gcross.20091112145455.1675:normalized
-- @-node:gcross.20091112145455.1656:Randomizers
-- @+node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
foreign import ccall unsafe "optimize" optimize ::
    Int -> -- state left bandwidth dimension
    Int -> -- state right bandwidth dimension
    Int -> -- operator left bandwidth dimension
    Int -> -- operator right bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- left environment
    Int -> -- number of matrices
    Ptr Int32 -> -- sparse operator indices
    Ptr (Complex Double) -> -- sparse operator matrices
    Ptr (Complex Double) -> -- right environment
    Int -> -- number of projectors
    Ptr (Complex Double) -> -- projectors
    CString -> -- which eigenvectors to select
    Double -> -- tolerance
    Ptr Int -> -- cap on the number of iterations / number of iterations that were used
    Ptr (Complex Double) -> -- initial guess for the state site tensor
    Ptr (Complex Double) -> -- where the result will be written
    Ptr (Complex Double) -> -- the minimal eigenvalue
    IO Int32

computeOptimalSiteStateTensor ::
    LeftBoundaryTensor ->
    UnnormalizedStateSiteTensor ->
    OperatorSiteTensor ->
    RightBoundaryTensor ->
    ProjectorMatrix ->
    OptimizerSelectionStrategy ->
    Double ->
    Int ->
    Either OptimizerFailureReason (Int, Complex Double, UnnormalizedStateSiteTensor)
computeOptimalSiteStateTensor
    left_boundary_tensor
    state_site_tensor
    operator_site_tensor
    right_boundary_tensor
    projector_matrix
    strategy
    tolerance
    maximum_number_of_iterations
    =
    let bl = left_boundary_tensor <-?-> state_site_tensor
        br = state_site_tensor <-?-> right_boundary_tensor
        cl = left_boundary_tensor <-?-> operator_site_tensor
        cr = operator_site_tensor <-?-> right_boundary_tensor
        d = operator_site_tensor <-?-> state_site_tensor
        ((info,number_of_iterations,eigenvalue),optimized_state_site_tensor) = unsafePerformIO $
            withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
            withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
            withPinnedOperatorSiteTensor operator_site_tensor $ \number_of_matrices p_operator_indices p_operator_matrices ->
            withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
            withPinnedProjectorMatrix projector_matrix $ \number_of_projectors p_projector_matrix ->
            withStrategyAsCString strategy $ \p_strategy ->
            with maximum_number_of_iterations  $ \p_number_of_iterations ->
            alloca $ \p_eigenvalue ->
            withNewPinnedTensor (d,bl,br) $ \p_result -> (
                optimize
                    bl
                    br
                    cl
                    cr
                    d
                    p_left_boundary
                    number_of_matrices p_operator_indices p_operator_matrices
                    p_right_boundary
                    number_of_projectors
                    p_projector_matrix
                    p_strategy
                    tolerance
                    p_number_of_iterations
                    p_state_site_tensor
                    p_result
                    p_eigenvalue
                >>= \info ->
                        liftM3 (,,) 
                            (return info)
                            (peek p_number_of_iterations)
                            (peek p_eigenvalue)
            )
    in case info of
        0 -> Right (number_of_iterations,eigenvalue,optimized_state_site_tensor)
        -14 -> Left OptimizerUnableToConverge
        _ -> error $ "Failed to converge! info = " ++ show info
-- @-node:gcross.20091111171052.1656:computeOptimalSiteStateTensor
-- @+node:gcross.20091115105949.1729:increaseBandwidthBetween
foreign import ccall unsafe "increase_bandwidth_between" increase_bandwidth_between :: 
    Int -> -- leftmost bandwidth dimension
    Int -> -- middle bandwidth dimension
    Int -> -- rightmost bandwidth dimension
    Int -> -- new middle bandwidth dimension
    Int -> -- left site physical dimension
    Int -> -- right site physical dimension
    Ptr (Complex Double) -> -- left (normalized) state site tensor
    Ptr (Complex Double) -> -- right (unnormalized) state site tensor
    Ptr (Complex Double) -> -- output left (unnormalized) state site tensor
    Ptr (Complex Double) -> -- output right (normalized) state site tensor
    IO Int

increaseBandwidthBetween ::
    Int ->
    RightAbsorptionNormalizedStateSiteTensor ->
    UnnormalizedStateSiteTensor ->
    IO (UnnormalizedStateSiteTensor,RightAbsorptionNormalizedStateSiteTensor)
increaseBandwidthBetween new_bm tensor_to_denormalize tensor_to_normalize =
    let bl = leftBandwidthOfState tensor_to_denormalize
        bm = let bandwidth = rightBandwidthOfState tensor_to_denormalize
                in assert (bandwidth == leftBandwidthOfState tensor_to_normalize)
                    $ bandwidth
        br = rightBandwidthOfState tensor_to_normalize
        dl = physicalDimensionOfState tensor_to_denormalize
        dr  = physicalDimensionOfState tensor_to_normalize
    in if bm > new_bm then error "can't use this function to decrease bandwidth!" else
        ( 
            withPinnedTensor tensor_to_denormalize $ \p_tensor_to_denormalize ->
            withPinnedTensor tensor_to_normalize $ \p_tensor_to_normalize ->
            withNewPinnedTensor (dl,bl,new_bm) $ \p_denormalized_state_site_tensor ->
            withNewPinnedTensor (dr,new_bm,br) $
                if new_bm > bm
                    then
                        increase_bandwidth_between
                            bl
                            bm
                            br
                            dl
                            dr
                            new_bm
                            p_tensor_to_denormalize
                            p_tensor_to_normalize
                            p_denormalized_state_site_tensor
                    else
                        norm_denorm_going_left
                            bl
                            bm
                            br
                            dl
                            dr
                            p_tensor_to_denormalize
                            p_tensor_to_normalize
                            p_denormalized_state_site_tensor
        ) >>= \((info,normalized_state_site_tensor),denormalized_state_site_tensor) ->
            if info /= 0
                then error "unable to normalize tensor!"
                else return (denormalized_state_site_tensor,normalized_state_site_tensor)
-- @-node:gcross.20091115105949.1729:increaseBandwidthBetween
-- @+node:gcross.20091116175016.1797:formProjectorMatrix
foreign import ccall unsafe "form_overlap_vector" form_overlap_vector :: 
    Int -> -- old state left bandwidth dimension
    Int -> -- old state right bandwidth dimension
    Int -> -- new state left bandwidth dimension
    Int -> -- new state right bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- left environment
    Ptr (Complex Double) -> -- right environment
    Ptr (Complex Double) -> -- old state site tensor
    Ptr (Complex Double) -> -- output overlap vector
    IO ()

foreign import ccall unsafe "orthogonalize_projector_matrix" orthogonalize_projector_matrix :: 
    Int -> -- number of projectors
    Int -> -- projector length
    Ptr (Complex Double) -> -- projector matrix
    IO ()

formProjectorMatrix :: 
    [(LeftOverlapBoundaryTensor
     ,RightOverlapBoundaryTensor
     ,UnnormalizedOverlapSiteTensor
     )] -> ProjectorMatrix

formProjectorMatrix [] = NullProjectorMatrix
formProjectorMatrix projectors@(first_projector:_) =
    snd . unsafePerformIO $
        withNewPinnedProjectorMatrix number_of_projectors projector_length $
            \p_projector_matrix ->
                go projectors p_projector_matrix
                >>
                case number_of_projectors of
                    1 -> do normalize projector_length p_projector_matrix
                    2 -> orthogonalize2
                            projector_length
                            p_projector_matrix
                            (plusPtr p_projector_matrix pointer_increment)
                    _ -> 
                     do orthogonalize_projector_matrix
                            number_of_projectors
                            projector_length
                            p_projector_matrix
  where
    number_of_projectors = length projectors
    (first_left_boundary,first_right_boundary,first_overlap_site_tensor) = first_projector
    b_left_new = getNewStateBandwidth first_left_boundary
    b_right_new = getNewStateBandwidth first_right_boundary
    d = physicalDimensionOfState first_overlap_site_tensor
    projector_length = b_left_new*b_right_new*d

    pointer_increment = projector_length * sizeOf (undefined :: Complex Double)

    go [] _ = return ()
    go ((left_boundary_tensor
        ,right_boundary_tensor
        ,overlap_site_tensor
        ):rest_projectors)
       p_projector
       =
        let b_left_old = left_boundary_tensor <-?-> overlap_site_tensor
            b_right_old = overlap_site_tensor <-?-> right_boundary_tensor
        in (withPinnedTensor left_boundary_tensor $ \p_left_boundary ->
            withPinnedTensor right_boundary_tensor $ \p_right_boundary ->
            withPinnedTensor overlap_site_tensor $ \p_overlap_site_tensor ->
                form_overlap_vector
                    b_left_old b_right_old
                    b_left_new b_right_new
                    d
                    p_left_boundary
                    p_right_boundary
                    p_overlap_site_tensor
                    p_projector
            ) >> go rest_projectors (p_projector `plusPtr` pointer_increment)
-- @-node:gcross.20091116175016.1797:formProjectorMatrix
-- @+node:gcross.20091120134444.1598:applyProjectorMatrix
foreign import ccall unsafe "project" project :: 
    Int -> -- vector size
    Int -> -- number of projectors
    Ptr (Complex Double) -> -- input: projector matrix
    Ptr (Complex Double) -> -- input: state site tensor
    Ptr (Complex Double) -> -- output: projected state site tensor
    IO ()

applyProjectorMatrix ::
    ProjectorMatrix ->
    UnnormalizedStateSiteTensor ->
    UnnormalizedStateSiteTensor
applyProjectorMatrix NullProjectorMatrix state_site_tensor = state_site_tensor
applyProjectorMatrix projector_matrix state_site_tensor =
    let br = rightBandwidthOfState state_site_tensor
        bl = leftBandwidthOfState state_site_tensor
        d = physicalDimensionOfState state_site_tensor
    in snd . unsafePerformIO $
            withPinnedProjectorMatrix projector_matrix $ \number_of_projectors p_projector_matrix ->
            withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
            withNewPinnedTensor (d,bl,br) $
                project
                    (br*bl*d)
                    number_of_projectors
                    p_projector_matrix
                    p_state_site_tensor
-- @-node:gcross.20091120134444.1598:applyProjectorMatrix
-- @+node:gcross.20091118141720.1810:Overlap tensor formation
-- @+node:gcross.20091118141720.1812:makeOverlapSiteTensor
foreign import ccall unsafe "form_overlap_site_tensor" form_overlap_site_tensor :: 
    Int -> -- rightmost bandwidth dimension
    Int -> -- leftmost bandwidth dimension
    Int -> -- physical dimension
    Ptr (Complex Double) -> -- input: state site tensor
    Ptr (Complex Double) -> -- output: overlap site tensor
    IO ()

makeOverlapSiteTensor ::
    UnnormalizedStateSiteTensor ->
    UnnormalizedOverlapSiteTensor
makeOverlapSiteTensor state_site_tensor =
    let br = rightBandwidthOfState state_site_tensor
        bl = leftBandwidthOfState state_site_tensor
        d = physicalDimensionOfState state_site_tensor
    in snd . unsafePerformIO $
            withPinnedTensor state_site_tensor $ \p_state_site_tensor ->
            withNewPinnedTensor (d,bl,br) $ \p_overlap_site_tensor ->
                form_overlap_site_tensor
                    br
                    bl
                    d
                    p_state_site_tensor
                    p_overlap_site_tensor
-- @-node:gcross.20091118141720.1812:makeOverlapSiteTensor
-- @+node:gcross.20091118141720.1814:makeAndNormalizeOverlapSiteTensors
foreign import ccall unsafe "form_norm_overlap_tensors" form_norm_overlap_tensors :: 
    Int -> -- leftmost bandwidth dimension
    Int -> -- middle bandwidth dimension
    Int -> -- rightmost bandwidth dimension
    Int -> -- left physical dimension
    Int -> -- right physical dimension
    Ptr (Complex Double) -> -- input: unnormalized state left site tensor
    Ptr (Complex Double) -> -- input: right normalized state right site tensor
    Ptr (Complex Double) -> -- output: left normalized overlap left site tensor
    Ptr (Complex Double) -> -- output: unnormalized overlap left site tensor
    Ptr (Complex Double) -> -- output: unnormalized state right site tensor
    Ptr (Complex Double) -> -- output: right normalized overlap right site tensor
    IO ()

makeAndNormalizeOverlapSiteTensors ::
    UnnormalizedStateSiteTensor ->
    RightAbsorptionNormalizedStateSiteTensor ->
    (LeftAbsorptionNormalizedOverlapSiteTensor
    ,UnnormalizedOverlapSiteTensor
    ,UnnormalizedStateSiteTensor
    ,RightAbsorptionNormalizedOverlapSiteTensor
    )
makeAndNormalizeOverlapSiteTensors unnormalized_state_left_site_tensor right_normalized_state_right_site_tensor =
    let bl = leftBandwidthOfState unnormalized_state_left_site_tensor
        bm = unnormalized_state_left_site_tensor <-?-> right_normalized_state_right_site_tensor
        br = rightBandwidthOfState right_normalized_state_right_site_tensor
        dl = physicalDimensionOfState unnormalized_state_left_site_tensor
        dr = physicalDimensionOfState right_normalized_state_right_site_tensor
        (((((
            ),right_normalized_overlap_right_site_tensor
            ),unnormalized_state_right_site_tensor
            ),unnormalized_overlap_left_site_tensor
            ),left_normalized_overlap_left_site_tensor
          ) = unsafePerformIO $
            withPinnedTensor unnormalized_state_left_site_tensor $ \p_unnormalized_state_left_site_tensor ->
            withPinnedTensor right_normalized_state_right_site_tensor $ \p_right_normalized_state_right_site_tensor ->
            withNewPinnedTensor (dl,bl,bm) $ \p_left_normalized_overlap_left_site_tensor ->
            withNewPinnedTensor (dl,bl,bm) $ \p_unnormalized_overlap_left_site_tensor ->
            withNewPinnedTensor (dr,bm,br) $ \p_unnormalized_state_right_site_tensor ->
            withNewPinnedTensor (dr,bm,br) $ \p_right_normalized_overlap_right_site_tensor ->
                form_norm_overlap_tensors
                    bl bm br
                    dl dr
                    p_unnormalized_state_left_site_tensor
                    p_right_normalized_state_right_site_tensor
                    p_left_normalized_overlap_left_site_tensor
                    p_unnormalized_overlap_left_site_tensor
                    p_unnormalized_state_right_site_tensor
                    p_right_normalized_overlap_right_site_tensor
    in (left_normalized_overlap_left_site_tensor
       ,unnormalized_overlap_left_site_tensor
       ,unnormalized_state_right_site_tensor
       ,right_normalized_overlap_right_site_tensor
       )
-- @-node:gcross.20091118141720.1814:makeAndNormalizeOverlapSiteTensors
-- @-node:gcross.20091118141720.1810:Overlap tensor formation
-- @+node:gcross.20091211120042.1685:applySingleSiteOperator
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
-- @-node:gcross.20091211120042.1685:applySingleSiteOperator
-- @-node:gcross.20091113125544.1661:Wrapper functions
-- @-others
-- @-node:gcross.20091113125544.1653:@thin Wrappers.hs
-- @-leo
