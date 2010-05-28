-- @+leo-ver=4-thin
-- @+node:gcross.20091110205054.1969:@thin Implementation.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091111171052.1600:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

-- @-node:gcross.20091111171052.1600:<< Language extensions >>
-- @nl

module VMPS.Tensors.Implementation where

-- @<< Import needed modules >>
-- @+node:gcross.20091111171052.1599:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.Array.Unboxed
import Data.Complex
import Data.Function
import Data.Int
import Data.Typeable
import Data.Vec ((:.)(..))

import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import Text.Printf

import VMPS.Miscellaneous
-- @-node:gcross.20091111171052.1599:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091111171052.1596:ComplexTensor
newtype MyIx i => ComplexTensor i = ComplexTensor { unwrapComplexTensor :: StorableArray i (Complex Double) }

withPinnedComplexTensor :: MyIx i => ComplexTensor i → (Ptr (Complex Double) → IO a) → IO a
withPinnedComplexTensor = withStorableArray . unwrapComplexTensor

withNewPinnedComplexTensor dimensions thunk = do
    storable_array ← newMyArray dimensions
    result ← withStorableArray storable_array thunk
    return (result,ComplexTensor storable_array)

complexTensorFromList :: MyIx i => i → [Complex Double] → ComplexTensor i
complexTensorFromList dimensions = ComplexTensor . myListArray dimensions

trivial_complex_tensor :: MyIx i => ComplexTensor i
trivial_complex_tensor = complexTensorFromList lowerBounds [1]

toListOfComplexNumbers :: MyIx i => ComplexTensor i → [Complex Double]
toListOfComplexNumbers = unsafePerformIO . getElems . unwrapComplexTensor
-- @nonl
-- @-node:gcross.20091111171052.1596:ComplexTensor
-- @+node:gcross.20091113142219.2538:Tensors
-- @+node:gcross.20091111171052.1595:Left/Right Boundaries
data BoundaryTensor = BoundaryTensor
    {   boundaryOperatorBandwidth :: !Int
    ,   boundaryStateBandwidth :: !Int
    ,   boundaryData :: !(ComplexTensor (Int,Int,Int))
    }

withPinnedBoundaryTensor = withPinnedComplexTensor . boundaryData
withNewPinnedBoundaryTensor (cl,bl) =
    fmap (second $ BoundaryTensor cl bl)
    .
    withNewPinnedComplexTensor (cl,bl,bl)

trivial_boundary = BoundaryTensor 1 1 trivial_complex_tensor

-- @+others
-- @+node:gcross.20091112145455.1649:Left boundary
newtype LeftBoundaryTensor = LeftBoundaryTensor { unwrapLeftBoundaryTensor :: BoundaryTensor }

instance Pinnable LeftBoundaryTensor where 
    withPinnedTensor = withPinnedBoundaryTensor . unwrapLeftBoundaryTensor

instance Creatable LeftBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftBoundaryTensor)
        .
        withNewPinnedBoundaryTensor bounds

instance Connected LeftBoundaryTensor UnnormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Left boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        (boundaryStateBandwidth . unwrapLeftBoundaryTensor)
        (stateLeftBandwidth . unwrapUnnormalizedStateSiteTensor)

instance Connected LeftBoundaryTensor LeftAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Left boundary and (left-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        (boundaryStateBandwidth . unwrapLeftBoundaryTensor)
        (stateLeftBandwidth . unwrapLeftAbsorptionNormalizedStateSiteTensor)

instance Connected LeftBoundaryTensor OperatorSiteTensor where
    (←?→) = makeConnectedTest
        "Left boundary and operator site tensors disagree over the bandwidth dimension!"
        (boundaryOperatorBandwidth . unwrapLeftBoundaryTensor)
        operatorLeftBandwidth

trivial_left_boundary = LeftBoundaryTensor trivial_boundary
-- @nonl
-- @-node:gcross.20091112145455.1649:Left boundary
-- @+node:gcross.20091112145455.1651:Right boundary
newtype RightBoundaryTensor = RightBoundaryTensor { unwrapRightBoundaryTensor :: BoundaryTensor }

instance Pinnable RightBoundaryTensor where 
    withPinnedTensor = withPinnedBoundaryTensor . unwrapRightBoundaryTensor

instance Creatable RightBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightBoundaryTensor)
        .
        withNewPinnedBoundaryTensor bounds

instance Connected UnnormalizedStateSiteTensor RightBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        (stateRightBandwidth . unwrapUnnormalizedStateSiteTensor)
        (boundaryStateBandwidth . unwrapRightBoundaryTensor)

instance Connected RightAbsorptionNormalizedStateSiteTensor RightBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right boundary and (right-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        (stateRightBandwidth . unwrapRightAbsorptionNormalizedStateSiteTensor)
        (boundaryStateBandwidth . unwrapRightBoundaryTensor)

instance Connected OperatorSiteTensor RightBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right boundary and operator site tensors disagree over the bandwidth dimension!"
        operatorRightBandwidth
        (boundaryOperatorBandwidth . unwrapRightBoundaryTensor)

trivial_right_boundary = RightBoundaryTensor trivial_boundary
-- @nonl
-- @-node:gcross.20091112145455.1651:Right boundary
-- @-others
-- @-node:gcross.20091111171052.1595:Left/Right Boundaries
-- @+node:gcross.20091116175016.1762:Left/Right Overlap Boundaries
data OverlapBoundaryTensor = OverlapBoundaryTensor
    {   overlapOldStateBandwidth :: !Int
    ,   overlapNewStateBandwidth :: !Int
    ,   overlapData :: !(ComplexTensor (Int,Int))
    }

withPinnedOverlapBoundaryTensor = withPinnedComplexTensor . overlapData
withNewPinnedOverlapBoundaryTensor (cl,bl) =
    fmap (second $ OverlapBoundaryTensor cl bl)
    .
    withNewPinnedComplexTensor (cl,bl)

trivial_overlap_boundary = OverlapBoundaryTensor 1 1 trivial_complex_tensor

class OverlapBoundaryTensorClass a where
    getNewStateBandwidth :: a → Int

-- @+others
-- @+node:gcross.20091116175016.1763:Left boundary
newtype LeftOverlapBoundaryTensor = LeftOverlapBoundaryTensor { unwrapLeftOverlapBoundaryTensor :: OverlapBoundaryTensor }

instance Pinnable LeftOverlapBoundaryTensor where 
    withPinnedTensor = withPinnedOverlapBoundaryTensor . unwrapLeftOverlapBoundaryTensor

instance Creatable LeftOverlapBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftOverlapBoundaryTensor)
        .
        withNewPinnedOverlapBoundaryTensor bounds

instance OverlapBoundaryTensorClass LeftOverlapBoundaryTensor where
    getNewStateBandwidth = overlapNewStateBandwidth . unwrapLeftOverlapBoundaryTensor

instance Connected LeftOverlapBoundaryTensor UnnormalizedOverlapSiteTensor where
    (←?→) = makeConnectedTest
        "Left overlap boundary and (unnormalized) overlap site tensors disagree over the bandwidth dimension!"
        (overlapOldStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

instance Connected LeftOverlapBoundaryTensor LeftAbsorptionNormalizedOverlapSiteTensor where
    (←?→) = makeConnectedTest
        "Left overlap boundary and (left-absorption normalized) overlap site tensors disagree over the bandwidth dimension!"
        (overlapOldStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

instance Connected LeftOverlapBoundaryTensor UnnormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Left overlap boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        (overlapNewStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

instance Connected LeftOverlapBoundaryTensor LeftAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Left overlap boundary and (left-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        (overlapNewStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

trivial_left_overlap_boundary = LeftOverlapBoundaryTensor trivial_overlap_boundary
-- @nonl
-- @-node:gcross.20091116175016.1763:Left boundary
-- @+node:gcross.20091116175016.1768:Right boundary
newtype RightOverlapBoundaryTensor = RightOverlapBoundaryTensor { unwrapRightOverlapBoundaryTensor :: OverlapBoundaryTensor }

instance Pinnable RightOverlapBoundaryTensor where 
    withPinnedTensor = withPinnedOverlapBoundaryTensor . unwrapRightOverlapBoundaryTensor

instance Creatable RightOverlapBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightOverlapBoundaryTensor)
        .
        withNewPinnedOverlapBoundaryTensor bounds

instance OverlapBoundaryTensorClass RightOverlapBoundaryTensor where
    getNewStateBandwidth = overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor

instance Connected UnnormalizedOverlapSiteTensor RightOverlapBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right overlap boundary and (unnormalized) overlap site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapOldStateBandwidth . unwrapRightOverlapBoundaryTensor)

instance Connected RightAbsorptionNormalizedOverlapSiteTensor RightOverlapBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right overlap boundary and (Right-absorption normalized) overlap site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapOldStateBandwidth . unwrapRightOverlapBoundaryTensor)

instance Connected UnnormalizedStateSiteTensor RightOverlapBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right overlap boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor)

instance Connected RightAbsorptionNormalizedStateSiteTensor RightOverlapBoundaryTensor where
    (←?→) = makeConnectedTest
        "Right overlap boundary and (right-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor)

trivial_right_overlap_boundary = RightOverlapBoundaryTensor trivial_overlap_boundary
-- @nonl
-- @-node:gcross.20091116175016.1768:Right boundary
-- @-others
-- @nonl
-- @-node:gcross.20091116175016.1762:Left/Right Overlap Boundaries
-- @+node:gcross.20091111171052.1597:State Site Tensor
data StateSiteTensor = StateSiteTensor
    {   statePhysicalDimension :: !Int
    ,   stateLeftBandwidth :: !Int
    ,   stateRightBandwidth :: !Int
    ,   stateData :: !(ComplexTensor (Int,Int,Int))
    } deriving (Typeable)


withPinnedStateTensor = withPinnedComplexTensor . stateData
withNewPinnedStateTensor bounds@(d,bl,br) =
    fmap (second $ StateSiteTensor d bl br)
    .
    withNewPinnedComplexTensor bounds
withNewPinnedStateTensor bounds@(d,bl,br) =
    fmap (second $ StateSiteTensor d bl br)
    .
    withNewPinnedComplexTensor bounds    
normOfStateSiteTensor (StateSiteTensor d bl br state_data) =
    unsafePerformIO $
    withPinnedComplexTensor state_data $
        \ptr → norm (d*bl*br) ptr

instance Connected LeftAbsorptionNormalizedStateSiteTensor UnnormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        leftBandwidthOfState

instance Connected UnnormalizedStateSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        leftBandwidthOfState

instance Connected UnnormalizedOverlapSiteTensor UnnormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        physicalDimensionOfState
        physicalDimensionOfState

instance Connected LeftAbsorptionNormalizedOverlapSiteTensor LeftAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        physicalDimensionOfState
        physicalDimensionOfState

instance Connected RightAbsorptionNormalizedOverlapSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        physicalDimensionOfState
        physicalDimensionOfState
-- @nonl
-- @+node:gcross.20091116175016.1755:newtypes
newtype UnnormalizedStateSiteTensor = UnnormalizedStateSiteTensor
    { unwrapUnnormalizedStateSiteTensor :: StateSiteTensor }
    deriving (Typeable)
newtype LeftAbsorptionNormalizedStateSiteTensor = LeftAbsorptionNormalizedStateSiteTensor
    { unwrapLeftAbsorptionNormalizedStateSiteTensor :: StateSiteTensor }
    deriving (Typeable)
newtype RightAbsorptionNormalizedStateSiteTensor = RightAbsorptionNormalizedStateSiteTensor
    { unwrapRightAbsorptionNormalizedStateSiteTensor :: StateSiteTensor }
    deriving (Typeable)
newtype UnnormalizedOverlapSiteTensor = UnnormalizedOverlapSiteTensor
    { unwrapUnnormalizedOverlapSiteTensor :: StateSiteTensor }
    deriving (Typeable)
newtype LeftAbsorptionNormalizedOverlapSiteTensor = LeftAbsorptionNormalizedOverlapSiteTensor
    { unwrapLeftAbsorptionNormalizedOverlapSiteTensor :: StateSiteTensor }
    deriving (Typeable)
newtype RightAbsorptionNormalizedOverlapSiteTensor = RightAbsorptionNormalizedOverlapSiteTensor
    { unwrapRightAbsorptionNormalizedOverlapSiteTensor :: StateSiteTensor }
    deriving (Typeable)
-- @-node:gcross.20091116175016.1755:newtypes
-- @+node:gcross.20091116175016.1756:Pinnable instances
instance Pinnable UnnormalizedStateSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapUnnormalizedStateSiteTensor
instance Pinnable LeftAbsorptionNormalizedStateSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
instance Pinnable RightAbsorptionNormalizedStateSiteTensor where 
    withPinnedTensor = withPinnedStateTensor . unwrapRightAbsorptionNormalizedStateSiteTensor
instance Pinnable UnnormalizedOverlapSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapUnnormalizedOverlapSiteTensor
instance Pinnable LeftAbsorptionNormalizedOverlapSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
instance Pinnable RightAbsorptionNormalizedOverlapSiteTensor where 
    withPinnedTensor = withPinnedStateTensor . unwrapRightAbsorptionNormalizedOverlapSiteTensor
-- @-node:gcross.20091116175016.1756:Pinnable instances
-- @+node:gcross.20091116175016.1757:Creatable instances
instance Creatable UnnormalizedStateSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second UnnormalizedStateSiteTensor)
        .
        withNewPinnedStateTensor bounds
instance Creatable LeftAbsorptionNormalizedStateSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftAbsorptionNormalizedStateSiteTensor)
        .
        withNewPinnedStateTensor bounds
instance Creatable RightAbsorptionNormalizedStateSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightAbsorptionNormalizedStateSiteTensor)
        .
        withNewPinnedStateTensor bounds
instance Creatable UnnormalizedOverlapSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second UnnormalizedOverlapSiteTensor)
        .
        withNewPinnedStateTensor bounds
instance Creatable LeftAbsorptionNormalizedOverlapSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftAbsorptionNormalizedOverlapSiteTensor)
        .
        withNewPinnedStateTensor bounds
instance Creatable RightAbsorptionNormalizedOverlapSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightAbsorptionNormalizedOverlapSiteTensor)
        .
        withNewPinnedStateTensor bounds
-- @-node:gcross.20091116175016.1757:Creatable instances
-- @+node:gcross.20091116175016.1758:StateSiteTensorClass + instances
class StateSiteTensorClass a where
    leftBandwidthOfState :: a → Int
    rightBandwidthOfState :: a → Int
    physicalDimensionOfState :: a → Int
    unnormalize :: a → UnnormalizedStateSiteTensor
    normOfState :: a → Double

numberOfDegreesOfFreedomInState :: StateSiteTensorClass a => a → Int
numberOfDegreesOfFreedomInState = leftBandwidthOfState <^(*)^> rightBandwidthOfState <^(*)^> physicalDimensionOfState

instance StateSiteTensorClass UnnormalizedStateSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapUnnormalizedStateSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapUnnormalizedStateSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapUnnormalizedStateSiteTensor
    unnormalize = id
    normOfState = normOfStateSiteTensor . unwrapUnnormalizedStateSiteTensor
instance StateSiteTensorClass LeftAbsorptionNormalizedStateSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapLeftAbsorptionNormalizedStateSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapLeftAbsorptionNormalizedStateSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapLeftAbsorptionNormalizedStateSiteTensor
    unnormalize = UnnormalizedStateSiteTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
    normOfState = normOfStateSiteTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
instance StateSiteTensorClass RightAbsorptionNormalizedStateSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapRightAbsorptionNormalizedStateSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapRightAbsorptionNormalizedStateSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapRightAbsorptionNormalizedStateSiteTensor
    unnormalize = UnnormalizedStateSiteTensor . unwrapRightAbsorptionNormalizedStateSiteTensor
    normOfState = normOfStateSiteTensor . unwrapRightAbsorptionNormalizedStateSiteTensor
instance StateSiteTensorClass UnnormalizedOverlapSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapUnnormalizedOverlapSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapUnnormalizedOverlapSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapUnnormalizedOverlapSiteTensor
    unnormalize = undefined
    normOfState = normOfStateSiteTensor . unwrapUnnormalizedOverlapSiteTensor
instance StateSiteTensorClass LeftAbsorptionNormalizedOverlapSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    unnormalize = undefined
    normOfState = normOfStateSiteTensor . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
instance StateSiteTensorClass RightAbsorptionNormalizedOverlapSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    unnormalize = undefined
    normOfState = normOfStateSiteTensor . unwrapRightAbsorptionNormalizedOverlapSiteTensor
-- @nonl
-- @-node:gcross.20091116175016.1758:StateSiteTensorClass + instances
-- @-node:gcross.20091111171052.1597:State Site Tensor
-- @+node:gcross.20091111171052.1598:Operator Site Tensor
data OperatorSiteTensor = OperatorSiteTensor
    {   operatorLeftBandwidth :: !Int
    ,   operatorRightBandwidth :: !Int
    ,   operatorPhysicalDimension :: !Int
    ,   operatorNumberOfMatrices :: !Int
    ,   operatorIndices :: !(StorableArray (Int,Int) Int32)
    ,   operatorMatrices :: !(StorableArray (Int,Int,Int) (Complex Double))
    }

-- @+others
-- @+node:gcross.20091114174920.1715:withPinnedOperatorSiteTensor
withPinnedOperatorSiteTensor :: OperatorSiteTensor → (Int → Ptr Int32 → Ptr (Complex Double) → IO a) → IO a
withPinnedOperatorSiteTensor operator_site_tensor thunk = 
    (withStorableArray . operatorIndices) operator_site_tensor $ \p_indices →
    (withStorableArray . operatorMatrices) operator_site_tensor $
    thunk (operatorNumberOfMatrices operator_site_tensor) p_indices . castPtr
-- @nonl
-- @-node:gcross.20091114174920.1715:withPinnedOperatorSiteTensor
-- @+node:gcross.20091114174920.1717:(Connected instances)
instance Connected OperatorSiteTensor UnnormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Operator and (unnormalized) state site tensors disagree over the physical dimension!"
        operatorPhysicalDimension
        physicalDimensionOfState

instance Connected OperatorSiteTensor LeftAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Operator and (unnormalized) state site tensors disagree over the physical dimension!"
        operatorPhysicalDimension
        physicalDimensionOfState

instance Connected OperatorSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    (←?→) = makeConnectedTest
        "Operator and (unnormalized) state site tensors disagree over the physical dimension!"
        operatorPhysicalDimension
        physicalDimensionOfState
-- @nonl
-- @-node:gcross.20091114174920.1717:(Connected instances)
-- @-others

-- @-node:gcross.20091111171052.1598:Operator Site Tensor
-- @-node:gcross.20091113142219.2538:Tensors
-- @+node:gcross.20091116175016.1796:ProjectorMatrix
data ProjectorMatrix =
    NullProjectorMatrix
  | ProjectorMatrix
    {   projectorSubspaceDimension :: !Int
    ,   projectorCount :: !Int
    ,   projectorLength :: !Int
    ,   projectorReflectorCount :: !Int
    ,   projectorReflectors :: !(ComplexTensor (Int,Int))
    ,   projectorCoefficients :: !(ComplexTensor Int)
    ,   projectorSwaps :: !(StorableArray Int Int32)
    }

withPinnedProjectorMatrix ::
    ProjectorMatrix →
    (Ptr (Complex Double) → Ptr (Complex Double) → Ptr Int32 → IO a) →
    IO a
withPinnedProjectorMatrix projector_matrix thunk =
    case projector_matrix of
        NullProjectorMatrix → thunk nullPtr nullPtr nullPtr
        ProjectorMatrix _ _ _ _ reflectors coefficients swaps →
            withPinnedComplexTensor reflectors $ \p_reflector →
            withPinnedComplexTensor coefficients $ \p_coefficients →
            withStorableArray swaps $ \p_swaps →
                thunk p_reflector p_coefficients p_swaps

withNewPinnedProjectorMatrix ::
    Int →
    Int →
    (Ptr (Complex Double) → Ptr (Complex Double) → Ptr Int32 → IO (Int,a)) →
    IO (a,ProjectorMatrix)
withNewPinnedProjectorMatrix 0 _ _ = error "You are trying to get a write pointer to an empty array!"
withNewPinnedProjectorMatrix number_of_projectors projector_length thunk =
    (withNewPinnedComplexTensor (number_of_projectors,projector_length) $ \p_reflectors →
     withNewPinnedComplexTensor number_of_reflectors $ \p_coefficients → do
        swaps <- newArray_ (1,number_of_projectors `min` projector_length)
        (rank,result) <-
            withStorableArray swaps $ \p_swaps →
                thunk p_reflectors p_coefficients p_swaps
        return (result,rank,swaps)
    )
    >>=
    \(((result,rank,swaps),coefficients),reflectors) →
        return (result
               ,ProjectorMatrix
                {   projectorSubspaceDimension = rank
                ,   projectorCount = number_of_projectors
                ,   projectorLength = projector_length
                ,   projectorReflectorCount = number_of_reflectors
                ,   projectorReflectors = reflectors
                ,   projectorCoefficients = coefficients
                ,   projectorSwaps = swaps
                }
               )
  where
    number_of_reflectors = number_of_projectors `min` projector_length

projectorOrthogonalSubspaceDimension = projectorLength <^(-)^> projectorSubspaceDimension

numberOfOrthogonalProjectors NullProjectorMatrix = 0
numberOfOrthogonalProjectors projector_matrix = projectorSubspaceDimension projector_matrix
-- @-node:gcross.20091116175016.1796:ProjectorMatrix
-- @-node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091111171052.1601:Classes
-- @+node:gcross.20091111171052.1602:Connected
class Connected a b where
    (←?→) :: a → b → Int

makeConnectedTest :: String → (a → Int) → (b → Int) → a → b → Int
makeConnectedTest message fetch_left_dimension fetch_right_dimension x y =
    let d1 = fetch_left_dimension x
        d2 = fetch_right_dimension y
    in if d1 == d2 then d1 else error $ message ++ (printf " (%i != %i)\n" d1 d2)
-- @nonl
-- @-node:gcross.20091111171052.1602:Connected
-- @+node:gcross.20091116132159.1754:Creatable
class MyIx i => Creatable a i where
    withNewPinnedTensor :: i → (Ptr (Complex Double) → IO b) → IO (b,a)
-- @nonl
-- @-node:gcross.20091116132159.1754:Creatable
-- @+node:gcross.20091116132159.1750:MyIx
class Ix a => MyIx a where
    lowerBounds :: a

instance MyIx Int where { lowerBounds = 1 }
instance MyIx (Int,Int) where { lowerBounds = (1,1) }
instance MyIx (Int,Int,Int) where { lowerBounds = (1,1,1) }

newMyArray :: MyIx i => i → IO (StorableArray i (Complex Double))
newMyArray dimensions = newArray (lowerBounds,dimensions) 0

myListArray :: MyIx i => i → [Complex Double] → StorableArray i (Complex Double)
myListArray dimensions = unsafePerformIO . newListArray (lowerBounds,dimensions)
-- @nonl
-- @-node:gcross.20091116132159.1750:MyIx
-- @-node:gcross.20091111171052.1601:Classes
-- @-others
-- @-node:gcross.20091110205054.1969:@thin Implementation.hs
-- @-leo
