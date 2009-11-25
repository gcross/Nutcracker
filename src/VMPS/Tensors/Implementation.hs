-- @+leo-ver=4-thin
-- @+node:gcross.20091110205054.1969:@thin Implementation.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091111171052.1600:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091111171052.1600:<< Language extensions >>
-- @nl

module VMPS.Tensors.Implementation where

-- @<< Import needed modules >>
-- @+node:gcross.20091111171052.1599:<< Import needed modules >>
import Control.Arrow
import Control.Applicative
import Control.Applicative.Infix
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.Array.Unboxed
import Data.ByteString.Internal (ByteString(PS),fromForeignPtr,toForeignPtr,memcmp)
import Data.Complex
import Data.Function
import Data.Int

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import Text.Printf

import VMPS.Miscellaneous
import VMPS.Pauli
-- @-node:gcross.20091111171052.1599:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091111171052.1596:ComplexTensor
newtype MyIx i => ComplexTensor i = ComplexTensor { unwrapComplexTensor :: StorableArray i (Complex Double) }

instance MyIx i => Eq (ComplexTensor i) where
    (ComplexTensor x) == (ComplexTensor y) = unsafePerformIO $ do
        size1 <- fmap rangeSize (getBounds x)
        size2 <- fmap rangeSize (getBounds y)
        if size1 /= size2
            then return False
            else withStorableArray x $ \p_x ->
                 withStorableArray y $ \p_y ->
                    fmap (== 0) (memcmp (castPtr p_x) (castPtr p_y) (fromIntegral (size1*doubleComplexSizeInBytes)))

withPinnedComplexTensor :: MyIx i => ComplexTensor i -> (Ptr (Complex Double) -> IO a) -> IO a
withPinnedComplexTensor = withStorableArray . unwrapComplexTensor

withNewPinnedComplexTensor dimensions thunk = do
    storable_array <- newMyArray dimensions
    result <- withStorableArray storable_array thunk
    return (result,ComplexTensor storable_array)

complexTensorFromList :: MyIx i => i -> [Complex Double] -> ComplexTensor i
complexTensorFromList dimensions = ComplexTensor . myListArray dimensions

complexTensorFromForeignPtr :: MyIx i => i -> ForeignPtr (Complex Double) -> ComplexTensor i
complexTensorFromForeignPtr upperBounds =
    ComplexTensor
    .
    unsafePerformIO
    .
    flip unsafeForeignPtrToStorableArray (lowerBounds, upperBounds)

trivial_complex_tensor :: MyIx i => ComplexTensor i
trivial_complex_tensor = complexTensorFromList lowerBounds [1]

toListOfComplexNumbers :: MyIx i => ComplexTensor i -> [Complex Double]
toListOfComplexNumbers = unsafePerformIO . getElems . unwrapComplexTensor
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
boundaryTensorFromForeignPtr (cl,bl) =
    BoundaryTensor cl bl
    .
    complexTensorFromForeignPtr (cl,bl,bl)

sizeOfBoundaryTensor = boundaryOperatorBandwidth <^(*)^> boundaryStateBandwidth

trivial_boundary = BoundaryTensor 1 1 trivial_complex_tensor

-- @+others
-- @+node:gcross.20091112145455.1649:Left boundary
newtype LeftBoundaryTensor = LeftBoundaryTensor { unwrapLeftBoundaryTensor :: BoundaryTensor }

instance Pinnable LeftBoundaryTensor where 
    withPinnedTensor = withPinnedBoundaryTensor . unwrapLeftBoundaryTensor
    sizeOfTensor = sizeOfBoundaryTensor . unwrapLeftBoundaryTensor

instance Creatable LeftBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftBoundaryTensor)
        .
        withNewPinnedBoundaryTensor bounds
    tensorFromForeignPtr bounds =
        LeftBoundaryTensor
        .
        boundaryTensorFromForeignPtr bounds

instance Connected LeftBoundaryTensor UnnormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Left boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        (boundaryStateBandwidth . unwrapLeftBoundaryTensor)
        (stateLeftBandwidth . unwrapUnnormalizedStateSiteTensor)

instance Connected LeftBoundaryTensor LeftAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Left boundary and (left-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        (boundaryStateBandwidth . unwrapLeftBoundaryTensor)
        (stateLeftBandwidth . unwrapLeftAbsorptionNormalizedStateSiteTensor)

instance Connected LeftBoundaryTensor OperatorSiteTensor where
    (<-?->) = makeConnectedTest
        "Left boundary and operator site tensors disagree over the bandwidth dimension!"
        (boundaryOperatorBandwidth . unwrapLeftBoundaryTensor)
        operatorLeftBandwidth

trivial_left_boundary = LeftBoundaryTensor trivial_boundary
-- @-node:gcross.20091112145455.1649:Left boundary
-- @+node:gcross.20091112145455.1651:Right boundary
newtype RightBoundaryTensor = RightBoundaryTensor { unwrapRightBoundaryTensor :: BoundaryTensor }

instance Pinnable RightBoundaryTensor where 
    withPinnedTensor = withPinnedBoundaryTensor . unwrapRightBoundaryTensor
    sizeOfTensor = sizeOfBoundaryTensor . unwrapRightBoundaryTensor

instance Creatable RightBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightBoundaryTensor)
        .
        withNewPinnedBoundaryTensor bounds
    tensorFromForeignPtr bounds =
        RightBoundaryTensor
        .
        boundaryTensorFromForeignPtr bounds

instance Connected UnnormalizedStateSiteTensor RightBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        (stateRightBandwidth . unwrapUnnormalizedStateSiteTensor)
        (boundaryStateBandwidth . unwrapRightBoundaryTensor)

instance Connected RightAbsorptionNormalizedStateSiteTensor RightBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right boundary and (right-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        (stateRightBandwidth . unwrapRightAbsorptionNormalizedStateSiteTensor)
        (boundaryStateBandwidth . unwrapRightBoundaryTensor)

instance Connected OperatorSiteTensor RightBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right boundary and operator site tensors disagree over the bandwidth dimension!"
        operatorRightBandwidth
        (boundaryOperatorBandwidth . unwrapRightBoundaryTensor)

trivial_right_boundary = RightBoundaryTensor trivial_boundary
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
withNewPinnedOverlapBoundaryTensor bounds@(cl,bl) =
    fmap (second $ OverlapBoundaryTensor cl bl)
    .
    withNewPinnedComplexTensor (cl,bl)
overlapBoundaryTensorFromForeignPtr bounds@(cl,bl) =
    OverlapBoundaryTensor cl bl
    .
    complexTensorFromForeignPtr bounds

sizeOfOverlapBoundaryTensor = overlapOldStateBandwidth <^(*)^> overlapNewStateBandwidth

trivial_overlap_boundary = OverlapBoundaryTensor 1 1 trivial_complex_tensor

class OverlapBoundaryTensorClass a where
    getNewStateBandwidth :: a -> Int

-- @+others
-- @+node:gcross.20091116175016.1763:Left boundary
newtype LeftOverlapBoundaryTensor = LeftOverlapBoundaryTensor { unwrapLeftOverlapBoundaryTensor :: OverlapBoundaryTensor }

instance Pinnable LeftOverlapBoundaryTensor where 
    withPinnedTensor = withPinnedOverlapBoundaryTensor . unwrapLeftOverlapBoundaryTensor
    sizeOfTensor = sizeOfOverlapBoundaryTensor . unwrapLeftOverlapBoundaryTensor

instance Creatable LeftOverlapBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftOverlapBoundaryTensor)
        .
        withNewPinnedOverlapBoundaryTensor bounds
    tensorFromForeignPtr bounds =
        LeftOverlapBoundaryTensor
        .
        overlapBoundaryTensorFromForeignPtr bounds

instance OverlapBoundaryTensorClass LeftOverlapBoundaryTensor where
    getNewStateBandwidth = overlapNewStateBandwidth . unwrapLeftOverlapBoundaryTensor

instance Connected LeftOverlapBoundaryTensor UnnormalizedOverlapSiteTensor where
    (<-?->) = makeConnectedTest
        "Left overlap boundary and (unnormalized) overlap site tensors disagree over the bandwidth dimension!"
        (overlapOldStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

instance Connected LeftOverlapBoundaryTensor LeftAbsorptionNormalizedOverlapSiteTensor where
    (<-?->) = makeConnectedTest
        "Left overlap boundary and (left-absorption normalized) overlap site tensors disagree over the bandwidth dimension!"
        (overlapOldStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

instance Connected LeftOverlapBoundaryTensor UnnormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Left overlap boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        (overlapNewStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

instance Connected LeftOverlapBoundaryTensor LeftAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Left overlap boundary and (left-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        (overlapNewStateBandwidth . unwrapLeftOverlapBoundaryTensor)
        leftBandwidthOfState

trivial_left_overlap_boundary = LeftOverlapBoundaryTensor trivial_overlap_boundary
-- @-node:gcross.20091116175016.1763:Left boundary
-- @+node:gcross.20091116175016.1768:Right boundary
newtype RightOverlapBoundaryTensor = RightOverlapBoundaryTensor { unwrapRightOverlapBoundaryTensor :: OverlapBoundaryTensor }

instance Pinnable RightOverlapBoundaryTensor where 
    withPinnedTensor = withPinnedOverlapBoundaryTensor . unwrapRightOverlapBoundaryTensor
    sizeOfTensor = sizeOfOverlapBoundaryTensor . unwrapRightOverlapBoundaryTensor

instance Creatable RightOverlapBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightOverlapBoundaryTensor)
        .
        withNewPinnedOverlapBoundaryTensor bounds
    tensorFromForeignPtr bounds =
        RightOverlapBoundaryTensor
        .
        overlapBoundaryTensorFromForeignPtr bounds

instance OverlapBoundaryTensorClass RightOverlapBoundaryTensor where
    getNewStateBandwidth = overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor

instance Connected UnnormalizedOverlapSiteTensor RightOverlapBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right overlap boundary and (unnormalized) overlap site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapOldStateBandwidth . unwrapRightOverlapBoundaryTensor)

instance Connected RightAbsorptionNormalizedOverlapSiteTensor RightOverlapBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right overlap boundary and (Right-absorption normalized) overlap site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapOldStateBandwidth . unwrapRightOverlapBoundaryTensor)

instance Connected UnnormalizedStateSiteTensor RightOverlapBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right overlap boundary and (unnormalized) state site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor)

instance Connected RightAbsorptionNormalizedStateSiteTensor RightOverlapBoundaryTensor where
    (<-?->) = makeConnectedTest
        "Right overlap boundary and (right-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor)

trivial_right_overlap_boundary = RightOverlapBoundaryTensor trivial_overlap_boundary
-- @nonl
-- @-node:gcross.20091116175016.1768:Right boundary
-- @-others
-- @-node:gcross.20091116175016.1762:Left/Right Overlap Boundaries
-- @+node:gcross.20091111171052.1597:State Site Tensor
data StateSiteTensor = StateSiteTensor
    {   statePhysicalDimension :: !Int
    ,   stateLeftBandwidth :: !Int
    ,   stateRightBandwidth :: !Int
    ,   stateData :: !(ComplexTensor (Int,Int,Int))
    } deriving (Eq)

withPinnedStateTensor = withPinnedComplexTensor . stateData
withNewPinnedStateTensor bounds@(d,bl,br) =
    fmap (second $ StateSiteTensor d bl br)
    .
    withNewPinnedComplexTensor bounds
siteStateTensorFromForeignPtr bounds@(d,bl,br) =
    StateSiteTensor d bl br
    .
    complexTensorFromForeignPtr bounds

sizeOfStateTensor = statePhysicalDimension <^(*)^> stateLeftBandwidth <^(*)^> stateRightBandwidth

class StateSiteTensorClass a where
    leftBandwidthOfState :: a -> Int
    rightBandwidthOfState :: a -> Int
    physicalDimensionOfState :: a -> Int
    unnormalize :: a -> UnnormalizedStateSiteTensor

instance Connected LeftAbsorptionNormalizedStateSiteTensor UnnormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        leftBandwidthOfState

instance Connected UnnormalizedStateSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        leftBandwidthOfState

instance Connected UnnormalizedOverlapSiteTensor UnnormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        physicalDimensionOfState
        physicalDimensionOfState

instance Connected LeftAbsorptionNormalizedOverlapSiteTensor LeftAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        physicalDimensionOfState
        physicalDimensionOfState

instance Connected RightAbsorptionNormalizedOverlapSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "State site tensors disagree over the bandwidth dimension!"
        physicalDimensionOfState
        physicalDimensionOfState
-- @+node:gcross.20091116175016.1755:newtypes
newtype UnnormalizedStateSiteTensor = UnnormalizedStateSiteTensor
    { unwrapUnnormalizedStateSiteTensor :: StateSiteTensor } deriving (Eq)
newtype LeftAbsorptionNormalizedStateSiteTensor = LeftAbsorptionNormalizedStateSiteTensor
    { unwrapLeftAbsorptionNormalizedStateSiteTensor :: StateSiteTensor } deriving (Eq)
newtype RightAbsorptionNormalizedStateSiteTensor = RightAbsorptionNormalizedStateSiteTensor
    { unwrapRightAbsorptionNormalizedStateSiteTensor :: StateSiteTensor } deriving (Eq)
newtype UnnormalizedOverlapSiteTensor = UnnormalizedOverlapSiteTensor
    { unwrapUnnormalizedOverlapSiteTensor :: StateSiteTensor } deriving (Eq)
newtype LeftAbsorptionNormalizedOverlapSiteTensor = LeftAbsorptionNormalizedOverlapSiteTensor
    { unwrapLeftAbsorptionNormalizedOverlapSiteTensor :: StateSiteTensor } deriving (Eq)
newtype RightAbsorptionNormalizedOverlapSiteTensor = RightAbsorptionNormalizedOverlapSiteTensor
    { unwrapRightAbsorptionNormalizedOverlapSiteTensor :: StateSiteTensor } deriving (Eq)
-- @-node:gcross.20091116175016.1755:newtypes
-- @+node:gcross.20091116175016.1756:Pinnable instances
instance Pinnable UnnormalizedStateSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapUnnormalizedStateSiteTensor
    sizeOfTensor = sizeOfStateTensor . unwrapUnnormalizedStateSiteTensor
instance Pinnable LeftAbsorptionNormalizedStateSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
    sizeOfTensor = sizeOfStateTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
instance Pinnable RightAbsorptionNormalizedStateSiteTensor where 
    withPinnedTensor = withPinnedStateTensor . unwrapRightAbsorptionNormalizedStateSiteTensor
    sizeOfTensor = sizeOfStateTensor . unwrapRightAbsorptionNormalizedStateSiteTensor
instance Pinnable UnnormalizedOverlapSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapUnnormalizedOverlapSiteTensor
    sizeOfTensor = sizeOfStateTensor . unwrapUnnormalizedOverlapSiteTensor
instance Pinnable LeftAbsorptionNormalizedOverlapSiteTensor where
    withPinnedTensor = withPinnedStateTensor . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    sizeOfTensor = sizeOfStateTensor . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
instance Pinnable RightAbsorptionNormalizedOverlapSiteTensor where 
    withPinnedTensor = withPinnedStateTensor . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    sizeOfTensor = sizeOfStateTensor . unwrapRightAbsorptionNormalizedOverlapSiteTensor
-- @-node:gcross.20091116175016.1756:Pinnable instances
-- @+node:gcross.20091116175016.1757:Creatable instances
instance Creatable UnnormalizedStateSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second UnnormalizedStateSiteTensor)
        .
        withNewPinnedStateTensor bounds
    tensorFromForeignPtr bounds =
        UnnormalizedStateSiteTensor
        .
        siteStateTensorFromForeignPtr bounds
instance Creatable LeftAbsorptionNormalizedStateSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftAbsorptionNormalizedStateSiteTensor)
        .
        withNewPinnedStateTensor bounds
    tensorFromForeignPtr bounds =
        LeftAbsorptionNormalizedStateSiteTensor
        .
        siteStateTensorFromForeignPtr bounds
instance Creatable RightAbsorptionNormalizedStateSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightAbsorptionNormalizedStateSiteTensor)
        .
        withNewPinnedStateTensor bounds
    tensorFromForeignPtr bounds =
        RightAbsorptionNormalizedStateSiteTensor
        .
        siteStateTensorFromForeignPtr bounds
instance Creatable UnnormalizedOverlapSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second UnnormalizedOverlapSiteTensor)
        .
        withNewPinnedStateTensor bounds
    tensorFromForeignPtr bounds =
        UnnormalizedOverlapSiteTensor
        .
        siteStateTensorFromForeignPtr bounds
instance Creatable LeftAbsorptionNormalizedOverlapSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second LeftAbsorptionNormalizedOverlapSiteTensor)
        .
        withNewPinnedStateTensor bounds
    tensorFromForeignPtr bounds =
        LeftAbsorptionNormalizedOverlapSiteTensor
        .
        siteStateTensorFromForeignPtr bounds
instance Creatable RightAbsorptionNormalizedOverlapSiteTensor (Int,Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightAbsorptionNormalizedOverlapSiteTensor)
        .
        withNewPinnedStateTensor bounds
    tensorFromForeignPtr bounds =
        RightAbsorptionNormalizedOverlapSiteTensor
        .
        siteStateTensorFromForeignPtr bounds
-- @-node:gcross.20091116175016.1757:Creatable instances
-- @+node:gcross.20091116175016.1758:StateSiteTensorClass instances
instance StateSiteTensorClass UnnormalizedStateSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapUnnormalizedStateSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapUnnormalizedStateSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapUnnormalizedStateSiteTensor
    unnormalize = id
instance StateSiteTensorClass LeftAbsorptionNormalizedStateSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapLeftAbsorptionNormalizedStateSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapLeftAbsorptionNormalizedStateSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapLeftAbsorptionNormalizedStateSiteTensor
    unnormalize = UnnormalizedStateSiteTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
instance StateSiteTensorClass RightAbsorptionNormalizedStateSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapRightAbsorptionNormalizedStateSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapRightAbsorptionNormalizedStateSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapRightAbsorptionNormalizedStateSiteTensor
    unnormalize = UnnormalizedStateSiteTensor . unwrapRightAbsorptionNormalizedStateSiteTensor
instance StateSiteTensorClass UnnormalizedOverlapSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapUnnormalizedOverlapSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapUnnormalizedOverlapSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapUnnormalizedOverlapSiteTensor
    unnormalize = undefined
instance StateSiteTensorClass LeftAbsorptionNormalizedOverlapSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapLeftAbsorptionNormalizedOverlapSiteTensor
    unnormalize = undefined
instance StateSiteTensorClass RightAbsorptionNormalizedOverlapSiteTensor where
    leftBandwidthOfState = stateLeftBandwidth . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    rightBandwidthOfState = stateRightBandwidth . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    physicalDimensionOfState = statePhysicalDimension . unwrapRightAbsorptionNormalizedOverlapSiteTensor
    unnormalize = undefined
-- @-node:gcross.20091116175016.1758:StateSiteTensorClass instances
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
withPinnedOperatorSiteTensor :: OperatorSiteTensor -> (Int -> Ptr Int32 -> Ptr (Complex Double) -> IO a) -> IO a
withPinnedOperatorSiteTensor operator_site_tensor thunk = 
    (withStorableArray . operatorIndices) operator_site_tensor $ \p_indices ->
    (withStorableArray . operatorMatrices) operator_site_tensor $
    thunk (operatorNumberOfMatrices operator_site_tensor) p_indices . castPtr
-- @nonl
-- @-node:gcross.20091114174920.1715:withPinnedOperatorSiteTensor
-- @+node:gcross.20091114174920.1716:makeOperatorSiteTensorFromPaulis
makeOperatorSiteTensorFromPaulis ::
    Int ->
    Int ->
    PauliList ->
    OperatorSiteTensor
makeOperatorSiteTensorFromPaulis left_bandwidth right_bandwidth elements =
    let number_of_elements = length elements
    in unsafePerformIO $ do
        operator_indices <- newArray ((1,1),(number_of_elements,2)) 0
        operator_matrices <- newArray ((1,1,1),(number_of_elements,2,2)) 0
        let go :: Int -> [((Int32,Int32),(Double,Pauli))] -> IO ()
            go _ [] = return ()
            go index (((left_index,right_index),(coefficient,pauli)):rest) = do
                writeArray operator_indices (index,1) left_index
                writeArray operator_indices (index,2) right_index
                case pauli of
                    I -> do
                        writeArray operator_matrices (index,1,1) (coefficient :+ 0)
                        writeArray operator_matrices (index,2,2) (coefficient :+ 0)
                    X -> do
                        writeArray operator_matrices (index,1,2) (coefficient :+ 0)
                        writeArray operator_matrices (index,2,1) (coefficient :+ 0)
                    Y -> do
                        writeArray operator_matrices (index,1,2) (0 :+ (-coefficient))
                        writeArray operator_matrices (index,2,1) (0 :+ ( coefficient))
                    Z -> do
                        writeArray operator_matrices (index,1,1) (( coefficient) :+ 0)
                        writeArray operator_matrices (index,2,2) ((-coefficient) :+ 0)
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
-- @-node:gcross.20091114174920.1716:makeOperatorSiteTensorFromPaulis
-- @+node:gcross.20091114174920.1717:(Connected instances)
instance Connected OperatorSiteTensor UnnormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Operator and (unnormalized) state site tensors disagree over the physical dimension!"
        operatorPhysicalDimension
        physicalDimensionOfState

instance Connected OperatorSiteTensor LeftAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Operator and (unnormalized) state site tensors disagree over the physical dimension!"
        operatorPhysicalDimension
        physicalDimensionOfState

instance Connected OperatorSiteTensor RightAbsorptionNormalizedStateSiteTensor where
    (<-?->) = makeConnectedTest
        "Operator and (unnormalized) state site tensors disagree over the physical dimension!"
        operatorPhysicalDimension
        physicalDimensionOfState
-- @-node:gcross.20091114174920.1717:(Connected instances)
-- @-others

-- @-node:gcross.20091111171052.1598:Operator Site Tensor
-- @-node:gcross.20091113142219.2538:Tensors
-- @+node:gcross.20091116175016.1796:ProjectorMatrix
data ProjectorMatrix = NullProjectorMatrix | ProjectorMatrix
    {   projectorCount :: !Int
    ,   projectorMatrix :: !(ComplexTensor (Int,Int))
    }

withPinnedProjectorMatrix :: ProjectorMatrix -> (Int -> Ptr (Complex Double) -> IO a) -> IO a
withPinnedProjectorMatrix NullProjectorMatrix thunk = thunk 0 nullPtr
withPinnedProjectorMatrix (ProjectorMatrix number_of_projectors matrix) thunk =
    withPinnedComplexTensor matrix (thunk number_of_projectors)

withNewPinnedProjectorMatrix :: Int -> Int -> (Ptr (Complex Double) -> IO a) -> IO (a,ProjectorMatrix)
withNewPinnedProjectorMatrix 0 _ = error "You are trying to get a write pointer to an emptry array!"
withNewPinnedProjectorMatrix number_of_projectors projector_length =
    fmap (second $ ProjectorMatrix number_of_projectors)
    .
    withNewPinnedComplexTensor (number_of_projectors,projector_length)
-- @-node:gcross.20091116175016.1796:ProjectorMatrix
-- @-node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091111171052.1601:Classes
-- @+node:gcross.20091111171052.1602:Connected
class Connected a b where
    (<-?->) :: a -> b -> Int

makeConnectedTest :: String -> (a -> Int) -> (b -> Int) -> a -> b -> Int
makeConnectedTest message fetch_left_dimension fetch_right_dimension x y =
    let d1 = fetch_left_dimension x
        d2 = fetch_right_dimension y
    in if d1 == d2 then d1 else error $ message ++ (printf " (%i != %i)\n" d1 d2)
-- @-node:gcross.20091111171052.1602:Connected
-- @+node:gcross.20091112145455.1648:Pinnable
class Pinnable a where
    withPinnedTensor :: a -> (Ptr (Complex Double) -> IO b) -> IO b
    sizeOfTensor :: a -> Int
-- @-node:gcross.20091112145455.1648:Pinnable
-- @+node:gcross.20091116132159.1754:Creatable
class MyIx i => Creatable a i where
    withNewPinnedTensor :: i -> (Ptr (Complex Double) -> IO b) -> IO (b,a)
    tensorFromForeignPtr :: i -> ForeignPtr (Complex Double) -> a
-- @-node:gcross.20091116132159.1754:Creatable
-- @+node:gcross.20091116132159.1750:MyIx
class Ix a => MyIx a where
    lowerBounds :: a

instance MyIx Int where { lowerBounds = 1 }
instance MyIx (Int,Int) where { lowerBounds = (1,1) }
instance MyIx (Int,Int,Int) where { lowerBounds = (1,1,1) }

newMyArray :: MyIx i => i -> IO (StorableArray i (Complex Double))
newMyArray dimensions = newArray (lowerBounds,dimensions) 0

myListArray :: MyIx i => i -> [Complex Double] -> StorableArray i (Complex Double)
myListArray dimensions = unsafePerformIO . newListArray (lowerBounds,dimensions)
-- @-node:gcross.20091116132159.1750:MyIx
-- @-node:gcross.20091111171052.1601:Classes
-- @+node:gcross.20091124124443.1616:Functions
-- @+node:gcross.20091124124443.1617:withPinnedTensorAsByteString
withPinnedTensorAsByteString :: Pinnable a => a -> (ByteString -> IO b) -> IO b
withPinnedTensorAsByteString tensor thunk =
    withPinnedTensor tensor $
        newForeignPtr_ . castPtr
        >=>
        (\fptr -> thunk $ fromForeignPtr fptr 0 (doubleComplexSizeInBytes * sizeOfTensor tensor))
-- @-node:gcross.20091124124443.1617:withPinnedTensorAsByteString
-- @+node:gcross.20091124153705.1620:tensorFromByteString
tensorFromByteString :: Creatable a i => i -> ByteString -> a
tensorFromByteString upperBounds (PS fptr offset size)
    | offset /= 0
        = error "offset of ByteString being converted into tensor must be 0"
    | size /= rangeSize (lowerBounds,upperBounds) * doubleComplexSizeInBytes
        = error "size of ByteString does not match size of tensor"
    | otherwise
        = tensorFromForeignPtr upperBounds (castForeignPtr fptr)
-- @-node:gcross.20091124153705.1620:tensorFromByteString
-- @-node:gcross.20091124124443.1616:Functions
-- @-others
-- @-node:gcross.20091110205054.1969:@thin Implementation.hs
-- @-leo
