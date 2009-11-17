-- @+leo-ver=4-thin
-- @+node:gcross.20091110205054.1969:@thin Tensors.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091111171052.1600:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091111171052.1600:<< Language extensions >>
-- @nl

module VMPS.Tensors where

-- @<< Import needed modules >>
-- @+node:gcross.20091111171052.1599:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.Array.Unboxed
import Data.Complex
import Data.Function
import Data.Int

import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import Text.Printf
-- @-node:gcross.20091111171052.1599:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091111171052.1591:Types
-- @+node:gcross.20091114174920.1712:Pauli
data Pauli = I | X | Y | Z
-- @-node:gcross.20091114174920.1712:Pauli
-- @+node:gcross.20091111171052.1596:ComplexArray
newtype MyIx i => ComplexTensor i = ComplexTensor { unwrapComplexArray :: StorableArray i (Complex Double) }

withPinnedComplexTensor :: MyIx i => ComplexTensor i -> (Ptr (Complex Double) -> IO a) -> IO a
withPinnedComplexTensor = withStorableArray . unwrapComplexArray

withNewPinnedComplexTensor dimensions thunk = do
    storable_array <- newMyArray dimensions
    result <- withStorableArray storable_array thunk
    return (result,ComplexTensor storable_array)

complexTensorFromList :: MyIx i => i -> [Complex Double] -> ComplexTensor i
complexTensorFromList dimensions = ComplexTensor . myListArray dimensions

trivial_complex_tensor :: MyIx i => ComplexTensor i
trivial_complex_tensor = complexTensorFromList lowerBounds [1]

toListOfComplexNumbers :: MyIx i => ComplexTensor i -> [Complex Double]
toListOfComplexNumbers = unsafePerformIO . getElems . unwrapComplexArray
-- @-node:gcross.20091111171052.1596:ComplexArray
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

instance Creatable RightBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightBoundaryTensor)
        .
        withNewPinnedBoundaryTensor bounds

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
withNewPinnedOverlapBoundaryTensor (cl,bl) =
    fmap (second $ OverlapBoundaryTensor cl bl)
    .
    withNewPinnedComplexTensor (cl,bl)

trivial_overlap_boundary = OverlapBoundaryTensor 1 1 trivial_complex_tensor

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

instance Creatable RightOverlapBoundaryTensor (Int,Int) where
    withNewPinnedTensor bounds =
        fmap (second RightOverlapBoundaryTensor)
        .
        withNewPinnedOverlapBoundaryTensor bounds

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
        "Right overlap boundary and (Right-absorption normalized) state site tensors disagree over the bandwidth dimension!"
        rightBandwidthOfState
        (overlapNewStateBandwidth . unwrapRightOverlapBoundaryTensor)

trivial_Right_overlap_boundary = RightOverlapBoundaryTensor trivial_overlap_boundary
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
    }

withPinnedStateTensor = withPinnedComplexTensor . stateData
withNewPinnedStateTensor bounds@(d,bl,br) =
    fmap (second $ StateSiteTensor d bl br)
    .
    withNewPinnedComplexTensor bounds

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
    { unwrapUnnormalizedStateSiteTensor :: StateSiteTensor }
newtype LeftAbsorptionNormalizedStateSiteTensor = LeftAbsorptionNormalizedStateSiteTensor
    { unwrapLeftAbsorptionNormalizedStateSiteTensor :: StateSiteTensor }
newtype RightAbsorptionNormalizedStateSiteTensor = RightAbsorptionNormalizedStateSiteTensor
    { unwrapRightAbsorptionNormalizedStateSiteTensor :: StateSiteTensor }
newtype UnnormalizedOverlapSiteTensor = UnnormalizedOverlapSiteTensor
    { unwrapUnnormalizedOverlapSiteTensor :: StateSiteTensor }
newtype LeftAbsorptionNormalizedOverlapSiteTensor = LeftAbsorptionNormalizedOverlapSiteTensor
    { unwrapLeftAbsorptionNormalizedOverlapSiteTensor :: StateSiteTensor }
newtype RightAbsorptionNormalizedOverlapSiteTensor = RightAbsorptionNormalizedOverlapSiteTensor
    { unwrapRightAbsorptionNormalizedOverlapSiteTensor :: StateSiteTensor }
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
    ,   operatorIndices :: StorableArray (Int,Int) Int32
    ,   operatorMatrices :: StorableArray (Int,Int,Int) (Complex Double)
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
    [((Int32,Int32),Pauli)] ->
    OperatorSiteTensor
makeOperatorSiteTensorFromPaulis left_bandwidth right_bandwidth  elements =
    let number_of_elements = length elements
    in unsafePerformIO $ do
        operator_indices <- newArray ((1,1),(number_of_elements,2)) 0
        operator_matrices <- newArray ((1,1,1),(number_of_elements,2,2)) 0
        let go :: Int -> [((Int32,Int32),Pauli)] -> IO ()
            go _ [] = return ()
            go index (((left_index,right_index),pauli):rest) = do
                writeArray operator_indices (index,1) left_index
                writeArray operator_indices (index,2) right_index
                case pauli of
                    I -> do
                        writeArray operator_matrices (index,1,1) 1
                        writeArray operator_matrices (index,2,2) 1
                    X -> do
                        writeArray operator_matrices (index,1,2) 1
                        writeArray operator_matrices (index,2,1) 1
                    Y -> do
                        writeArray operator_matrices (index,1,2) (0 :+ (-1))
                        writeArray operator_matrices (index,2,1) (0 :+ ( 1))
                    Z -> do
                        writeArray operator_matrices (index,1,1) 1
                        writeArray operator_matrices (index,2,2) (-1)
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
-- @-node:gcross.20091112145455.1648:Pinnable
-- @+node:gcross.20091116132159.1754:Creatable
class MyIx i => Creatable a i where
    withNewPinnedTensor :: i -> (Ptr (Complex Double) -> IO b) -> IO (b,a)
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
-- @+node:gcross.20091114174920.1720:Instances
-- @+node:gcross.20091114174920.1721:Storable (Complex a)
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
-- @-node:gcross.20091114174920.1721:Storable (Complex a)
-- @-node:gcross.20091114174920.1720:Instances
-- @-others
-- @-node:gcross.20091110205054.1969:@thin Tensors.hs
-- @-leo
