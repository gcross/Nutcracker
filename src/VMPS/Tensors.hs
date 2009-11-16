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
-- @+node:gcross.20091114174920.1712:Pauli
data Pauli = I | X | Y | Z
-- @-node:gcross.20091114174920.1712:Pauli
-- @+node:gcross.20091111171052.1596:ComplexArray
newtype ComplexArray = ComplexArray { unwrapComplexArray :: UArray Int Double } deriving (Show)

withPinnedComplexArray = withPinnedArray . unwrapComplexArray

unpinComplexArray = unpinArray >=> return . ComplexArray

unboxedArrayFromList list = listArray (1,length list) list

complexArrayFromList = ComplexArray . unboxedArrayFromList

trivial_complex_array = complexArrayFromList [1.0,0.0]

toListOfComplexNumbers :: ComplexArray -> [Complex Double]
toListOfComplexNumbers = go . elems . unwrapComplexArray
    where
        go [] = []
        go (a:b:rest) = (a :+ b) :go rest
        go _ = error "Can't convert an odd number of doubles to complex numbers."
-- @-node:gcross.20091111171052.1596:ComplexArray
-- @+node:gcross.20091113142219.2538:Tensors
-- @+node:gcross.20091111171052.1595:Left/Right Boundaries
data BoundaryTensor = BoundaryTensor
    {   boundaryStateBandwidth :: !Int
    ,   boundaryOperatorBandwidth :: !Int
    ,   boundaryData :: !ComplexArray
    }

withPinnedBoundaryTensor :: BoundaryTensor -> (Ptr Double -> IO a) -> IO a
withPinnedBoundaryTensor = withPinnedComplexArray . boundaryData

trivial_boundary = BoundaryTensor 1 1 trivial_complex_array

-- @<< Left boundary >>
-- @+node:gcross.20091112145455.1649:<< Left boundary >>
newtype LeftBoundaryTensor = LeftBoundaryTensor { unwrapLeftBoundaryTensor :: BoundaryTensor }

instance Pinnable LeftBoundaryTensor where { withPinnedTensor = withPinnedBoundaryTensor . unwrapLeftBoundaryTensor }

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
-- @-node:gcross.20091112145455.1649:<< Left boundary >>
-- @nl

-- @<< Right boundary >>
-- @+node:gcross.20091112145455.1651:<< Right boundary >>
newtype RightBoundaryTensor = RightBoundaryTensor { unwrapRightBoundaryTensor :: BoundaryTensor }

instance Pinnable RightBoundaryTensor where { withPinnedTensor = withPinnedBoundaryTensor . unwrapRightBoundaryTensor }

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
-- @-node:gcross.20091112145455.1651:<< Right boundary >>
-- @nl
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

newtype UnnormalizedStateSiteTensor = UnnormalizedStateSiteTensor
    { unwrapUnnormalizedStateSiteTensor :: StateSiteTensor }
newtype LeftAbsorptionNormalizedStateSiteTensor = LeftAbsorptionNormalizedStateSiteTensor
    { unwrapLeftAbsorptionNormalizedStateSiteTensor :: StateSiteTensor }
newtype RightAbsorptionNormalizedStateSiteTensor = RightAbsorptionNormalizedStateSiteTensor
    { unwrapRightAbsorptionNormalizedStateSiteTensor :: StateSiteTensor }

instance Pinnable UnnormalizedStateSiteTensor where
    withPinnedTensor = withPinnedStateSiteTensor . unwrapUnnormalizedStateSiteTensor
instance Pinnable LeftAbsorptionNormalizedStateSiteTensor where
    withPinnedTensor = withPinnedStateSiteTensor . unwrapLeftAbsorptionNormalizedStateSiteTensor
instance Pinnable RightAbsorptionNormalizedStateSiteTensor where 
    withPinnedTensor = withPinnedStateSiteTensor . unwrapRightAbsorptionNormalizedStateSiteTensor

class StateSiteTensorClass a where
    leftBandwidthOfState :: a -> Int
    rightBandwidthOfState :: a -> Int
    physicalDimensionOfState :: a -> Int
    unnormalize :: a -> UnnormalizedStateSiteTensor

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
withPinnedOperatorSiteTensor :: OperatorSiteTensor -> (Int -> Ptr Int32 -> Ptr Double -> IO a) -> IO a
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
    withPinnedTensor :: a -> (Ptr Double -> IO b) -> IO b
-- @-node:gcross.20091112145455.1648:Pinnable
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
