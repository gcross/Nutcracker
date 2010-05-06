-- @+leo-ver=4-thin
-- @+node:gcross.20091201134050.1634:@thin Operators.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100504143114.1704:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- @nonl
-- @-node:gcross.20100504143114.1704:<< Language extensions >>
-- @nl

module VMPS.Operators where

-- @<< Import needed modules >>
-- @+node:gcross.20100504143114.1698:<< Import needed modules >>
import Control.Exception
import Control.Monad

import Data.Array.Storable
import Data.Complex
import Data.Int
import Data.Vec ((:.)(..))
import qualified Data.Vec as Vec
import Data.Vec.Nat

import Foreign.Ptr

import System.IO.Unsafe

import VMPS.States
import VMPS.Tensors
import VMPS.Tensors.Implementation (OperatorSiteTensor(..))
import VMPS.Miscellaneous
-- @-node:gcross.20100504143114.1698:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100505152919.1753:Classes
-- @+node:gcross.20100505152919.1754:OperatorDimension
class (Nat n
      ,Vec.Fold (ComplexVector n) (Data.Complex.Complex Double)
      ,Vec.Fold (ComplexMatrix n n) (ComplexVector n)
      ) => OperatorDimension n
  where
    identity :: SingleSiteOperator n
    physicalDimensionOfSingleSiteOperator :: SingleSiteOperator n -> Int
    physicalDimensionOfSingleSiteOperator _ = nat (undefined :: n)
-- @-node:gcross.20100505152919.1754:OperatorDimension
-- @-node:gcross.20100505152919.1753:Classes
-- @+node:gcross.20100505152547.1700:Type Families
-- @+node:gcross.20100505152547.1702:ComplexMatrix
type family ComplexMatrix n m
type instance ComplexMatrix N0 m = ()
type instance ComplexMatrix (Succ n) m = ComplexVector m :. ComplexMatrix n m
-- @-node:gcross.20100505152547.1702:ComplexMatrix
-- @+node:gcross.20100505152547.1701:ComplexVector
type family ComplexVector n
type instance ComplexVector N0 = ()
type instance ComplexVector (Succ n) = Complex Double :. ComplexVector n
-- @-node:gcross.20100505152547.1701:ComplexVector
-- @-node:gcross.20100505152547.1700:Type Families
-- @+node:gcross.20100504143114.1692:Types
-- @+node:gcross.20100505152547.1703:ComplexSquareMatrix
type ComplexSquareMatrix n = ComplexMatrix n n
-- @-node:gcross.20100505152547.1703:ComplexSquareMatrix
-- @+node:gcross.20100504143114.1697:SingleSiteOperator
newtype SingleSiteOperator n =
    SingleSiteOperator { unwrapSingleSiteOperator :: ComplexSquareMatrix n }

deriving instance Eq (ComplexMatrix n n) => Eq (SingleSiteOperator n)
deriving instance Show (ComplexMatrix n n) => Show (SingleSiteOperator n)
deriving instance Num (ComplexMatrix n n) => Num (SingleSiteOperator n)
-- @-node:gcross.20100504143114.1697:SingleSiteOperator
-- @+node:gcross.20100504143114.1699:OperatorSiteSpecification
type OperatorSiteSpecification n = [((Int32,Int32),SingleSiteOperator n)]
-- @-node:gcross.20100504143114.1699:OperatorSiteSpecification
-- @-node:gcross.20100504143114.1692:Types
-- @+node:gcross.20100504143114.1693:Instances
-- @+node:gcross.20100504143114.1694:Pinnable SingleSiteOperator
instance OperatorDimension n => Pinnable (SingleSiteOperator n)
  where
    withPinnedTensor single_site_operator thunk = do
        let n = physicalDimensionOfSingleSiteOperator single_site_operator
        operator <- newArray ((1,1),(n,n)) 0
        forM_ (enumerateSingleSiteOperator single_site_operator) $
            \(row,columns) -> forM_ columns $
                \(column,value) -> writeArray operator (row,column) value
        withStorableArray operator thunk
-- @-node:gcross.20100504143114.1694:Pinnable SingleSiteOperator
-- @-node:gcross.20100504143114.1693:Instances
-- @+node:gcross.20100504143114.1695:Functions
-- @+node:gcross.20100504143114.1696:(*:)
infix 5 *:
(*:) :: (Vec.Map (ComplexVector n) (ComplexVector n) (ComplexSquareMatrix n) (ComplexSquareMatrix n)
        ,Vec.Map (Complex Double) (Complex Double) (ComplexVector n) (ComplexVector n)
        ) =>
        Complex Double ->
        SingleSiteOperator n ->
        SingleSiteOperator n
(*:) c = SingleSiteOperator . Vec.map (Vec.map (c*)) . unwrapSingleSiteOperator
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
    (OperatorDimension n
    ,Pinnable a
    ,Creatable a (Int,Int,Int)
    ,StateSiteTensorClass a
    ) =>
    SingleSiteOperator n ->
    a ->
    a
applySingleSiteOperator operator state_site_tensor =
    let br = rightBandwidthOfState state_site_tensor
        bl = leftBandwidthOfState state_site_tensor
        d = physicalDimensionOfState state_site_tensor
    in assert (d == physicalDimensionOfSingleSiteOperator operator) $
       snd . unsafePerformIO $
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
-- @+node:gcross.20100505152919.1748:applySingleSiteOperators
applySingleSiteOperators :: OperatorDimension n => [SingleSiteOperator n] -> CanonicalStateRepresentation -> CanonicalStateRepresentation
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
-- @-node:gcross.20100505152919.1748:applySingleSiteOperators
-- @+node:gcross.20100505152919.1705:enumerateMatrix
enumerateMatrix :: (Vec.Fold v (Complex Double), Vec.Fold m v)  => m -> [(Int,[(Int,Complex Double)])]
enumerateMatrix = zip [1..] . map enumerateVector . Vec.toList
-- @-node:gcross.20100505152919.1705:enumerateMatrix
-- @+node:gcross.20100505152919.1704:enumerateVector
enumerateVector :: Vec.Fold v (Complex Double) => v -> [(Int,Complex Double)]
enumerateVector = zip [1..] . Vec.toList
-- @-node:gcross.20100505152919.1704:enumerateVector
-- @+node:gcross.20100505152919.1706:enumerateSingleSiteOperator
enumerateSingleSiteOperator :: OperatorDimension n => SingleSiteOperator n -> [(Int,[(Int,Complex Double)])]
enumerateSingleSiteOperator = enumerateMatrix . unwrapSingleSiteOperator
-- @-node:gcross.20100505152919.1706:enumerateSingleSiteOperator
-- @+node:gcross.20100504143114.1691:makeOperatorSiteTensorFromSpecification
makeOperatorSiteTensorFromSpecification ::
    OperatorDimension n =>
    Int ->
    Int ->
    OperatorSiteSpecification n ->
    OperatorSiteTensor
makeOperatorSiteTensorFromSpecification _ _ [] = error "The specification must be non-empty."
makeOperatorSiteTensorFromSpecification left_bandwidth right_bandwidth elements@((_,o):_) =
    let number_of_elements = length elements
    in unsafePerformIO $ do
        let n = physicalDimensionOfSingleSiteOperator o
        operator_indices <- newArray ((1,1),(number_of_elements,2)) 0
        operator_matrices <- newArray ((1,1,1),(number_of_elements,n,n)) 0
        let go _ [] = return ()
            go index (((left_index,right_index),single_site_operator):rest) = do
                writeArray operator_indices (index,1) left_index
                writeArray operator_indices (index,2) right_index
                forM_ (enumerateSingleSiteOperator single_site_operator) $
                    \(row,columns) -> forM_ columns $
                        \(column,value) -> writeArray operator_matrices (index,row,column) value
                go (index+1) rest
        go 1 elements
        return OperatorSiteTensor
            {   operatorLeftBandwidth = left_bandwidth
            ,   operatorRightBandwidth = right_bandwidth
            ,   operatorPhysicalDimension = n
            ,   operatorNumberOfMatrices = number_of_elements
            ,   operatorIndices = operator_indices
            ,   operatorMatrices = operator_matrices
            }
-- @-node:gcross.20100504143114.1691:makeOperatorSiteTensorFromSpecification
-- @-node:gcross.20100504143114.1695:Functions
-- @-others
-- @-node:gcross.20091201134050.1634:@thin Operators.hs
-- @-leo
