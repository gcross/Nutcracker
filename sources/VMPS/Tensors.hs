-- @+leo-ver=4-thin
-- @+node:gcross.20091116222034.1779:@thin Tensors.hs
-- @@language Haskell

module VMPS.Tensors
    (withNewPinnedTensor
    ,(←?→)
    ,leftBandwidthOfState
    ,rightBandwidthOfState
    ,physicalDimensionOfState
    ,numberOfDegreesOfFreedomInState
    ,unnormalize
    ,normOfState
    ,ProjectorMatrix(NullProjectorMatrix)
    ,withPinnedProjectorMatrix
    ,withNewPinnedProjectorMatrix
    ,StateSiteTensorClass
    ,UnnormalizedStateSiteTensor
    ,LeftAbsorptionNormalizedStateSiteTensor
    ,RightAbsorptionNormalizedStateSiteTensor
    ,UnnormalizedOverlapSiteTensor
    ,LeftAbsorptionNormalizedOverlapSiteTensor
    ,RightAbsorptionNormalizedOverlapSiteTensor
    ,LeftBoundaryTensor
    ,RightBoundaryTensor
    ,LeftOverlapBoundaryTensor
    ,RightOverlapBoundaryTensor
    ,withPinnedOperatorSiteTensor
    ,Creatable()
    ,operatorLeftBandwidth
    ,operatorRightBandwidth
    ,operatorPhysicalDimension
    ,getNewStateBandwidth
    ,trivial_left_boundary
    ,trivial_right_boundary
    ,trivial_left_overlap_boundary
    ,trivial_right_overlap_boundary
    ,OperatorSiteTensor
    ,projectorCount
    ,projectorLength
    ,projectorReflectorCount
    ,projectorOrthogonalSubspaceDimension
    ) where

import VMPS.Tensors.Implementation
-- @nonl
-- @-node:gcross.20091116222034.1779:@thin Tensors.hs
-- @-leo
