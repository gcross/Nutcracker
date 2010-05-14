-- @+leo-ver=4-thin
-- @+node:gcross.20091113142219.1659:@thin EnergyMinimizationChain.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113142219.1661:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
-- @-node:gcross.20091113142219.1661:<< Language extensions >>
-- @nl

module VMPS.EnergyMinimizationChain where

-- @<< Import needed modules >>
-- @+node:gcross.20091113142219.1663:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.Complex
import Data.Either.Unwrap
import Data.Function
import Data.List
import Data.Typeable

import Text.Printf

import VMPS.Miscellaneous
import VMPS.States
import VMPS.Tensors
import VMPS.Tensors.Implementation
    (RightAbsorptionNormalizedStateSiteTensor(..)
    ,unwrapUnnormalizedStateSiteTensor
    )
import VMPS.Wrappers

import Debug.Trace
-- @-node:gcross.20091113142219.1663:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100506200958.2694:Exceptions
-- @+node:gcross.20100512151146.1738:SanityCheckFailed
data SanityCheckFailed =
    EnergyChangedAfterMoveError (Either () ()) Int Double Double
  | EnergyChangedAfterBandwidthIncreaseError Int Int Double Double
  deriving Typeable

instance Show SanityCheckFailed where
    show (EnergyChangedAfterMoveError sweep_direction old_site_number old_energy new_energy) =
        printf "SanityCheckFailed:  When moving from site %i to site %i, the energy changed from %f to %f."
            old_site_number
            new_site_number
            old_energy
            new_energy
      where
        new_site_number =
            case sweep_direction of
                Left () -> old_site_number-1
                Right () -> old_site_number+1
    show (EnergyChangedAfterBandwidthIncreaseError old_bandwidth new_bandwidth old_energy new_energy) =
        printf "SanityCheckFailed:  When increasing the bandwidth from %i to %i, the energy changed from %f to %f."
            old_bandwidth
            new_bandwidth
            old_energy
            new_energy

instance Exception SanityCheckFailed
-- @-node:gcross.20100512151146.1738:SanityCheckFailed
-- @-node:gcross.20100506200958.2694:Exceptions
-- @+node:gcross.20091113142219.1664:Types
-- @+node:gcross.20091117140132.1795:OverlapTensorTrio
data OverlapTensorTrio = 
    LeftSiteOverlapTensorTrio
    {   leftOverlapUnnormalizedTensor :: !UnnormalizedOverlapSiteTensor
    ,   leftOverlapLeftAbsorptionNormalizedTensor :: !LeftAbsorptionNormalizedOverlapSiteTensor
    } |
    MiddleSiteOverlapTensorTrio
    {   middleOverlapUnnormalizedTensor :: !UnnormalizedOverlapSiteTensor
    ,   middleOverlapLeftAbsorptionNormalizedTensor :: !LeftAbsorptionNormalizedOverlapSiteTensor
    ,   middleOverlapRightAbsorptionNormalizedTensor :: !RightAbsorptionNormalizedOverlapSiteTensor
    } |
    RightSiteOverlapTensorTrio
    {   rightOverlapUnnormalizedTensor :: !UnnormalizedOverlapSiteTensor
    ,   rightOverlapRightAbsorptionNormalizedTensor :: !RightAbsorptionNormalizedOverlapSiteTensor
    }

overlapUnnormalizedTensor (LeftSiteOverlapTensorTrio x _) = x
overlapUnnormalizedTensor (MiddleSiteOverlapTensorTrio x _ _) = x
overlapUnnormalizedTensor (RightSiteOverlapTensorTrio x _) = x

overlapLeftAbsorptionNormalizedTensor (LeftSiteOverlapTensorTrio _ x) = x
overlapLeftAbsorptionNormalizedTensor (MiddleSiteOverlapTensorTrio _ x _) = x
overlapLeftAbsorptionNormalizedTensor (RightSiteOverlapTensorTrio _ _) = error "The right-most site does not have a left-absorption normalized overlap tensor!"

overlapRightAbsorptionNormalizedTensor (LeftSiteOverlapTensorTrio _ _) = error "The left-most site does not have a right-absorption normalized overlap tensor!"
overlapRightAbsorptionNormalizedTensor (MiddleSiteOverlapTensorTrio _ _ x) = x
overlapRightAbsorptionNormalizedTensor (RightSiteOverlapTensorTrio _ x) = x
-- @-node:gcross.20091117140132.1795:OverlapTensorTrio
-- @+node:gcross.20091115105949.1734:Neighbor
data LeftNeighbor = LeftNeighbor
    {   leftNeighborBoundary :: !LeftBoundaryTensor
    ,   leftNeighborState :: !LeftAbsorptionNormalizedStateSiteTensor
    ,   leftNeighborOperator :: !OperatorSiteTensor
    ,   leftNeighborOverlapBoundaries :: ![LeftOverlapBoundaryTensor]
    ,   leftNeighborOverlapTrios :: ![OverlapTensorTrio]
    }

data RightNeighbor = RightNeighbor
    {   rightNeighborBoundary :: !RightBoundaryTensor
    ,   rightNeighborState :: !RightAbsorptionNormalizedStateSiteTensor
    ,   rightNeighborOperator :: !OperatorSiteTensor
    ,   rightNeighborOverlapBoundaries :: ![RightOverlapBoundaryTensor]
    ,   rightNeighborOverlapTrios :: ![OverlapTensorTrio]
    }

-- @+others
-- @+node:gcross.20091117140132.1792:absorbIntoNew___Neighbor
absorbIntoNewRightNeighbor ::
    RightBoundaryTensor ->
    [RightOverlapBoundaryTensor] ->
    RightAbsorptionNormalizedStateSiteTensor ->
    OperatorSiteTensor ->
    [OverlapTensorTrio] ->
    (RightBoundaryTensor,[RightOverlapBoundaryTensor],RightNeighbor)

absorbIntoNewRightNeighbor
    right_boundary_tensor
    right_overlap_boundary_tensors
    state_site_tensor
    operator_site_tensor
    overlap_tensor_trios
    =
    (contractSOSRight right_boundary_tensor state_site_tensor operator_site_tensor
    ,map (
        uncurry (contractSSRight state_site_tensor)
        .
        (second overlapRightAbsorptionNormalizedTensor)
     ) $ zip right_overlap_boundary_tensors overlap_tensor_trios
    ,RightNeighbor
        right_boundary_tensor
        state_site_tensor
        operator_site_tensor
        right_overlap_boundary_tensors
        overlap_tensor_trios
    )

absorbIntoNewLeftNeighbor ::
    LeftBoundaryTensor ->
    [LeftOverlapBoundaryTensor] ->
    LeftAbsorptionNormalizedStateSiteTensor ->
    OperatorSiteTensor ->
    [OverlapTensorTrio] ->
    (LeftBoundaryTensor,[LeftOverlapBoundaryTensor],LeftNeighbor)

absorbIntoNewLeftNeighbor
    left_boundary_tensor
    left_overlap_boundary_tensors
    state_site_tensor
    operator_site_tensor
    overlap_tensor_trios
    =
    (contractSOSLeft left_boundary_tensor state_site_tensor operator_site_tensor
    ,map (
        uncurry (contractSSLeft state_site_tensor)
        .
        (second overlapLeftAbsorptionNormalizedTensor)
     ) $ zip left_overlap_boundary_tensors overlap_tensor_trios
    ,LeftNeighbor
        left_boundary_tensor
        state_site_tensor
        operator_site_tensor
        left_overlap_boundary_tensors
        overlap_tensor_trios
    )
-- @-node:gcross.20091117140132.1792:absorbIntoNew___Neighbor
-- @+node:gcross.20091117140132.1794:prepareAndAbsorbIntoNew___Neighbor
prepareAndAbsorbIntoNewRightNeighbor ::
    LeftNeighbor ->
    RightBoundaryTensor ->
    [RightOverlapBoundaryTensor] ->
    UnnormalizedStateSiteTensor ->
    OperatorSiteTensor ->
    [OverlapTensorTrio] ->
     (LeftBoundaryTensor
     ,[LeftOverlapBoundaryTensor]
     ,RightBoundaryTensor
     ,[RightOverlapBoundaryTensor]
     ,UnnormalizedStateSiteTensor
     ,OperatorSiteTensor
     ,[OverlapTensorTrio]
     ,RightNeighbor
     )

prepareAndAbsorbIntoNewRightNeighbor
    old_left_neighbor
    old_right_boundary_tensor
    old_right_overlap_boundary_tensors
    old_state_site_tensor
    old_operator_site_tensor
    old_overlap_tensor_trios
    =
    let (new_state_site_tensor,normalized_state_site_tensor) =
            makeNormalizedForAbsorbingRight (leftNeighborState old_left_neighbor) old_state_site_tensor
        (new_right_boundary_tensor,new_right_overlap_boundary_tensors,new_right_neighbor) =
            absorbIntoNewRightNeighbor
                old_right_boundary_tensor
                old_right_overlap_boundary_tensors
                normalized_state_site_tensor
                old_operator_site_tensor
                old_overlap_tensor_trios
    in
        ((leftNeighborBoundary old_left_neighbor)
        ,(leftNeighborOverlapBoundaries old_left_neighbor)
        ,new_right_boundary_tensor
        ,new_right_overlap_boundary_tensors
        ,new_state_site_tensor
        ,(leftNeighborOperator old_left_neighbor)
        ,(leftNeighborOverlapTrios old_left_neighbor)
        ,new_right_neighbor
        )

prepareAndAbsorbIntoNewLeftNeighbor ::
    RightNeighbor ->
    LeftBoundaryTensor ->
    [LeftOverlapBoundaryTensor] ->
    UnnormalizedStateSiteTensor ->
    OperatorSiteTensor ->
    [OverlapTensorTrio] ->
     (LeftBoundaryTensor
     ,[LeftOverlapBoundaryTensor]
     ,RightBoundaryTensor
     ,[RightOverlapBoundaryTensor]
     ,UnnormalizedStateSiteTensor
     ,OperatorSiteTensor
     ,[OverlapTensorTrio]
     ,LeftNeighbor
     )

prepareAndAbsorbIntoNewLeftNeighbor
    old_right_neighbor
    old_left_boundary_tensor
    old_left_overlap_boundary_tensors
    old_state_site_tensor
    old_operator_site_tensor
    old_overlap_tensor_trios
    =
    let (normalized_state_site_tensor,new_state_site_tensor) =
            makeNormalizedForAbsorbingLeft old_state_site_tensor (rightNeighborState old_right_neighbor)
        (new_left_boundary_tensor,new_left_overlap_boundary_tensors,new_left_neighbor) =
            absorbIntoNewLeftNeighbor
                old_left_boundary_tensor
                old_left_overlap_boundary_tensors
                normalized_state_site_tensor
                old_operator_site_tensor
                old_overlap_tensor_trios
    in
        (new_left_boundary_tensor
        ,new_left_overlap_boundary_tensors
        ,(rightNeighborBoundary old_right_neighbor)
        ,(rightNeighborOverlapBoundaries old_right_neighbor)
        ,new_state_site_tensor
        ,(rightNeighborOperator old_right_neighbor)
        ,(rightNeighborOverlapTrios old_right_neighbor)
        ,new_left_neighbor
        )
-- @-node:gcross.20091117140132.1794:prepareAndAbsorbIntoNew___Neighbor
-- @-others
-- @-node:gcross.20091115105949.1734:Neighbor
-- @+node:gcross.20091113142219.1665:EnergyMinimizationChain
data EnergyMinimizationChain = EnergyMinimizationChain
    {   siteLeftBoundaryTensor :: !LeftBoundaryTensor
    ,   siteLeftOverlapBoundaryTensors :: ![LeftOverlapBoundaryTensor]
    ,   siteStateTensor :: !UnnormalizedStateSiteTensor
    ,   siteHamiltonianTensor :: !OperatorSiteTensor
    ,   siteOverlapTrios :: ![OverlapTensorTrio]
    ,   siteRightBoundaryTensor :: !RightBoundaryTensor
    ,   siteRightOverlapBoundaryTensors :: ![RightOverlapBoundaryTensor]
    ,   siteLeftNeighbors :: ![LeftNeighbor]
    ,   siteRightNeighbors :: ![RightNeighbor]
    ,   siteNumber :: !Int
    ,   chainNumberOfSites :: !Int
    ,   chainEnergy :: Double
    }
-- @-node:gcross.20091113142219.1665:EnergyMinimizationChain
-- @-node:gcross.20091113142219.1664:Types
-- @+node:gcross.20091116222034.2374:Utility Functions
-- @+node:gcross.20091113142219.1699:computeBandwidthDimensionSequence
computeBandwidthDimensionSequence :: Int -> [Int] -> [Int]
computeBandwidthDimensionSequence requested_bandwidth_dimension physical_dimensions =
    let computeSequence physical_dimensions = 1:go 1 physical_dimensions
          where
            go :: Int -> [Int] -> [Int]
            go _ [] = []
            go last_bandwidth_dimension (current_physical_dimension:remaining_physical_dimensions)
               | current_bandwidth_dimension >= requested_bandwidth_dimension
                = []
               | otherwise
                = current_bandwidth_dimension : go current_bandwidth_dimension remaining_physical_dimensions
              where
                current_bandwidth_dimension = last_bandwidth_dimension*current_physical_dimension
        left_sequence = computeSequence physical_dimensions
        right_sequence = reverse . computeSequence . reverse $ physical_dimensions
        requested_length = length physical_dimensions + 1
        minimal_length_without_center = length left_sequence + length right_sequence
    in if minimal_length_without_center >= requested_length
        then error "The supplied bandwidth dimension is too large for the given physical dimension and number of sites."
        else
            left_sequence
            ++
            replicate (requested_length - minimal_length_without_center) requested_bandwidth_dimension
            ++
            right_sequence
-- @-node:gcross.20091113142219.1699:computeBandwidthDimensionSequence
-- @+node:gcross.20091113142219.2519:computeSiteDimensionSequence
computeSiteDimensionSequence :: Int -> [Int] -> [(Int,Int,Int)]
computeSiteDimensionSequence requested_bandwidth_dimension physical_dimensions =
    zip3
        physical_dimensions
        bandwidth_dimension_sequence
        (tail bandwidth_dimension_sequence)
  where
    bandwidth_dimension_sequence =
        computeBandwidthDimensionSequence
            requested_bandwidth_dimension
            physical_dimensions
-- @-node:gcross.20091113142219.2519:computeSiteDimensionSequence
-- @+node:gcross.20091120112621.1590:makeConfiguration
makeConfiguration :: [OperatorSiteTensor] -> [[OverlapTensorTrio]] -> [(OperatorSiteTensor,[OverlapTensorTrio])]
makeConfiguration operator_site_tensors [] = zip operator_site_tensors (repeat [])
makeConfiguration operator_site_tensors projectors = zip operator_site_tensors . transpose $ projectors
-- @-node:gcross.20091120112621.1590:makeConfiguration
-- @+node:gcross.20100512154636.1738:throwIfEnergyChanged
throwIfEnergyChanged ::
    Double ->
    EnergyMinimizationChain ->
    (EnergyMinimizationChain -> EnergyMinimizationChain -> SanityCheckFailed) ->
    EnergyMinimizationChain ->
    EnergyMinimizationChain
throwIfEnergyChanged tolerance old_chain createException new_chain
 | abs (chainEnergy new_chain - chainEnergy old_chain) > tolerance
    = throw $ createException old_chain new_chain
 | otherwise
    = new_chain
-- @-node:gcross.20100512154636.1738:throwIfEnergyChanged
-- @-node:gcross.20091116222034.2374:Utility Functions
-- @+node:gcross.20091113142219.1678:Chain Functions
-- @+node:gcross.20091113142219.1684:activateLeftNeighbor
activateLeftNeighbor :: EnergyMinimizationChain -> EnergyMinimizationChain
activateLeftNeighbor EnergyMinimizationChain { siteLeftNeighbors = [] } =
    error "Chain is already at its leftmost site!"
activateLeftNeighbor old_chain =
    let (   new_left_boundary_tensor
           ,new_left_overlap_boundary_tensors
           ,new_right_boundary_tensor
           ,new_right_overlap_boundary_tensors
           ,new_state_site_tensor
           ,new_hamiltonian_site_tensor
           ,new_overlap_trios
           ,new_right_neighbor
           ) = prepareAndAbsorbIntoNewRightNeighbor
                    (head . siteLeftNeighbors $ old_chain)
                    (siteRightBoundaryTensor old_chain)
                    (siteRightOverlapBoundaryTensors old_chain)
                    (siteStateTensor old_chain)
                    (siteHamiltonianTensor old_chain)
                    (siteOverlapTrios old_chain)
        new_chain = EnergyMinimizationChain
                {   siteLeftBoundaryTensor = new_left_boundary_tensor
                ,   siteLeftOverlapBoundaryTensors = new_left_overlap_boundary_tensors
                ,   siteStateTensor = new_state_site_tensor
                ,   siteHamiltonianTensor = new_hamiltonian_site_tensor
                ,   siteOverlapTrios = new_overlap_trios
                ,   siteRightBoundaryTensor = new_right_boundary_tensor
                ,   siteRightOverlapBoundaryTensors = new_right_overlap_boundary_tensors
                ,   siteLeftNeighbors = tail . siteLeftNeighbors $ old_chain
                ,   siteRightNeighbors = (new_right_neighbor:) . siteRightNeighbors $ old_chain
                ,   siteNumber = (siteNumber old_chain) - 1
                ,   chainNumberOfSites = chainNumberOfSites old_chain
                ,   chainEnergy = computeEnergy new_chain
                }
    in new_chain

-- @-node:gcross.20091113142219.1684:activateLeftNeighbor
-- @+node:gcross.20100512151146.1739:activateLeftNeighborWithSanityCheck
activateLeftNeighborWithSanityCheck = checkSanityAfterActivatingNeighbor activateLeftNeighbor (Left ())
-- @-node:gcross.20100512151146.1739:activateLeftNeighborWithSanityCheck
-- @+node:gcross.20091113142219.1686:activateRightNeighbor
activateRightNeighbor :: EnergyMinimizationChain -> EnergyMinimizationChain
activateRightNeighbor EnergyMinimizationChain { siteRightNeighbors = [] } =
    error "Chain is already at its rightmost site!"
activateRightNeighbor old_chain =
    let (   new_left_boundary_tensor
           ,new_left_overlap_boundary_tensors
           ,new_right_boundary_tensor
           ,new_right_overlap_boundary_tensors
           ,new_state_site_tensor
           ,new_hamiltonian_site_tensor
           ,new_overlap_trios
           ,new_left_neighbor
           ) = prepareAndAbsorbIntoNewLeftNeighbor
                    (head . siteRightNeighbors $ old_chain)
                    (siteLeftBoundaryTensor old_chain)
                    (siteLeftOverlapBoundaryTensors old_chain)
                    (siteStateTensor old_chain)
                    (siteHamiltonianTensor old_chain)
                    (siteOverlapTrios old_chain)
        new_chain = EnergyMinimizationChain
                {   siteLeftBoundaryTensor = new_left_boundary_tensor
                ,   siteLeftOverlapBoundaryTensors = new_left_overlap_boundary_tensors
                ,   siteStateTensor = new_state_site_tensor
                ,   siteHamiltonianTensor = new_hamiltonian_site_tensor
                ,   siteOverlapTrios = new_overlap_trios
                ,   siteRightBoundaryTensor = new_right_boundary_tensor
                ,   siteRightOverlapBoundaryTensors = new_right_overlap_boundary_tensors
                ,   siteLeftNeighbors = (new_left_neighbor:) . siteLeftNeighbors $ old_chain
                ,   siteRightNeighbors = tail . siteRightNeighbors $ old_chain
                ,   siteNumber = (siteNumber old_chain) + 1
                ,   chainNumberOfSites = chainNumberOfSites old_chain
                ,   chainEnergy = computeEnergy new_chain
                }
    in new_chain
-- @-node:gcross.20091113142219.1686:activateRightNeighbor
-- @+node:gcross.20100512151146.1743:activateRightNeighborWithSanityCheck
activateRightNeighborWithSanityCheck = checkSanityAfterActivatingNeighbor activateRightNeighbor (Right ())
-- @-node:gcross.20100512151146.1743:activateRightNeighborWithSanityCheck
-- @+node:gcross.20100513131210.1743:chainNumberOfProjectors
chainNumberOfProjectors = length . siteOverlapTrios
-- @-node:gcross.20100513131210.1743:chainNumberOfProjectors
-- @+node:gcross.20100503130440.1689:chainPhysicalDimensions
chainPhysicalDimensions :: EnergyMinimizationChain -> [Int]
chainPhysicalDimensions chain =
    reverse (map (physicalDimensionOfState . leftNeighborState) (siteLeftNeighbors chain))
    ++
    [physicalDimensionOfState . siteStateTensor $ chain]
    ++
    map (physicalDimensionOfState . rightNeighborState) (siteRightNeighbors chain)
-- @-node:gcross.20100503130440.1689:chainPhysicalDimensions
-- @+node:gcross.20100512151146.1740:checkSanityAfterActivatingNeighbor
checkSanityAfterActivatingNeighbor activateNeighbor direction tolerance old_chain =
    throwIfEnergyChanged tolerance old_chain (
        \old_chain new_chain ->
            EnergyChangedAfterMoveError
                direction
                (siteNumber old_chain)
                (chainEnergy old_chain)
                (chainEnergy new_chain)
    )
    .
    activateNeighbor
    $
    old_chain
-- @-node:gcross.20100512151146.1740:checkSanityAfterActivatingNeighbor
-- @+node:gcross.20091113142219.1679:computeEnergy
computeEnergy :: EnergyMinimizationChain -> Double
computeEnergy EnergyMinimizationChain
            {   siteLeftBoundaryTensor = left_boundary_tensor
            ,   siteStateTensor = state_site_tensor
            ,   siteHamiltonianTensor = operator_site_tensor
            ,   siteRightBoundaryTensor = right_boundary_tensor
            } = 
    let expectation = computeExpectation left_boundary_tensor state_site_tensor operator_site_tensor right_boundary_tensor
    in assert (imagPart expectation ~= 0) (realPart expectation)
-- @-node:gcross.20091113142219.1679:computeEnergy
-- @+node:gcross.20091117140132.1798:computeOverlapTriosFromStateTensors
computeOverlapTriosFromCanonicalStateRepresentation :: CanonicalStateRepresentation -> [OverlapTensorTrio]
computeOverlapTriosFromCanonicalStateRepresentation (CanonicalStateRepresentation _ _ []) =
    error "There needs to be more than one site to use this function!"
computeOverlapTriosFromCanonicalStateRepresentation
    (CanonicalStateRepresentation
        _
        unnormalized_state_first_site_tensor
        (right_normalized_state_second_site_tensor:right_normalized_state_rest_site_tensors)
    ) =
    let ( left_normalized_overlap_first_site_tensor
         ,unnormalized_overlap_first_site_tensor
         ,unnormalized_state_second_site_tensor
         ,right_normalized_overlap_second_site_tensor
         ) = makeAndNormalizeOverlapSiteTensors unnormalized_state_first_site_tensor
                                                right_normalized_state_second_site_tensor
    in (LeftSiteOverlapTensorTrio
         unnormalized_overlap_first_site_tensor
         left_normalized_overlap_first_site_tensor
        ):go unnormalized_state_second_site_tensor
             right_normalized_state_second_site_tensor
             right_normalized_overlap_second_site_tensor
             right_normalized_state_rest_site_tensors
  where
    go :: UnnormalizedStateSiteTensor ->
          RightAbsorptionNormalizedStateSiteTensor ->
          RightAbsorptionNormalizedOverlapSiteTensor ->
          [RightAbsorptionNormalizedStateSiteTensor] ->
          [OverlapTensorTrio]
    go unnormalized_state_last_site_tensor
       _
       right_normalized_overlap_last_site_tensor
       []
        = let unnormalized_overlap_last_site_tensor = makeOverlapSiteTensor unnormalized_state_last_site_tensor
          in [RightSiteOverlapTensorTrio unnormalized_overlap_last_site_tensor right_normalized_overlap_last_site_tensor]
    go unnormalized_state_current_site_tensor
       right_normalized_state_current_site_tensor
       right_normalized_overlap_current_site_tensor
       (right_normalized_state_next_site_tensor:normalized_state_rest_site_tensors)
        = let ( left_normalized_overlap_current_site_tensor
               ,unnormalized_overlap_current_site_tensor
               ,unnormalized_state_next_site_tensor
               ,right_normalized_overlap_next_site_tensor
               ) = makeAndNormalizeOverlapSiteTensors unnormalized_state_current_site_tensor
                                                      right_normalized_state_next_site_tensor
          in (MiddleSiteOverlapTensorTrio
                unnormalized_overlap_current_site_tensor
                left_normalized_overlap_current_site_tensor
                right_normalized_overlap_current_site_tensor
              ):go unnormalized_state_next_site_tensor
                   right_normalized_state_next_site_tensor
                   right_normalized_overlap_next_site_tensor
                   normalized_state_rest_site_tensors
-- @-node:gcross.20091117140132.1798:computeOverlapTriosFromStateTensors
-- @+node:gcross.20091117140132.1797:computeProjectorMatrix
computeProjectorMatrix :: EnergyMinimizationChain -> ProjectorMatrix
computeProjectorMatrix chain =
    formProjectorMatrix $
        zip3 (siteLeftOverlapBoundaryTensors chain)
             (siteRightOverlapBoundaryTensors chain)
             (map overlapUnnormalizedTensor . siteOverlapTrios $ chain)
-- @-node:gcross.20091117140132.1797:computeProjectorMatrix
-- @+node:gcross.20091117140132.1796:generateRandomizedChain
generateRandomizedChain :: Int -> [OperatorSiteTensor] -> IO (EnergyMinimizationChain)
generateRandomizedChain requested_bandwidth_dimension =
    generateRandomizedChainWithOverlaps requested_bandwidth_dimension . flip zip (repeat [])
-- @-node:gcross.20091117140132.1796:generateRandomizedChain
-- @+node:gcross.20091113142219.2535:generateRandomizedChainWithOverlaps
generateRandomizedChainWithOverlaps :: Int -> [(OperatorSiteTensor,[OverlapTensorTrio])] -> IO (EnergyMinimizationChain)
generateRandomizedChainWithOverlaps _ [] = error "Must have at least one operator site tensor!"
generateRandomizedChainWithOverlaps requested_bandwidth_dimension operator_site_tensors =
    let number_of_sites = length operator_site_tensors
        number_of_projectors = length . snd . head $ operator_site_tensors
        sequence_of_site_dimensions =
            computeSiteDimensionSequence requested_bandwidth_dimension
            .
            map (operatorPhysicalDimension . fst)
            $
            operator_site_tensors
        go final_right_boundary
           final_right_overlap_boundaries
           final_neighbors
           []
           = return (final_neighbors,final_right_boundary,final_right_overlap_boundaries)
        go right_boundary
           right_overlap_boundaries
           current_neighbors
           (((d,bl,br),(operator_site_tensor,overlap_trios)):remaining) 
           = generateRandomizedStateSiteTensor d bl br
             >>=
             \site_state_tensor ->
                 let (new_right_boundary,new_right_overlap_boundaries,new_neighbor) =
                         absorbIntoNewRightNeighbor
                             right_boundary
                             right_overlap_boundaries
                             site_state_tensor
                             operator_site_tensor
                             overlap_trios
                 in go new_right_boundary
                       new_right_overlap_boundaries
                       (new_neighbor:current_neighbors)
                       remaining
    in do
        (right_neighbors,right_boundary,right_overlap_boundaries) <-
            go trivial_right_boundary
               (replicate number_of_projectors trivial_right_overlap_boundary)
               []
               (reverse . tail $ zip sequence_of_site_dimensions operator_site_tensors)
        unnormalized_state_site_tensor <-
            let (d,bl,br) = head sequence_of_site_dimensions
            in generateRandomizedStateSiteTensor d bl br
        return $
            let left_overlap_boundaries = (replicate number_of_projectors trivial_left_overlap_boundary)
                overlap_trios = snd . head $ operator_site_tensors
                projector_matrix = formProjectorMatrix $
                                        zip3 left_overlap_boundaries
                                             right_overlap_boundaries
                                             (map overlapUnnormalizedTensor overlap_trios)
                chain = EnergyMinimizationChain
                    {   siteLeftBoundaryTensor = trivial_left_boundary
                    ,   siteLeftOverlapBoundaryTensors = left_overlap_boundaries
                    ,   siteStateTensor = applyProjectorMatrix projector_matrix unnormalized_state_site_tensor
                    ,   siteHamiltonianTensor = fst . head $ operator_site_tensors
                    ,   siteOverlapTrios = overlap_trios
                    ,   siteRightBoundaryTensor = right_boundary
                    ,   siteRightOverlapBoundaryTensors = right_overlap_boundaries
                    ,   siteLeftNeighbors = []
                    ,   siteRightNeighbors = right_neighbors
                    ,   siteNumber = 1
                    ,   chainNumberOfSites = number_of_sites
                    ,   chainEnergy = computeEnergy chain
                    }
            in chain
-- @-node:gcross.20091113142219.2535:generateRandomizedChainWithOverlaps
-- @+node:gcross.20091119150241.1849:getCanonicalStateRepresentation
getCanonicalStateRepresentation :: EnergyMinimizationChain -> CanonicalStateRepresentation
getCanonicalStateRepresentation chain
    | siteNumber chain > 1
        = error "The chain must be at the first site in order to extract the canonical state representation."
    | otherwise
        = CanonicalStateRepresentation
            (chainNumberOfSites chain)
            (siteStateTensor chain)
            (map rightNeighborState . siteRightNeighbors $ chain)
-- @-node:gcross.20091119150241.1849:getCanonicalStateRepresentation
-- @+node:gcross.20091115105949.1744:increaseChainBandwidth
increaseChainBandwidth :: Int -> EnergyMinimizationChain -> IO EnergyMinimizationChain
increaseChainBandwidth
    new_bandwidth
    chain@
    EnergyMinimizationChain
    {   siteRightNeighbors = neighbors
    ,   siteLeftNeighbors = []
    ,   chainNumberOfSites = number_of_sites
    }
 = (flip go1) []
    .
    zip (
        reverse
        .
        ((head neighbors) { 
            rightNeighborState = 
                (
                    RightAbsorptionNormalizedStateSiteTensor
                    .
                    unwrapUnnormalizedStateSiteTensor
                    .
                    siteStateTensor
                    $
                    chain
                )
        }:)
        $
        neighbors
    )
    .
    tail
    .
    reverse
    .
    computeBandwidthDimensionSequence new_bandwidth
    .
    chainPhysicalDimensions
    $
    chain
  where
    go1 ::
        [(RightNeighbor,Int)] ->
        [RightNeighbor] ->
        IO EnergyMinimizationChain
    go1 [] _ = error "The chain already had the desired bandwidth!"
    go1 ((current_neighbor,desired_bandwidth):rest_neighbors) new_neighbors
        | neighbor_bandwidth > desired_bandwidth
            = error "The desired bandwidth is *less* than the current bandwidth!"
        | neighbor_bandwidth == desired_bandwidth
            = go1 rest_neighbors (current_neighbor:new_neighbors)
        | otherwise
            = go2
                (rightNeighborBoundary current_neighbor)
                (rightNeighborOverlapBoundaries current_neighbor)
                (unnormalize . rightNeighborState $ current_neighbor)
                (rightNeighborOperator current_neighbor)
                (rightNeighborOverlapTrios current_neighbor)
                desired_bandwidth
                rest_neighbors
                new_neighbors
      where neighbor_bandwidth = leftBandwidthOfState . rightNeighborState $ current_neighbor

    go2 ::
        RightBoundaryTensor ->
        [RightOverlapBoundaryTensor] ->
        UnnormalizedStateSiteTensor ->
        OperatorSiteTensor ->
        [OverlapTensorTrio] ->
        Int ->
        [(RightNeighbor,Int)] ->
        [RightNeighbor] ->
        IO EnergyMinimizationChain
    go2 right_boundary
        right_overlap_boundaries
        state_site_tensor
        _
        _
        _
        []
        new_right_neighbors
        =
        return $
            let new_chain = chain
                    {   siteRightBoundaryTensor = right_boundary
                    ,   siteRightOverlapBoundaryTensors = right_overlap_boundaries
                    ,   siteStateTensor = state_site_tensor
                    ,   siteRightNeighbors = new_right_neighbors
                    ,   chainEnergy = computeEnergy new_chain
                    }
            in new_chain
    go2
        right_boundary
        right_overlap_boundaries
        state_site_tensor
        operator_site_tensor
        overlap_trios
        desired_bandwidth_at_site
        ((neighbor,next_bandwidth_dimension):remaining_neighbors_to_process)
        new_neighbors
        =
        increaseBandwidthBetween
            desired_bandwidth_at_site
            (rightNeighborState neighbor)
            state_site_tensor
        >>= \(denormalized_state_site_tensor,normalized_state_site_tensor) ->
            let (new_right_boundary,new_right_overlap_boundaries,new_neighbor) =
                    absorbIntoNewRightNeighbor
                        right_boundary
                        right_overlap_boundaries
                        normalized_state_site_tensor
                        operator_site_tensor
                        overlap_trios
            in go2
                new_right_boundary
                new_right_overlap_boundaries
                denormalized_state_site_tensor
                (rightNeighborOperator neighbor)
                (rightNeighborOverlapTrios neighbor)
                next_bandwidth_dimension
                remaining_neighbors_to_process
                (new_neighbor:new_neighbors)

increaseChainBandwidth _ _ = error "This algorithm is only designed to work when the chain is at its leftmost site."
-- @-node:gcross.20091115105949.1744:increaseChainBandwidth
-- @+node:gcross.20100512151146.1745:increaseChainBandwithWithSanityCheck
increaseChainBandwidthWithSanityCheck tolerance new_bandwidth old_chain =
    increaseChainBandwidth new_bandwidth old_chain
    >>=
    \new_chain ->
        let new_energy = chainEnergy new_chain
            old_energy = chainEnergy old_chain
        in if abs (new_energy - old_energy) > tolerance
            then throwIO $ EnergyChangedAfterBandwidthIncreaseError (maximumBandwidthIn old_chain) new_bandwidth old_energy new_energy
            else return new_chain
-- @-node:gcross.20100512151146.1745:increaseChainBandwithWithSanityCheck
-- @+node:gcross.20091118213523.1855:maximumBandwidthIn
maximumBandwidthIn :: EnergyMinimizationChain -> Int
maximumBandwidthIn chain =
    maximum
    .
    case siteLeftNeighbors chain of
        [] -> id
        neighbors -> (:) (maximum . map (rightBandwidthOfState . leftNeighborState) $ neighbors)
    .
    case siteRightNeighbors chain of
        [] -> id
        neighbors -> (:) (maximum . map (leftBandwidthOfState . rightNeighborState) $ neighbors)
    $
    [   leftBandwidthOfState . siteStateTensor $ chain
    ,   rightBandwidthOfState . siteStateTensor $ chain
    ]
-- @-node:gcross.20091118213523.1855:maximumBandwidthIn
-- @+node:gcross.20091113142219.1687:optimizeSite
optimizeSite :: Double -> Int -> EnergyMinimizationChain -> Either OptimizerFailure (Int,EnergyMinimizationChain)
optimizeSite tolerance maximum_number_of_iterations chain =
    mapRight postProcess $
        computeOptimalSiteStateTensor
            (siteLeftBoundaryTensor chain)
            (siteStateTensor chain)
            (siteHamiltonianTensor chain)
            (siteRightBoundaryTensor chain)
            (computeProjectorMatrix chain)
            SR
            tolerance
            maximum_number_of_iterations
  where
    postProcess (number_of_iterations,new_energy,optimal_site_tensor) =
        (number_of_iterations
        ,chain
            {   siteStateTensor = optimal_site_tensor
            ,   chainEnergy = new_energy
            }
        )

optimizeSite_ = optimizeSite 0 1000
-- @-node:gcross.20091113142219.1687:optimizeSite
-- @-node:gcross.20091113142219.1678:Chain Functions
-- @-others
-- @-node:gcross.20091113142219.1659:@thin EnergyMinimizationChain.hs
-- @-leo
