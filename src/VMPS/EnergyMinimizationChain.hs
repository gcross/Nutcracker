-- @+leo-ver=4-thin
-- @+node:gcross.20091113142219.1659:@thin EnergyMinimizationChain.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113142219.1661:<< Language extensions >>
-- @-node:gcross.20091113142219.1661:<< Language extensions >>
-- @nl

module VMPS.EnergyMinimizationChain where

-- @<< Import needed modules >>
-- @+node:gcross.20091113142219.1663:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad

import Data.Complex
import Data.Function
import Data.List

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
-- @+others
-- @+node:gcross.20091117140132.1791:Definitions
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
-- @-node:gcross.20091117140132.1791:Definitions
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
computeBandwidthDimensionSequence :: Int -> Int -> Int -> [Int]
computeBandwidthDimensionSequence number_of_sites physical_dimension bandwidth_dimension =
    let go :: Int -> Int -> [Int] -> (Int,[Int])
        go list_length next_bandwidth_dimension list
            | next_bandwidth_dimension >= bandwidth_dimension
                = (list_length,list)
            | otherwise
                = go (list_length+1) (next_bandwidth_dimension*physical_dimension) (next_bandwidth_dimension:list)
        (prefix_list_length,prefix_list) = go 0 1 []
        filler_length = number_of_sites - 2 * prefix_list_length + 1
    in if filler_length < 1 then error "The supplied bandwidth dimension is too large for the given physical dimension and number of sites."
        else reverse prefix_list ++ replicate filler_length bandwidth_dimension ++ prefix_list 
-- @-node:gcross.20091113142219.1699:computeBandwidthDimensionSequence
-- @+node:gcross.20091113142219.2519:computeBandwidthDimensionsAtAllSites
computeBandwidthDimensionsAtAllSites :: Int -> Int -> Int -> [(Int,Int)]
computeBandwidthDimensionsAtAllSites number_of_sites physical_dimension bandwidth_dimension =
    uncurry zip . (id &&& tail) $
        computeBandwidthDimensionSequence number_of_sites physical_dimension bandwidth_dimension
-- @-node:gcross.20091113142219.2519:computeBandwidthDimensionsAtAllSites
-- @+node:gcross.20091120112621.1590:makeConfiguration
makeConfiguration :: [OperatorSiteTensor] -> [[OverlapTensorTrio]] -> [(OperatorSiteTensor,[OverlapTensorTrio])]
makeConfiguration operator_site_tensors [] = zip operator_site_tensors (repeat [])
makeConfiguration operator_site_tensors projectors = zip operator_site_tensors . transpose $ projectors
-- @-node:gcross.20091120112621.1590:makeConfiguration
-- @-node:gcross.20091116222034.2374:Utility Functions
-- @+node:gcross.20091113142219.1678:Chain Functions
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
-- @+node:gcross.20091117140132.1797:computeProjectorMatrix
computeProjectorMatrix :: EnergyMinimizationChain -> ProjectorMatrix
computeProjectorMatrix chain =
    formProjectorMatrix $
        zip3 (siteLeftOverlapBoundaryTensors chain)
             (siteRightOverlapBoundaryTensors chain)
             (map overlapUnnormalizedTensor . siteOverlapTrios $ chain)
-- @-node:gcross.20091117140132.1797:computeProjectorMatrix
-- @+node:gcross.20091117140132.1798:computeOverlapTriosFromStateTensors
computeOverlapTriosFromCanonicalStateRepresentation :: CanonicalStateRepresentation -> [OverlapTensorTrio]
computeOverlapTriosFromCanonicalStateRepresentation (CanonicalStateRepresentation _ []) =
    error "There needs to be more than one site to use this function!"
computeOverlapTriosFromCanonicalStateRepresentation
    (CanonicalStateRepresentation
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
-- @+node:gcross.20091113142219.1687:optimizeSite
optimizeSite :: Double -> Int -> EnergyMinimizationChain -> Either OptimizerFailureReason (Int,EnergyMinimizationChain)
optimizeSite tolerance maximum_number_of_iterations chain =
    either Left postProcess $
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
    postProcess (number_of_iterations, eigenvalue, optimal_site_tensor) =
        if (imagPart eigenvalue ~= 0)
            then Right (number_of_iterations, chain
                    {   siteStateTensor = optimal_site_tensor
                    ,   chainEnergy = realPart eigenvalue
                    }
                 )
            else error $ "Non-imaginary eigenvalue: " ++ (show eigenvalue)

optimizeSite_ = optimizeSite 0 1000
-- @-node:gcross.20091113142219.1687:optimizeSite
-- @+node:gcross.20091117140132.1796:generateRandomizedChain
generateRandomizedChain :: Int -> Int -> [OperatorSiteTensor] -> IO (EnergyMinimizationChain)
generateRandomizedChain d b = generateRandomizedChainWithOverlaps d b . flip zip (repeat [])
-- @-node:gcross.20091117140132.1796:generateRandomizedChain
-- @+node:gcross.20091113142219.2535:generateRandomizedChainWithOverlaps
generateRandomizedChainWithOverlaps :: Int -> Int -> [(OperatorSiteTensor,[OverlapTensorTrio])] -> IO (EnergyMinimizationChain)
generateRandomizedChainWithOverlaps _ _ [] = error "Must have at least one operator site tensor!"
generateRandomizedChainWithOverlaps physical_dimension bandwidth_dimension operator_site_tensors =
    let number_of_sites = length operator_site_tensors
        number_of_projectors = length . snd . head $ operator_site_tensors
        normalized_randomizer = uncurry (generateRandomizedStateSiteTensor physical_dimension)
        unnormalized_randomizer = uncurry (generateRandomizedStateSiteTensor physical_dimension)
        state_site_bandwidth_dimensions =
            computeBandwidthDimensionsAtAllSites number_of_sites physical_dimension bandwidth_dimension
        go final_right_boundary
           final_right_overlap_boundaries
           final_neighbors
           []
           = return (final_neighbors,final_right_boundary,final_right_overlap_boundaries)
        go right_boundary
           right_overlap_boundaries
           current_neighbors
           ((bandwidth_dimensions,(operator_site_tensor,overlap_trios)):remaining) 
           = normalized_randomizer bandwidth_dimensions
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
        (first_site_left_bandwidth_dimension, first_site_right_bandwidth_dimension) = head state_site_bandwidth_dimensions
    in do
        (right_neighbors,right_boundary,right_overlap_boundaries) <-
            go trivial_right_boundary
               (replicate number_of_projectors trivial_right_overlap_boundary)
               []
               (reverse . tail $ zip state_site_bandwidth_dimensions operator_site_tensors)
        unnormalized_state_site_tensor <- unnormalized_randomizer . head $ state_site_bandwidth_dimensions
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
-- @+node:gcross.20091115105949.1744:increaseChainBandwidth
increaseChainBandwidth :: Int -> Int -> EnergyMinimizationChain -> IO EnergyMinimizationChain
increaseChainBandwidth
    physical_dimension new_bandwidth
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
    $
    computeBandwidthDimensionSequence number_of_sites physical_dimension new_bandwidth
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

increaseChainBandwidth _ _ _ = error "This algorithm is only designed to work when the chain is at its leftmost site."
-- @-node:gcross.20091115105949.1744:increaseChainBandwidth
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
-- @+node:gcross.20091119150241.1849:getCanonicalStateRepresentation
getCanonicalStateRepresentation :: EnergyMinimizationChain -> CanonicalStateRepresentation
getCanonicalStateRepresentation chain
    | siteNumber chain > 1
        = error "The chain must be at the first site in order to extract the canonical state representation."
    | otherwise
        = CanonicalStateRepresentation
            (siteStateTensor chain)
            (map rightNeighborState . siteRightNeighbors $ chain)
-- @-node:gcross.20091119150241.1849:getCanonicalStateRepresentation
-- @-node:gcross.20091113142219.1678:Chain Functions
-- @-others
-- @-node:gcross.20091113142219.1659:@thin EnergyMinimizationChain.hs
-- @-leo
