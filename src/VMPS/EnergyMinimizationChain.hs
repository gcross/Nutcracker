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

import Data.Complex
import Data.Function

import Debug.Trace

import VMPS.Miscellaneous
import VMPS.Tensors
import VMPS.Wrappers
-- @-node:gcross.20091113142219.1663:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091113142219.1664:Types
-- @+node:gcross.20091115105949.1734:Neighbor
data (StateSiteTensorClass b) => Neighbor a b = Neighbor
    {   neighborBoundary :: a
    ,   neighborState    :: b
    ,   neighborOperator :: OperatorSiteTensor
    }
-- @-node:gcross.20091115105949.1734:Neighbor
-- @+node:gcross.20091113142219.1665:EnergyMinimizationChain
data EnergyMinimizationChain = EnergyMinimizationChain
    {   siteLeftBoundaryTensor :: LeftBoundaryTensor
    ,   siteStateTensor :: UnnormalizedStateSiteTensor
    ,   siteHamiltonianTensor :: OperatorSiteTensor
    ,   siteRightBoundaryTensor :: RightBoundaryTensor
    ,   siteLeftNeighbors :: [Neighbor LeftBoundaryTensor LeftAbsorptionNormalizedStateSiteTensor]
    ,   siteRightNeighbors :: [Neighbor RightBoundaryTensor RightAbsorptionNormalizedStateSiteTensor]
    ,   siteNumber :: !Int
    ,   chainNumberOfSites :: !Int
    ,   chainEnergy :: Double
    }
-- @-node:gcross.20091113142219.1665:EnergyMinimizationChain
-- @-node:gcross.20091113142219.1664:Types
-- @+node:gcross.20091113142219.1678:Functions
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
-- @+node:gcross.20091113142219.1684:moveActiveSiteLeftByOneBead
activateLeftNeighbor :: EnergyMinimizationChain -> EnergyMinimizationChain
activateLeftNeighbor EnergyMinimizationChain { siteLeftNeighbors = [] } =
    error "Chain is already at its leftmost site!"
activateLeftNeighbor EnergyMinimizationChain
    {   siteStateTensor = state_tensor_to_normalize
    ,   siteHamiltonianTensor = old_hamiltonian_tensor
    ,   siteRightBoundaryTensor = old_right_boundary_tensor
    ,   siteLeftNeighbors =
            (Neighbor
                new_left_boundary
                state_tensor_to_denormalize
                new_hamiltonian_tensor
            ):new_left_neighbors
    ,   siteRightNeighbors = old_right_neighbors
    ,   siteNumber = old_site_number
    ,   chainNumberOfSites = number_of_sites
    } =
    let (active_site_tensor,normalized_site_tensor) = makeNormalizedForAbsorbingRight state_tensor_to_denormalize state_tensor_to_normalize
        new_chain = EnergyMinimizationChain
            {   siteLeftBoundaryTensor = new_left_boundary
            ,   siteStateTensor = active_site_tensor
            ,   siteHamiltonianTensor = new_hamiltonian_tensor
            ,   siteRightBoundaryTensor = contractSOSRight old_right_boundary_tensor normalized_site_tensor old_hamiltonian_tensor
            ,   siteLeftNeighbors = new_left_neighbors
            ,   siteRightNeighbors =
                    (Neighbor
                        old_right_boundary_tensor
                        normalized_site_tensor
                        old_hamiltonian_tensor
                    ):old_right_neighbors
            ,   siteNumber = old_site_number-1
            ,   chainNumberOfSites = number_of_sites
            ,   chainEnergy = computeEnergy new_chain
            }
    in new_chain
-- @-node:gcross.20091113142219.1684:moveActiveSiteLeftByOneBead
-- @+node:gcross.20091113142219.1686:moveActiveSiteRightByOneBead
activateRightNeighbor :: EnergyMinimizationChain -> EnergyMinimizationChain
activateRightNeighbor EnergyMinimizationChain { siteRightNeighbors = [] } =
    error "Chain is already at its rightmost site!"
activateRightNeighbor EnergyMinimizationChain
    {   siteLeftBoundaryTensor = old_left_boundary_tensor
    ,   siteStateTensor = state_tensor_to_normalize
    ,   siteHamiltonianTensor = old_hamiltonian_tensor
    ,   siteLeftNeighbors = old_left_neighbors
    ,   siteRightNeighbors =
            (Neighbor
                new_right_boundary
                state_tensor_to_denormalize
                new_hamiltonian_tensor
            ):new_right_neighbors
    ,   siteNumber = old_site_number
    ,   chainNumberOfSites = number_of_sites
    } =
    let (normalized_site_tensor,active_site_tensor) = makeNormalizedForAbsorbingLeft state_tensor_to_normalize state_tensor_to_denormalize
        new_chain = EnergyMinimizationChain
            {   siteLeftBoundaryTensor = contractSOSLeft old_left_boundary_tensor normalized_site_tensor old_hamiltonian_tensor
            ,   siteStateTensor = active_site_tensor
            ,   siteHamiltonianTensor = new_hamiltonian_tensor
            ,   siteRightBoundaryTensor = new_right_boundary
            ,   siteLeftNeighbors =
                    (Neighbor
                        old_left_boundary_tensor
                        normalized_site_tensor
                        old_hamiltonian_tensor
                    ):old_left_neighbors
            ,   siteRightNeighbors = new_right_neighbors
            ,   siteNumber = old_site_number+1
            ,   chainNumberOfSites = number_of_sites
            ,   chainEnergy = computeEnergy new_chain
            }
    in new_chain
-- @-node:gcross.20091113142219.1686:moveActiveSiteRightByOneBead
-- @+node:gcross.20091113142219.1687:optimizeSite
optimizeSite :: Double -> Int -> EnergyMinimizationChain -> (Int,EnergyMinimizationChain)
optimizeSite tolerance maximum_number_of_iterations chain@EnergyMinimizationChain
            {   siteLeftBoundaryTensor = left_boundary_tensor
            ,   siteStateTensor = state_site_tensor
            ,   siteHamiltonianTensor = operator_site_tensor
            ,   siteRightBoundaryTensor = right_boundary_tensor
            } =
    let (number_of_iterations, eigenvalue, optimal_site_tensor) =
            computeOptimalSiteStateTensor
                left_boundary_tensor
                state_site_tensor
                operator_site_tensor
                right_boundary_tensor
                SR
                tolerance
                maximum_number_of_iterations
    in assert (imagPart eigenvalue ~= 0) $
        (number_of_iterations, chain
            {   siteStateTensor = optimal_site_tensor
            ,   chainEnergy = realPart eigenvalue
            }
        )

optimizeSite_ = optimizeSite 0 1000
-- @-node:gcross.20091113142219.1687:optimizeSite
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
-- @+node:gcross.20091113142219.2535:generateRandomizedChain
generateRandomizedChain :: [OperatorSiteTensor] -> Int -> Int -> IO (EnergyMinimizationChain)
generateRandomizedChain [] _ _ = error "Must have at least one operator site tensor!"
generateRandomizedChain operator_site_tensors physical_dimension bandwidth_dimension =
    let number_of_sites = length operator_site_tensors
        normalized_randomizer = uncurry (generateRandomizedStateSiteTensor physical_dimension)
        unnormalized_randomizer = uncurry (generateRandomizedStateSiteTensor physical_dimension)
        state_site_bandwidth_dimensions = computeBandwidthDimensionsAtAllSites number_of_sites physical_dimension bandwidth_dimension
        go final_neighbors final_right_environment [] = return (final_neighbors,final_right_environment)
        go right_environment current_neighbors ((bandwidth_dimensions,operator_site_tensor):remaining) =
            normalized_randomizer bandwidth_dimensions
            >>=
            \site_state_tensor ->
                go  (contractSOSRight right_environment site_state_tensor operator_site_tensor)
                    ((Neighbor right_environment site_state_tensor operator_site_tensor):current_neighbors)
                    remaining
        (first_site_left_bandwidth_dimension, first_site_right_bandwidth_dimension) = head state_site_bandwidth_dimensions
    in do
        (right_boundary,right_neighbors) <- go trivial_right_boundary [] (reverse . tail $ zip state_site_bandwidth_dimensions operator_site_tensors)
        unnormalized_state_site_tensor <- unnormalized_randomizer . head $ state_site_bandwidth_dimensions
        return $
            let chain = EnergyMinimizationChain
                    {   siteLeftBoundaryTensor = trivial_left_boundary
                    ,   siteStateTensor = unnormalized_state_site_tensor
                    ,   siteHamiltonianTensor = head operator_site_tensors
                    ,   siteRightBoundaryTensor = right_boundary
                    ,   siteLeftNeighbors = []
                    ,   siteRightNeighbors = right_neighbors
                    ,   siteNumber = 1
                    ,   chainNumberOfSites = number_of_sites
                    ,   chainEnergy = computeEnergy chain
                    }
            in chain
-- @-node:gcross.20091113142219.2535:generateRandomizedChain
-- @-node:gcross.20091113142219.1678:Functions
-- @-others
-- @-node:gcross.20091113142219.1659:@thin EnergyMinimizationChain.hs
-- @-leo
