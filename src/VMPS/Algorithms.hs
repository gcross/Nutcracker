-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1809:@thin Algorithms.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091118213523.1811:<< Language extensions >>
-- @-node:gcross.20091118213523.1811:<< Language extensions >>
-- @nl

module VMPS.Algorithms where

-- @<< Import needed modules >>
-- @+node:gcross.20091118213523.1812:<< Import needed modules >>
import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State

import Data.List
import Data.Maybe

import VMPS.EnergyMinimizationChain
import VMPS.Tensors
import VMPS.Wrappers
-- @nonl
-- @-node:gcross.20091118213523.1812:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091119150241.1881:Callback Types
type SiteCallback m = Maybe OptimizerFailureReason -> SweepDirection -> EnergyMinimizationChain -> m ()
type SweepCallback m = Bool -> EnergyMinimizationChain -> m ()
type BandwidthIncreaseCallback m = EnergyMinimizationChain -> m EnergyMinimizationChain
-- @-node:gcross.20091119150241.1881:Callback Types
-- @+node:gcross.20091118213523.1814:Functions
-- @+node:gcross.20091118213523.1822:Single sweep optimization
-- @+node:gcross.20091118213523.1815:SweepDirection
data SweepDirection = SweepingRight | SweepingLeft
-- @-node:gcross.20091118213523.1815:SweepDirection
-- @+node:gcross.20091118213523.1817:performOptimizationSweep
performOptimizationSweep :: Double -> Int -> EnergyMinimizationChain -> EnergyMinimizationChain
performOptimizationSweep tolerance maximum_number_of_iterations starting_chain
    = runIdentity $
          performOptimizationSweepWithCallback
            ignoreSiteCallback
            tolerance
            maximum_number_of_iterations
            starting_chain

performOptimizationSweep_ = performOptimizationSweep 0 1000
-- @nonl
-- @-node:gcross.20091118213523.1817:performOptimizationSweep
-- @+node:gcross.20091118213523.1810:performOptimizationSweepWithCallback
performOptimizationSweepWithCallback ::
    Monad m =>
    SiteCallback m ->
    Double ->
    Int ->
    EnergyMinimizationChain ->
    m EnergyMinimizationChain
performOptimizationSweepWithCallback callback tolerance maximum_number_of_iterations starting_chain
    | siteNumber starting_chain /= 1
        = error "This algorithm requires that the first site be active."
    | number_of_sites <= 1
        = error "This algorithm requires the chain to have more than one site."
    | otherwise
        = goRight (number_of_sites-1) starting_chain
  where
    number_of_sites = chainNumberOfSites starting_chain

    runOptimizerOn = optimizeSite tolerance maximum_number_of_iterations

    goRight 0 chain = goLeft (number_of_sites-1) chain
    goRight n chain =
        case runOptimizerOn $ chain of
            Left failure_reason -> callback (Just failure_reason) SweepingRight chain >> return chain
            Right (_,optimized_chain) -> callback Nothing SweepingRight optimized_chain >> return optimized_chain
        >>=
        goRight (n-1) . activateRightNeighbor

    goLeft 0 chain = return chain
    goLeft n chain =
        case runOptimizerOn $ chain of
            Left failure_reason -> callback (Just failure_reason) SweepingLeft chain >> return chain
            Right (_,optimized_chain) -> callback Nothing SweepingLeft optimized_chain >> return optimized_chain
        >>=
        goLeft (n-1) . activateLeftNeighbor
-- @-node:gcross.20091118213523.1810:performOptimizationSweepWithCallback
-- @+node:gcross.20091119150241.1855:ignoreSiteCallback
ignoreSiteCallback _ _ _ = return ()
-- @-node:gcross.20091119150241.1855:ignoreSiteCallback
-- @-node:gcross.20091118213523.1822:Single sweep optimization
-- @+node:gcross.20091118213523.1823:Multiple sweep optimization
-- @+node:gcross.20091118213523.1826:performRepeatedSweepsUntilConvergence
performRepeatedSweepsUntilConvergence :: Double ->
                                         Double ->
                                         Int ->
                                         EnergyMinimizationChain ->
                                         EnergyMinimizationChain
performRepeatedSweepsUntilConvergence energy_change_convergence_criterion
                                      tolerance
                                      maximum_number_of_iterations
                                      starting_chain
    = runIdentity $
          performRepeatedSweepsUntilConvergenceWithCallbacks
            ignoreSweepCallback
            ignoreSiteCallback
            energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
            starting_chain

performRepeatedSweepsUntilConvergence_ = performRepeatedSweepsUntilConvergence 1e-7 0 1000
-- @-node:gcross.20091118213523.1826:performRepeatedSweepsUntilConvergence
-- @+node:gcross.20091118213523.1825:performRepeatedSweepsUntilConvergenceWithCallbacks
performRepeatedSweepsUntilConvergenceWithCallbacks ::
    Monad m =>
    SweepCallback m ->
    SiteCallback m ->
    Double ->
    Double ->
    Int ->
    EnergyMinimizationChain ->
    m EnergyMinimizationChain
performRepeatedSweepsUntilConvergenceWithCallbacks
    callback_after_each_sweep
    callback_after_each_site
    energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    old_chain
    = performOptimizationSweepWithCallback
        callback_after_each_site
        tolerance
        maximum_number_of_iterations
        old_chain
    >>=
    \new_chain ->
        let new_energy = chainEnergy new_chain
            old_energy = chainEnergy old_chain
        in if old_energy - new_energy <= energy_change_convergence_criterion
            then callback_after_each_sweep True new_chain >> return new_chain
            else
                callback_after_each_sweep False new_chain
                >>
                performRepeatedSweepsUntilConvergenceWithCallbacks
                    callback_after_each_sweep
                    callback_after_each_site
                    energy_change_convergence_criterion
                    tolerance
                    maximum_number_of_iterations
                    new_chain
-- @-node:gcross.20091118213523.1825:performRepeatedSweepsUntilConvergenceWithCallbacks
-- @+node:gcross.20091119150241.1857:ignoreSiteCallback
ignoreSweepCallback _ _ = return ()
-- @-node:gcross.20091119150241.1857:ignoreSiteCallback
-- @-node:gcross.20091118213523.1823:Multiple sweep optimization
-- @+node:gcross.20091118213523.1844:Bandwidth increasing
-- @+node:gcross.20091118213523.1846:increaseBandwidthAndSweepUntilConvergence
increaseBandwidthAndSweepUntilConvergence ::
    Double ->
    Double ->
    Double ->
    Int ->
    Int ->
    EnergyMinimizationChain ->
    IO EnergyMinimizationChain
increaseBandwidthAndSweepUntilConvergence
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    physical_dimension
    chain
    =
    flip evalStateT (maximumBandwidthIn chain) $
        increaseBandwidthAndSweepUntilConvergenceWithCallbacks
            (singleIncrementBandwidthIncreaser physical_dimension)
            ignoreSweepCallback
            ignoreSiteCallback
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
            chain

increaseBandwidthAndSweepUntilConvergence_ =
    increaseBandwidthAndSweepUntilConvergence 1e-7 1e-7 0 1000
-- @nonl
-- @-node:gcross.20091118213523.1846:increaseBandwidthAndSweepUntilConvergence
-- @+node:gcross.20091118213523.1854:increaseBandwidthAndSweepUntilConvergenceWithCallbacks
increaseBandwidthAndSweepUntilConvergenceWithCallbacks ::
    Monad m =>
    BandwidthIncreaseCallback m ->
    SweepCallback m ->
    SiteCallback m ->
    Double ->
    Double ->
    Double ->
    Int ->
    EnergyMinimizationChain ->
    m EnergyMinimizationChain
increaseBandwidthAndSweepUntilConvergenceWithCallbacks
    callback_to_increase_bandwidth
    callback_after_each_sweep
    callback_after_each_site
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    = runOptimizer >=> go
  where
    go old_chain =
        callback_to_increase_bandwidth old_chain
        >>=
        runOptimizer
        >>=
        \new_chain ->
            let new_energy = chainEnergy new_chain
                old_energy = chainEnergy old_chain
            in if old_energy - new_energy <= bandwidth_increase_energy_change_convergence_criterion
                then return old_chain
                else go new_chain

    runOptimizer = 
        performRepeatedSweepsUntilConvergenceWithCallbacks
            callback_after_each_sweep
            callback_after_each_site
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations

-- @-node:gcross.20091118213523.1854:increaseBandwidthAndSweepUntilConvergenceWithCallbacks
-- @+node:gcross.20091119150241.1854:singleIncrementBandwidthIncreaser
singleIncrementBandwidthIncreaser physical_dimension chain = do
    old_bandwidth <- get
    let new_bandwidth = old_bandwidth+1
    put new_bandwidth
    liftIO $ increaseChainBandwidth physical_dimension new_bandwidth chain
-- @-node:gcross.20091119150241.1854:singleIncrementBandwidthIncreaser
-- @-node:gcross.20091118213523.1844:Bandwidth increasing
-- @+node:gcross.20091119150241.1846:Multiple trials
-- @+node:gcross.20091119150241.1847:runMultipleTrialsWithCallbacks
runMultipleTrialsWithCallbacks ::
    Monad m =>
    (EnergyMinimizationChain -> m (Either EnergyMinimizationChain (Double,CanonicalStateRepresentation))) ->
    BandwidthIncreaseCallback m ->
    SweepCallback m ->
    SiteCallback m ->
    Double ->
    Double ->
    Double ->
    Int ->
    EnergyMinimizationChain ->
    m (Double,CanonicalStateRepresentation)
runMultipleTrialsWithCallbacks
    callback_to_determine_whether_to_declare_victory
    callback_to_increase_bandwidth
    callback_after_each_sweep
    callback_after_each_site
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    =
    increaseBandwidthAndSweepUntilConvergenceWithCallbacks
        callback_to_increase_bandwidth
        callback_after_each_sweep
        callback_after_each_site
        bandwidth_increase_energy_change_convergence_criterion
        multisweep_energy_change_convergence_criterion
        tolerance
        maximum_number_of_iterations
    >=>
    callback_to_determine_whether_to_declare_victory
    >=>
    either
        (runMultipleTrialsWithCallbacks
            callback_to_determine_whether_to_declare_victory
            callback_to_increase_bandwidth
            callback_after_each_sweep
            callback_after_each_site
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations)
        return
-- @-node:gcross.20091119150241.1847:runMultipleTrialsWithCallbacks
-- @-node:gcross.20091119150241.1846:Multiple trials
-- @+node:gcross.20091119150241.1851:Multiple levels
-- @+node:gcross.20091119150241.1853:solveForMultipleLevels
solveForMultipleLevels ::
    Double ->
    Double ->
    Double ->
    Int ->
    Int ->
    Int ->
    [OperatorSiteTensor] ->
    [[OverlapTensorTrio]] ->
    IO [(Double,CanonicalStateRepresentation,[OverlapTensorTrio])]
solveForMultipleLevels
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    number_of_levels
    physical_dimension
    operator_site_tensors
    projectors
    = flip evalStateT undefined $
        solveForMultipleLevelsWithCallbacks
            (const . return $ True)
            (\projectors ->
                let configuration =
                        case projectors of
                            [] -> zip operator_site_tensors (repeat [])
                            _ -> zip operator_site_tensors . transpose $ projectors
                in put 2 >> (liftIO $ generateRandomizedChainWithOverlaps physical_dimension 2 configuration)
            )
            (singleIncrementBandwidthIncreaser physical_dimension)
            ignoreSweepCallback
            ignoreSiteCallback
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
            number_of_levels
            projectors

solveForMultipleLevels_ = solveForMultipleLevels 1e-7 1e-7 0 1000
-- @-node:gcross.20091119150241.1853:solveForMultipleLevels
-- @+node:gcross.20091119150241.1852:solveForMultipleLevelsWithCallbacks
solveForMultipleLevelsWithCallbacks ::
    Monad m =>
    (EnergyMinimizationChain -> m Bool) ->
    ([[OverlapTensorTrio]] -> m EnergyMinimizationChain) ->
    BandwidthIncreaseCallback m ->
    SweepCallback m ->
    SiteCallback m ->
    Double ->
    Double ->
    Double ->
    Int ->
    Int ->
    [[OverlapTensorTrio]] ->
    m [(Double,CanonicalStateRepresentation,[OverlapTensorTrio])]
solveForMultipleLevelsWithCallbacks
    callback_to_decide_whether_to_declare_victory_with_trial
    callback_to_create_new_chain
    callback_to_increase_bandwidth
    callback_after_each_sweep
    callback_after_each_site
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    = go []
  where
    go levels 0 _ = (return . reverse) levels
    go levels n projectors =
        new_chain_factory
        >>=
        runMultipleTrialsWithCallbacks
            my_callback
            callback_to_increase_bandwidth
            callback_after_each_sweep
            callback_after_each_site
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
        >>=
        \(energy,state) ->
            let projector = computeOverlapTriosFromCanonicalStateRepresentation state
            in go ((energy,state,projector):levels) (n-1) (projector:projectors)
      where
        new_chain_factory = callback_to_create_new_chain projectors

        my_callback chain =
            callback_to_decide_whether_to_declare_victory_with_trial chain
            >>=
            \declare_victory ->
                if declare_victory 
                    then return . Right . (chainEnergy &&& getCanonicalStateRepresentation) $ chain
                    else new_chain_factory >>= return . Left
-- @-node:gcross.20091119150241.1852:solveForMultipleLevelsWithCallbacks
-- @-node:gcross.20091119150241.1851:Multiple levels
-- @-node:gcross.20091118213523.1814:Functions
-- @-others
-- @-node:gcross.20091118213523.1809:@thin Algorithms.hs
-- @-leo
