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
import Control.Monad.Identity
import Control.Monad.State

import VMPS.EnergyMinimizationChain
-- @-node:gcross.20091118213523.1812:<< Import needed modules >>
-- @nl

-- @+others
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
            (\_ _ -> return ())
            tolerance
            maximum_number_of_iterations
            starting_chain

performOptimizationSweep_ = performOptimizationSweep 0 1000
-- @nonl
-- @-node:gcross.20091118213523.1817:performOptimizationSweep
-- @+node:gcross.20091118213523.1810:performOptimizationSweepWithCallback
performOptimizationSweepWithCallback ::
    Monad m =>
    (SweepDirection -> EnergyMinimizationChain -> m ()) ->
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
        let optimized_chain = snd . runOptimizerOn $ chain
        in callback SweepingRight optimized_chain
            >>
           goRight (n-1) (activateRightNeighbor optimized_chain)

    goLeft 0 chain = return chain
    goLeft n chain =
        let optimized_chain = snd . runOptimizerOn $ chain
        in callback SweepingLeft optimized_chain
            >>
           goLeft (n-1) (activateLeftNeighbor optimized_chain)
-- @-node:gcross.20091118213523.1810:performOptimizationSweepWithCallback
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
            (\_ _ -> return ())
            (\_ _ -> return ())
            energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
            starting_chain

performRepeatedSweepsUntilConvergence_ = performRepeatedSweepsUntilConvergence 1e-7 0 1000
-- @-node:gcross.20091118213523.1826:performRepeatedSweepsUntilConvergence
-- @+node:gcross.20091118213523.1825:performRepeatedSweepsUntilConvergenceWithCallbacks
performRepeatedSweepsUntilConvergenceWithCallbacks ::
    Monad m =>
    (Bool -> EnergyMinimizationChain -> m ()) ->
    (SweepDirection -> EnergyMinimizationChain -> m ()) ->
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
            callback_to_increase_bandwidth
            (\_ _ -> return ())
            (\_ _ -> return ())
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
            chain
  where
    callback_to_increase_bandwidth chain = do
        old_bandwidth <- get
        let new_bandwidth = old_bandwidth+1
        put new_bandwidth
        liftIO $ increaseChainBandwidth chain physical_dimension new_bandwidth

increaseBandwidthAndSweepUntilConvergence_ =
    increaseBandwidthAndSweepUntilConvergence 1e-7 1e-7 0 1000
-- @nonl
-- @-node:gcross.20091118213523.1846:increaseBandwidthAndSweepUntilConvergence
-- @+node:gcross.20091118213523.1854:increaseBandwidthAndSweepUntilConvergenceWithCallbacks
increaseBandwidthAndSweepUntilConvergenceWithCallbacks ::
    Monad m =>
    (EnergyMinimizationChain -> m EnergyMinimizationChain) ->
    (Bool -> EnergyMinimizationChain -> m ()) ->
    (SweepDirection -> EnergyMinimizationChain -> m ()) ->
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
-- @-node:gcross.20091118213523.1844:Bandwidth increasing
-- @-node:gcross.20091118213523.1814:Functions
-- @-others
-- @-node:gcross.20091118213523.1809:@thin Algorithms.hs
-- @-leo
