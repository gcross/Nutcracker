-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1809:@thin Algorithms.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091118213523.1811:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20091118213523.1811:<< Language extensions >>
-- @nl

module VMPS.Algorithms where

-- @<< Import needed modules >>
-- @+node:gcross.20091118213523.1812:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Typeable

import Debug.Trace

import Text.Printf

import VMPS.EnergyMinimizationChain
import VMPS.Tensors
import VMPS.States
import VMPS.Wrappers
-- @-node:gcross.20091118213523.1812:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091119150241.1881:Callback Types
type SiteCallback m = Either OptimizerFailure Int → SweepDirection → EnergyMinimizationChain → m ()
type SweepCallback m = Bool → EnergyMinimizationChain → m ()
type BandwidthIncreaseCallback m = EnergyMinimizationChain → m EnergyMinimizationChain
type MultilevelVictoryDeterminationCallback m = EnergyMinimizationChain → m (Maybe (Double,CanonicalStateRepresentation))
-- @nonl
-- @-node:gcross.20091119150241.1881:Callback Types
-- @+node:gcross.20100513000837.1741:Exceptions
-- @+node:gcross.20100513000837.1742:SweepFailure
data SweepFailure = OptimizerFailureAtSite SweepDirection Int OptimizerFailure
    deriving (Typeable,Show)

instance Exception SweepFailure
-- @-node:gcross.20100513000837.1742:SweepFailure
-- @-node:gcross.20100513000837.1741:Exceptions
-- @+node:gcross.20091120112621.1591:Predefined Callbacks
-- @+node:gcross.20091120112621.1592:site callbacks
-- @+node:gcross.20091119150241.1855:ignoreSiteCallback
-- ignoreSiteCallback (Left e) direction chain = throw $ OptimizerFailureAtSite direction (siteNumber chain) e
ignoreSiteCallback (Left e) direction chain =
    trace (
        printf "WARNING -- Problem while optimizing site %i while %s with %i projectors: %s"
            (siteNumber chain)
            (show direction)
            (chainNumberOfProjectors chain)
            (show e)
    ) $ return ()
ignoreSiteCallback _ _ _ = return ()
-- @-node:gcross.20091119150241.1855:ignoreSiteCallback
-- @-node:gcross.20091120112621.1592:site callbacks
-- @+node:gcross.20091120112621.1593:sweep callbacks
-- @+node:gcross.20100204150305.1689:ignoreSweepCallback
ignoreSweepCallback _ _ = return ()
-- @-node:gcross.20100204150305.1689:ignoreSweepCallback
-- @-node:gcross.20091120112621.1593:sweep callbacks
-- @+node:gcross.20091120112621.1596:bandwidth increase callbacks
-- @+node:gcross.20091120112621.1597:newChainCreator
newChainCreator :: MonadIO m => (Int → m Int) → [OperatorSiteTensor] → [[OverlapTensorTrio]] → m EnergyMinimizationChain
newChainCreator initialization operator_site_tensors overlap_tensor_trios = do
    initial_bandwidth ← initialization (length overlap_tensor_trios)
    new_chain ←
        liftIO
        .
        fmap (
            fromMaybe (error "Bandwidth was insufficient to construct a randomized energy chain that was perpendicular to the projectors.")
            .
            projectChain
        )
        .
        generateRandomizedChainWithOverlaps initial_bandwidth
        $
        makeConfiguration operator_site_tensors overlap_tensor_trios
    return new_chain
-- @-node:gcross.20091120112621.1597:newChainCreator
-- @-node:gcross.20091120112621.1596:bandwidth increase callbacks
-- @+node:gcross.20091120112621.1594:multi-level callbacks
-- @+node:gcross.20091120112621.1595:alwaysDeclareVictory
alwaysDeclareVictory :: Monad m => MultilevelVictoryDeterminationCallback m
alwaysDeclareVictory = return . Just . (chainEnergy &&& getCanonicalStateRepresentation)
-- @-node:gcross.20091120112621.1595:alwaysDeclareVictory
-- @-node:gcross.20091120112621.1594:multi-level callbacks
-- @-node:gcross.20091120112621.1591:Predefined Callbacks
-- @+node:gcross.20100512154636.1737:Types
-- @+node:gcross.20091118213523.1815:SweepDirection
data SweepDirection = SweepingRight | SweepingLeft deriving (Show,Typeable)
-- @-node:gcross.20091118213523.1815:SweepDirection
-- @-node:gcross.20100512154636.1737:Types
-- @+node:gcross.20091118213523.1814:Functions
-- @+node:gcross.20091118213523.1822:Single sweep optimization
-- @+node:gcross.20091118213523.1817:performOptimizationSweep
performOptimizationSweep :: Double → Int → EnergyMinimizationChain → EnergyMinimizationChain
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
    SiteCallback m →
    Double →
    Int →
    EnergyMinimizationChain →
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
            Left failure_reason → callback (Left failure_reason) SweepingRight chain >>= \() → return chain
            Right (number_of_iterations,optimized_chain) → callback (Right number_of_iterations) SweepingRight optimized_chain >> return optimized_chain
        >>=
        goRight (n-1) .  activateRightNeighborWithSanityCheck 1e-7

    goLeft 0 chain = return chain
    goLeft n chain =
        case runOptimizerOn $ chain of
            Left failure_reason → callback (Left failure_reason) SweepingLeft chain >>= \() → return chain
            Right (number_of_iterations,optimized_chain) → callback (Right number_of_iterations) SweepingLeft optimized_chain >> return optimized_chain
        >>=
        goLeft (n-1) . activateLeftNeighborWithSanityCheck 1e-7

{-# INLINE performOptimizationSweepWithCallback #-}
-- @nonl
-- @-node:gcross.20091118213523.1810:performOptimizationSweepWithCallback
-- @-node:gcross.20091118213523.1822:Single sweep optimization
-- @+node:gcross.20091118213523.1823:Multiple sweep optimization
-- @+node:gcross.20091118213523.1826:performRepeatedSweepsUntilConvergence
performRepeatedSweepsUntilConvergence :: Double →
                                         Double →
                                         Int →
                                         EnergyMinimizationChain →
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
-- @nonl
-- @-node:gcross.20091118213523.1826:performRepeatedSweepsUntilConvergence
-- @+node:gcross.20091118213523.1825:performRepeatedSweepsUntilConvergenceWithCallbacks
performRepeatedSweepsUntilConvergenceWithCallbacks ::
    Monad m =>
    SweepCallback m →
    SiteCallback m →
    Double →
    Double →
    Int →
    EnergyMinimizationChain →
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
    \new_chain →
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

{-# INLINE performRepeatedSweepsUntilConvergenceWithCallbacks #-}
-- @nonl
-- @-node:gcross.20091118213523.1825:performRepeatedSweepsUntilConvergenceWithCallbacks
-- @-node:gcross.20091118213523.1823:Multiple sweep optimization
-- @+node:gcross.20091118213523.1844:Bandwidth increasing
-- @+node:gcross.20091118213523.1846:increaseBandwidthAndSweepUntilConvergence
increaseBandwidthAndSweepUntilConvergence ::
    Double →
    Double →
    Double →
    Int →
    EnergyMinimizationChain →
    IO EnergyMinimizationChain
increaseBandwidthAndSweepUntilConvergence
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    chain
    =
    flip evalStateT (maximumBandwidthIn chain) $
        increaseBandwidthAndSweepUntilConvergenceWithCallbacks
            singleIncrementBandwidthIncreaser
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
    BandwidthIncreaseCallback m →
    SweepCallback m →
    SiteCallback m →
    Double →
    Double →
    Double →
    Int →
    EnergyMinimizationChain →
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
        liftM (
            \new_chain →
                if abs (chainEnergy old_chain - chainEnergy new_chain) > 1e-7
                    then throw $
                        EnergyChangedAfterBandwidthIncreaseError
                            (maximumBandwidthIn old_chain)
                            (maximumBandwidthIn new_chain)
                            (chainEnergy old_chain)
                            (chainEnergy new_chain)
                    else new_chain
        ) (callback_to_increase_bandwidth old_chain)
        >>=
        runOptimizer
        >>=
        \new_chain →
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

{-# INLINE increaseBandwidthAndSweepUntilConvergenceWithCallbacks #-}
-- @nonl
-- @-node:gcross.20091118213523.1854:increaseBandwidthAndSweepUntilConvergenceWithCallbacks
-- @+node:gcross.20091119150241.1854:singleIncrementBandwidthIncreaser
singleIncrementBandwidthIncreaser chain = do
    old_bandwidth ← get
    let new_bandwidth = old_bandwidth+1
    put new_bandwidth
    liftIO $ increaseChainBandwidth new_bandwidth chain
-- @nonl
-- @-node:gcross.20091119150241.1854:singleIncrementBandwidthIncreaser
-- @-node:gcross.20091118213523.1844:Bandwidth increasing
-- @+node:gcross.20091119150241.1846:Multiple trials
-- @+node:gcross.20091119150241.1847:runMultipleTrialsWithCallbacks
runMultipleTrialsWithCallbacks ::
    Monad m =>
    (EnergyMinimizationChain → m (Either EnergyMinimizationChain (Double,CanonicalStateRepresentation))) →
    BandwidthIncreaseCallback m →
    SweepCallback m →
    SiteCallback m →
    Double →
    Double →
    Double →
    Int →
    EnergyMinimizationChain →
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

{-# INLINE runMultipleTrialsWithCallbacks #-}
-- @nonl
-- @-node:gcross.20091119150241.1847:runMultipleTrialsWithCallbacks
-- @-node:gcross.20091119150241.1846:Multiple trials
-- @+node:gcross.20091119150241.1851:Multiple levels
-- @+node:gcross.20091119150241.1852:solveForMultipleLevelsWithCallbacks
solveForMultipleLevelsWithCallbacks ::
    Monad m =>
    MultilevelVictoryDeterminationCallback m →
    ([[OverlapTensorTrio]] → m EnergyMinimizationChain) →
    BandwidthIncreaseCallback m →
    SweepCallback m →
    SiteCallback m →
    Double →
    Double →
    Double →
    Int →
    Int →
    [[OverlapTensorTrio]] →
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
        \(energy,state) →
            let projector = computeOverlapTriosFromCanonicalStateRepresentation state
            in go ((energy,state,projector):levels) (n-1) (projector:projectors)
      where
        new_chain_factory = callback_to_create_new_chain projectors

        my_callback chain =
            callback_to_decide_whether_to_declare_victory_with_trial chain
            >>=
            maybe (new_chain_factory >>= return . Left) (return . Right)

{-# INLINE solveForMultipleLevelsWithCallbacks #-}
-- @nonl
-- @-node:gcross.20091119150241.1852:solveForMultipleLevelsWithCallbacks
-- @+node:gcross.20091119150241.1853:solveForMultipleLevels
solveForMultipleLevels ::
    Double →
    Double →
    Double →
    Int →
    Int →
    [OperatorSiteTensor] →
    [[OverlapTensorTrio]] →
    IO [(Double,CanonicalStateRepresentation,[OverlapTensorTrio])]
solveForMultipleLevels
    bandwidth_increase_energy_change_convergence_criterion
    multisweep_energy_change_convergence_criterion
    tolerance
    maximum_number_of_iterations
    number_of_levels
    operator_site_tensors
    projectors
    = flip evalStateT undefined $
        solveForMultipleLevelsWithCallbacks
            alwaysDeclareVictory
            (newChainCreator
                (\number_of_projectors ->
                    put (number_of_projectors+1)
                    >>
                    return (number_of_projectors+1)
                )
                operator_site_tensors
            )
            singleIncrementBandwidthIncreaser
            ignoreSweepCallback
            ignoreSiteCallback
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            maximum_number_of_iterations
            number_of_levels
            projectors

solveForMultipleLevels_ = solveForMultipleLevels 1e-7 1e-7 1e-10 1000
-- @nonl
-- @-node:gcross.20091119150241.1853:solveForMultipleLevels
-- @-node:gcross.20091119150241.1851:Multiple levels
-- @-node:gcross.20091118213523.1814:Functions
-- @-others
-- @-node:gcross.20091118213523.1809:@thin Algorithms.hs
-- @-leo
