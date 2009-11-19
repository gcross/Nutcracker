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

import VMPS.EnergyMinimizationChain
-- @-node:gcross.20091118213523.1812:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091118213523.1813:Types
-- @+node:gcross.20091118213523.1815:SweepDirection
data SweepDirection = SweepingRight | SweepingLeft
-- @-node:gcross.20091118213523.1815:SweepDirection
-- @-node:gcross.20091118213523.1813:Types
-- @+node:gcross.20091118213523.1814:Functions
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
-- @-node:gcross.20091118213523.1814:Functions
-- @-others
-- @-node:gcross.20091118213523.1809:@thin Algorithms.hs
-- @-leo
