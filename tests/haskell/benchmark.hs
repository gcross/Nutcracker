-- @+leo-ver=4-thin
-- @+node:gcross.20091120134444.1970:@thin benchmark.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091120134444.1972:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091120134444.1972:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091120134444.1974:<< Import needed modules >>
import Criterion.Main

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.Miscellaneous
import VMPS.OperatorConstruction
import VMPS.Pauli
-- @-node:gcross.20091120134444.1974:<< Import needed modules >>
-- @nl

-- @+others
-- @-others

main = do
    three_site_chain_with_small_bandwidth <- generateRandomizedChain 2 2 (makeTransverseIsingModelOperatorSiteTensors 1 3)
    defaultMain
        -- @        << Benchmarks >>
        -- @+node:gcross.20091120134444.1975:<< Benchmarks >>
        [bench "activateRightNeighbor" (activateRightNeighbor,three_site_chain_with_small_bandwidth)
        -- @-node:gcross.20091120134444.1975:<< Benchmarks >>
        -- @nl
        ]
-- @-node:gcross.20091120134444.1970:@thin benchmark.hs
-- @-leo
