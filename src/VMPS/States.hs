-- @+leo-ver=4-thin
-- @+node:gcross.20091123113033.1622:@thin States.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091123113033.1623:<< Language extensions >>
-- @-node:gcross.20091123113033.1623:<< Language extensions >>
-- @nl

module VMPS.States where

-- @<< Import needed modules >>
-- @+node:gcross.20091123113033.1628:<< Import needed modules >>
import Data.List

import VMPS.Tensors
import VMPS.Wrappers
-- @nonl
-- @-node:gcross.20091123113033.1628:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091123113033.1629:Types
-- @+node:gcross.20091123113033.1631:CanonicalStateRepresentation
data CanonicalStateRepresentation =
    CanonicalStateRepresentation
        {   canonicalStateFirstSiteTensor :: !UnnormalizedStateSiteTensor
        ,   canonicalStateRestSiteTensors :: ![RightAbsorptionNormalizedStateSiteTensor]
        }
-- @-node:gcross.20091123113033.1631:CanonicalStateRepresentation
-- @-node:gcross.20091123113033.1629:Types
-- @+node:gcross.20091123113033.1626:Functions
-- @-node:gcross.20091123113033.1626:Functions
-- @-others
-- @-node:gcross.20091123113033.1622:@thin States.hs
-- @-leo
