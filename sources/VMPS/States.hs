-- @+leo-ver=4-thin
-- @+node:gcross.20091123113033.1622:@thin States.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091123113033.1623:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
-- @-node:gcross.20091123113033.1623:<< Language extensions >>
-- @nl

module VMPS.States where

-- @<< Import needed modules >>
-- @+node:gcross.20091123113033.1628:<< Import needed modules >>
import Control.Exception

import Data.Complex
import Data.List
import Data.Typeable

import VMPS.Operators
import VMPS.Tensors
import VMPS.Wrappers
-- @-node:gcross.20091123113033.1628:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091123113033.1629:Types
-- @+node:gcross.20091123113033.1631:CanonicalStateRepresentation
data CanonicalStateRepresentation =
    CanonicalStateRepresentation
        {   canonicalStateNumberOfSites :: !Int
        ,   canonicalStateFirstSiteTensor :: !UnnormalizedStateSiteTensor
        ,   canonicalStateRestSiteTensors :: ![RightAbsorptionNormalizedStateSiteTensor]
        } deriving (Typeable)

-- @-node:gcross.20091123113033.1631:CanonicalStateRepresentation
-- @-node:gcross.20091123113033.1629:Types
-- @+node:gcross.20091123113033.1626:Functions
-- @+node:gcross.20091211120042.1696:applySingleSiteOperators
applySingleSiteOperators :: [SingleQubitOperator] -> CanonicalStateRepresentation -> CanonicalStateRepresentation
applySingleSiteOperators operators old_state = assert (length operators >= canonicalStateNumberOfSites old_state) $
    old_state
        {   canonicalStateFirstSiteTensor =
                applySingleSiteOperator
                    (head operators)
                    (canonicalStateFirstSiteTensor old_state)
        ,   canonicalStateRestSiteTensors =
                zipWith applySingleSiteOperator
                    (tail operators)
                    (canonicalStateRestSiteTensors old_state)
        }
-- @-node:gcross.20091211120042.1696:applySingleSiteOperators
-- @+node:gcross.20091123113033.1635:expectationOf
expectationOf :: [OperatorSiteTensor] -> CanonicalStateRepresentation -> Complex Double
expectationOf operator_site_tensors state
    | length operator_site_tensors /= canonicalStateNumberOfSites state
        = error "The number of operator sites and state sites do not match!"
    | otherwise
        = computeExpectation
            trivial_left_boundary
            (canonicalStateFirstSiteTensor state)
            (head operator_site_tensors)
          .
          foldl' -- '
            (\right_boundary (state_site_tensor,operator_site_tensor) ->
                contractSOSRight right_boundary state_site_tensor operator_site_tensor
            )
            trivial_right_boundary
          .
          reverse
          .
          zip (canonicalStateRestSiteTensors state)
          .
          tail
          $
          operator_site_tensors
-- @-node:gcross.20091123113033.1635:expectationOf
-- @-node:gcross.20091123113033.1626:Functions
-- @-others
-- @-node:gcross.20091123113033.1622:@thin States.hs
-- @-leo
