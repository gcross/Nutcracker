-- @+leo-ver=4-thin
-- @+node:gcross.20100505152919.1709:@thin Operators.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100505152919.1757:<< Language extensions >>
{-# LANGUAGE TypeSynonymInstances #-}
-- @-node:gcross.20100505152919.1757:<< Language extensions >>
-- @nl

module VMPS.Qubits.Operators (QubitOperatorSiteSpecification,SingleQubitOperator,pI,pX,pY,pZ) where

-- @<< Import needed modules >>
-- @+node:gcross.20100505152919.1710:<< Import needed modules >>
import Data.Complex
import Data.Vec ((:.)(..))
import Data.Vec.Nat (N2)

import VMPS.Operators
import VMPS.Operators.Dimensions
-- @-node:gcross.20100505152919.1710:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100505152919.1711:Types
-- @+node:gcross.20100505152919.1713:QubitOperatorSiteSpecification
type QubitOperatorSiteSpecification = OperatorSiteSpecification N2
-- @-node:gcross.20100505152919.1713:QubitOperatorSiteSpecification
-- @+node:gcross.20100505152919.1712:SingleQubitOperator
type SingleQubitOperator = SingleSiteOperator N2
-- @-node:gcross.20100505152919.1712:SingleQubitOperator
-- @-node:gcross.20100505152919.1711:Types
-- @+node:gcross.20100505152919.1714:Pauli operators
i :: Complex Double
i = (0 :+ 1)

pI,pX,pY,pZ :: SingleQubitOperator

pI = SingleSiteOperator $
     (1 :. 0 :. ()) :.
     (0 :. 1 :. ()) :.
                ()

pX = SingleSiteOperator $
     (0 :. 1 :. ()) :.
     (1 :. 0 :. ()) :.
                ()

pY = SingleSiteOperator $
     (0 :.(-i) :. ()) :.
     (i :.  0  :. ()) :.
                  ()

pZ = SingleSiteOperator $
     (1 :.  0  :. ()) :.
     (0 :.(-1) :. ()) :.
                  ()
-- @-node:gcross.20100505152919.1714:Pauli operators
-- @-others
-- @-node:gcross.20100505152919.1709:@thin Operators.hs
-- @-leo
