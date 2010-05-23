-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1839:@thin Models.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091118213523.1840:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20091118213523.1840:<< Language extensions >>
-- @nl

module VMPS.Qubits.Models where

-- @<< Import needed modules >>
-- @+node:gcross.20091118213523.1841:<< Import needed modules >>
import Data.Int

import VMPS.Operators
import VMPS.Operators.Dimensions
import VMPS.Models
import VMPS.Qubits.Operators
import VMPS.Tensors
-- @-node:gcross.20091118213523.1841:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100505152919.1741:Models
-- @+node:gcross.20100505152919.1743:makeTransverseIsingOperatorSiteTensors
makeTransverseIsingModelOperatorSiteTensors coupling_stringth =
    makeSimpleModelOperatorSiteTensors 3
        [(1 ⇨ 1) pI
        ,(1 ⇨ 3) pZ
        ,(1 ⇨ 2) pX
        ,(2 ⇨ 3) (-coupling_stringth *: pX)
        ,(3 ⇨ 3) pI
        ]
-- @nonl
-- @-node:gcross.20100505152919.1743:makeTransverseIsingOperatorSiteTensors
-- @-node:gcross.20100505152919.1741:Models
-- @-others
-- @-node:gcross.20091118213523.1839:@thin Models.hs
-- @-leo
