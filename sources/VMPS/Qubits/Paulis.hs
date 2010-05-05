-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1832:@thin Paulis.hs
-- @@language Haskell

module VMPS.Qubits.Paulis (pI,pX,pY,pZ) where

import Data.Complex
import Data.Vec

import VMPS.Qubits.Operators

i :: Complex Double
i = (0 :+ 1)

pI :: SingleQubitOperator
pX :: SingleQubitOperator
pY :: SingleQubitOperator
pZ :: SingleQubitOperator

pI = SQO (  1 :.  0
         :. 0 :.  1
   :. () )

pX = SQO (  0 :.  1
         :. 1 :.  0
   :. () )

pY = SQO (  0 :.(-i)
         :. i :.  0
   :. () )

pZ = SQO (  1 :.  0
         :. 0 :.(-1)
   :. () )
-- @-node:gcross.20091118213523.1832:@thin Paulis.hs
-- @-leo
