-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1832:@thin Pauli.hs
-- @@language Haskell

module VMPS.Pauli where

import Data.Int

data Pauli = I | X | Y | Z

type PauliList = [((Int32,Int32),(Double,Pauli))]
-- @-node:gcross.20091118213523.1832:@thin Pauli.hs
-- @-leo
