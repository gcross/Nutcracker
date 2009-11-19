-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1839:@thin OperatorConstruction.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091118213523.1840:<< Language extensions >>
-- @-node:gcross.20091118213523.1840:<< Language extensions >>
-- @nl

module VMPS.OperatorConstruction where

-- @<< Import needed modules >>
-- @+node:gcross.20091118213523.1841:<< Import needed modules >>
import Data.Int

import VMPS.Pauli
-- @-node:gcross.20091118213523.1841:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091118213523.1843:startingFrom/endingWith
startingFrom :: PauliList -> PauliList
startingFrom = filter $ (== 1) . fst . fst

endingWith :: Int32 -> PauliList -> PauliList
endingWith ending_index list = [((index1,1),matrix) | ((index1,index2),matrix) <- list, index2 == ending_index]
-- @-node:gcross.20091118213523.1843:startingFrom/endingWith
-- @+node:gcross.20091118213523.1836:-->
(-->) :: Int32 -> Int32 -> Double -> Pauli -> ((Int32,Int32),(Double,Pauli))
(-->) i j coefficient pauli = ((i,j),(coefficient,pauli))
-- @-node:gcross.20091118213523.1836:-->
-- @-others
-- @-node:gcross.20091118213523.1839:@thin OperatorConstruction.hs
-- @-leo
