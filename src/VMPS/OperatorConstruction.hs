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
import VMPS.Tensors
-- @-node:gcross.20091118213523.1841:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091118213523.1850:Functions
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
-- @-node:gcross.20091118213523.1850:Functions
-- @+node:gcross.20091118213523.1849:Models
-- @+node:gcross.20091118213523.1851:magnetic_field
magnetic_field =
    [(1 --> 1) 1.0 I
    ,(1 --> 2) 1.0 Z
    ,(2 --> 2) 1.0 I
    ]
-- @-node:gcross.20091118213523.1851:magnetic_field
-- @+node:gcross.20091118213523.1852:makeTransverseIsingModel
makeTransverseIsingModel coupling_stringth =
    let middle_model =
              [(1 --> 1) 1 I
              ,(1 --> 3) 1 Z
              ,(1 --> 2) 1 X
              ,(2 --> 3) (-coupling_stringth) X
              ,(3 --> 3) 1 I
              ]
    in  (makeOperatorSiteTensorFromPaulis 1 3 . startingFrom $ middle_model
        ,makeOperatorSiteTensorFromPaulis 3 3 $ middle_model
        ,makeOperatorSiteTensorFromPaulis 3 1 . endingWith 3 $ middle_model
        )
-- @-node:gcross.20091118213523.1852:makeTransverseIsingModel
-- @-node:gcross.20091118213523.1849:Models
-- @-others
-- @-node:gcross.20091118213523.1839:@thin OperatorConstruction.hs
-- @-leo
