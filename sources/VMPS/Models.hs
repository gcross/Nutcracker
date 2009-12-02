-- @+leo-ver=4-thin
-- @+node:gcross.20091118213523.1839:@thin Models.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091118213523.1840:<< Language extensions >>
-- @-node:gcross.20091118213523.1840:<< Language extensions >>
-- @nl

module VMPS.Models where

-- @<< Import needed modules >>
-- @+node:gcross.20091118213523.1841:<< Import needed modules >>
import Data.Int

import VMPS.Operators
import VMPS.Paulis
import VMPS.Tensors
-- @-node:gcross.20091118213523.1841:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091118213523.1850:Functions
-- @+node:gcross.20091118213523.1843:startingFrom/endingWith
startingFrom :: OperatorSpecification -> OperatorSpecification
startingFrom = filter $ (== 1) . fst . fst

endingWith :: Int -> OperatorSpecification -> OperatorSpecification
endingWith ending_index list = [((index1,1),matrix) | ((index1,index2),matrix) <- list, index2 == toEnum ending_index]
-- @-node:gcross.20091118213523.1843:startingFrom/endingWith
-- @+node:gcross.20091118213523.1836:-->
(-->) :: Int32 -> Int32 -> SingleQubitOperator -> ((Int32,Int32),SingleQubitOperator)
(-->) i j sqo = ((i,j),sqo)
-- @-node:gcross.20091118213523.1836:-->
-- @-node:gcross.20091118213523.1850:Functions
-- @+node:gcross.20091118213523.1849:Models
-- @+node:gcross.20091120112621.1587:makeSimpleModelOperatorSiteTensors
makeSimpleModelOperatorSiteTensors :: Int -> OperatorSpecification -> Int -> [OperatorSiteTensor]
makeSimpleModelOperatorSiteTensors bandwidth middle_model number_of_sites =
    makeModelWithSpecialEndpointsOperatorSiteTensors
        bandwidth
        (startingFrom middle_model)
        middle_model
        (endingWith bandwidth middle_model)
        number_of_sites
-- @-node:gcross.20091120112621.1587:makeSimpleModelOperatorSiteTensors
-- @+node:gcross.20091123113033.1653:makeModelWithSpecialEndpointsOperatorSiteTensors
makeModelWithSpecialEndpointsOperatorSiteTensors :: Int -> OperatorSpecification -> OperatorSpecification -> OperatorSpecification -> Int -> [OperatorSiteTensor]
makeModelWithSpecialEndpointsOperatorSiteTensors bandwidth left_model middle_model right_model number_of_sites =
    [makeOperatorSiteTensorFromPaulis 1 bandwidth $ left_model]
    ++
    replicate (number_of_sites-2) (makeOperatorSiteTensorFromPaulis bandwidth bandwidth $ middle_model)
    ++
    [makeOperatorSiteTensorFromPaulis bandwidth 1 $ right_model]
-- @-node:gcross.20091123113033.1653:makeModelWithSpecialEndpointsOperatorSiteTensors
-- @+node:gcross.20091118213523.1851:makeMagneticFieldOperatorSiteTensors
makeMagneticFieldOperatorSiteTensors =
    makeSimpleModelOperatorSiteTensors 2
        [(1 --> 1) pI
        ,(1 --> 2) pZ
        ,(2 --> 2) pI
        ]
-- @-node:gcross.20091118213523.1851:makeMagneticFieldOperatorSiteTensors
-- @+node:gcross.20091118213523.1852:makeTransverseIsingOperatorSiteTensors
makeTransverseIsingModelOperatorSiteTensors coupling_stringth =
    makeSimpleModelOperatorSiteTensors 3
        [(1 --> 1) pI
        ,(1 --> 3) pZ
        ,(1 --> 2) pX
        ,(2 --> 3) (-coupling_stringth *: pX)
        ,(3 --> 3) pI
        ]
-- @-node:gcross.20091118213523.1852:makeTransverseIsingOperatorSiteTensors
-- @-node:gcross.20091118213523.1849:Models
-- @-others
-- @-node:gcross.20091118213523.1839:@thin Models.hs
-- @-leo
