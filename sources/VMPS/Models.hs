-- @+leo-ver=4-thin
-- @+node:gcross.20100505152919.1729:@thin Models.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100505152919.1730:<< Language extensions >>
-- @-node:gcross.20100505152919.1730:<< Language extensions >>
-- @nl

module VMPS.Models where

-- @<< Import needed modules >>
-- @+node:gcross.20100505152919.1731:<< Import needed modules >>
import Data.Int

import VMPS.Operators
import VMPS.Tensors
-- @-node:gcross.20100505152919.1731:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100505152919.1732:Functions
-- @+node:gcross.20100505152919.1733:startingFrom/endingWith
startingFrom :: OperatorSiteSpecification n -> OperatorSiteSpecification n
startingFrom = filter $ (== 1) . fst . fst

endingWith :: Int -> OperatorSiteSpecification n -> OperatorSiteSpecification n
endingWith ending_index list = [((index1,1),matrix) | ((index1,index2),matrix) <- list, index2 == toEnum ending_index]
-- @-node:gcross.20100505152919.1733:startingFrom/endingWith
-- @+node:gcross.20100505152919.1734:-->
(-->) :: Int32 -> Int32 -> SingleSiteOperator n -> ((Int32,Int32),SingleSiteOperator n)
(-->) i j sqo = ((i,j),sqo)
-- @-node:gcross.20100505152919.1734:-->
-- @-node:gcross.20100505152919.1732:Functions
-- @+node:gcross.20100505152919.1735:Models
-- @+node:gcross.20100505152919.1736:makeLocalOperatorSiteTensors
makeLocalOperatorSiteTensors :: OperatorDimension n => [SingleSiteOperator n] -> [OperatorSiteTensor]
makeLocalOperatorSiteTensors = map (makeOperatorSiteTensorFromSpecification 1 1 . (:[]) . (1 --> 1))
-- @-node:gcross.20100505152919.1736:makeLocalOperatorSiteTensors
-- @+node:gcross.20100505152919.1737:makeSimpleModelOperatorSiteTensors
makeSimpleModelOperatorSiteTensors :: OperatorDimension n => Int -> OperatorSiteSpecification n -> Int -> [OperatorSiteTensor]
makeSimpleModelOperatorSiteTensors bandwidth middle_model number_of_sites =
    makeModelWithSpecialEndpointsOperatorSiteTensors
        bandwidth
        (startingFrom middle_model)
        middle_model
        (endingWith bandwidth middle_model)
        number_of_sites
-- @-node:gcross.20100505152919.1737:makeSimpleModelOperatorSiteTensors
-- @+node:gcross.20100505152919.1738:makeModelWithSpecialEndpointsOperatorSiteTensors
makeModelWithSpecialEndpointsOperatorSiteTensors ::
    OperatorDimension n =>
    Int ->
    OperatorSiteSpecification n ->
    OperatorSiteSpecification n ->
    OperatorSiteSpecification n ->
    Int ->
    [OperatorSiteTensor]
makeModelWithSpecialEndpointsOperatorSiteTensors bandwidth left_model middle_model right_model number_of_sites =
    [makeOperatorSiteTensorFromSpecification 1 bandwidth $ left_model]
    ++
    replicate (number_of_sites-2) (makeOperatorSiteTensorFromSpecification bandwidth bandwidth $ middle_model)
    ++
    [makeOperatorSiteTensorFromSpecification bandwidth 1 $ right_model]
-- @-node:gcross.20100505152919.1738:makeModelWithSpecialEndpointsOperatorSiteTensors
-- @+node:gcross.20100505152919.1759:makeMagneticFieldOperatorSiteTensors
makeExternalFieldOperatorSiteTensors :: OperatorDimension n => SingleSiteOperator n -> Int -> [OperatorSiteTensor]
makeExternalFieldOperatorSiteTensors field_operator =
    makeSimpleModelOperatorSiteTensors 2
        [(1 --> 1) identity
        ,(1 --> 2) field_operator
        ,(2 --> 2) identity
        ]
-- @-node:gcross.20100505152919.1759:makeMagneticFieldOperatorSiteTensors
-- @-node:gcross.20100505152919.1735:Models
-- @-others
-- @-node:gcross.20100505152919.1729:@thin Models.hs
-- @-leo
