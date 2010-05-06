-- @+leo-ver=4-thin
-- @+node:gcross.20100505152919.1750:@thin Dimensions.hs
-- @@language Haskell

{-# LANGUAGE TypeSynonymInstances #-}

module VMPS.Operators.Dimensions where

import Data.Vec ((:.)(..))
import Data.Vec.Nat

import VMPS.Operators

instance OperatorDimension N1 where
    identity = SingleSiteOperator $
        (1 :. ()) :.
              ()

instance OperatorDimension N2 where
    identity = SingleSiteOperator $
        (1 :. 0 :. ()) :.
        (0 :. 1 :. ()) :.
                   ()

instance OperatorDimension N3 where
    identity = SingleSiteOperator $
        (1 :. 0 :. 0 :. ()) :.
        (0 :. 1 :. 0 :. ()) :.
        (0 :. 0 :. 1 :. ()) :.
                        ()

instance OperatorDimension N4 where
    identity = SingleSiteOperator $
        (1 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 1 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 1 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 1 :. ()) :.
                             ()

instance OperatorDimension N5 where
    identity = SingleSiteOperator $
        (1 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 1 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 1 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 1 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 1 :. ()) :.
                                  ()

instance OperatorDimension N6 where
    identity = SingleSiteOperator $
        (1 :. 0 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 1 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 1 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 1 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 1 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 0 :. 1 :. ()) :.
                                       ()

instance OperatorDimension N7 where
    identity = SingleSiteOperator $
        (1 :. 0 :. 0 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 1 :. 0 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 1 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 1 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 1 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 0 :. 1 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 0 :. 0 :. 1 :. ()) :.
                                            ()

instance OperatorDimension N8 where
    identity = SingleSiteOperator $
        (1 :. 0 :. 0 :. 0 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 1 :. 0 :. 0 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 1 :. 0 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 1 :. 0 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 1 :. 0 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 0 :. 1 :. 0 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 0 :. 0 :. 1 :. 0 :. ()) :.
        (0 :. 0 :. 0 :. 0 :. 0 :. 0 :. 0 :. 1 :. ()) :.
                                                 ()
-- @-node:gcross.20100505152919.1750:@thin Dimensions.hs
-- @-leo
