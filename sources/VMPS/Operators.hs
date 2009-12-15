-- @+leo-ver=4-thin
-- @+node:gcross.20091201134050.1634:@thin Operators.hs
-- @@language Haskell

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VMPS.Operators where

import Data.Array.Storable
import Data.Complex
import Data.Int
import Data.Vec as Vec

import VMPS.Miscellaneous

newtype SingleQubitOperator = SQO (Vec4 (Complex Double)) deriving (Num,Eq,Show)

infix 5 *:
(*:) :: Complex Double -> SingleQubitOperator -> SingleQubitOperator
(*:) c (SQO v) = SQO (Vec.map (c*) v)

type OperatorSpecification = [((Int32,Int32),SingleQubitOperator)]

instance Pinnable SingleQubitOperator where
    withPinnedTensor (SQO (    p11 :. p12
                            :. p21 :. p22
                            :. () )) thunk = do
        operator <- newArray ((1,1),(2,2)) 0
        writeArray operator (1,1) p11
        writeArray operator (1,2) p12
        writeArray operator (2,1) p21
        writeArray operator (2,2) p22
        withStorableArray operator thunk

-- @-node:gcross.20091201134050.1634:@thin Operators.hs
-- @-leo
