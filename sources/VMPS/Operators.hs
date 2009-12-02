-- @+leo-ver=4-thin
-- @+node:gcross.20091201134050.1634:@thin Operators.hs
-- @@language Haskell

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VMPS.Operators where

import Data.Complex
import Data.Int
import Data.Vec as Vec

newtype SingleQubitOperator = SQO (Vec4 (Complex Double)) deriving (Num,Eq,Show)

(*:) :: Complex Double -> SingleQubitOperator -> SingleQubitOperator
(*:) c (SQO v) = SQO (Vec.map (c*) v)

type OperatorSpecification = [((Int32,Int32),SingleQubitOperator)]
-- @-node:gcross.20091201134050.1634:@thin Operators.hs
-- @-leo
