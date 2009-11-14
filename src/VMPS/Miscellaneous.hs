-- @+leo-ver=4-thin
-- @+node:gcross.20091113142219.1688:@thin Miscellaneous.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113142219.1694:<< Language extensions >>
-- @-node:gcross.20091113142219.1694:<< Language extensions >>
-- @nl

module VMPS.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20091113142219.1696:<< Import needed modules >>
import Data.Complex
-- @-node:gcross.20091113142219.1696:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091113142219.1691:Classes
-- @+node:gcross.20091113142219.1692:AlmostEq
class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = abs (x-y) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
    (a :+ b) ~= (c :+ d) = (a ~= c) && (b ~= d)

x /~ y = not (x ~= y)
-- @-node:gcross.20091113142219.1692:AlmostEq
-- @-node:gcross.20091113142219.1691:Classes
-- @-others
-- @-node:gcross.20091113142219.1688:@thin Miscellaneous.hs
-- @-leo
