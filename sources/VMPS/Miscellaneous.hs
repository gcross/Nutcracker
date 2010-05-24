-- @+leo-ver=4-thin
-- @+node:gcross.20091113142219.1688:@thin Miscellaneous.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091113142219.1694:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20091113142219.1694:<< Language extensions >>
-- @nl

module VMPS.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20091113142219.1696:<< Import needed modules >>
import Control.Arrow
import Control.Monad

import Data.Complex

import Foreign.Ptr
import Foreign.Storable

import Debug.Trace
-- @-node:gcross.20091113142219.1696:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091113142219.1691:Classes
-- @+node:gcross.20091113142219.1692:AlmostEq
class AlmostEq a where
    (≅) :: a → a → Bool

instance AlmostEq Double where
    x ≅ y = abs (x-y) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ≅ y = all (uncurry (≅)) $ zip x y

instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
    (a :+ b) ≅ (c :+ d) = (a ≅ c) && (b ≅ d)

x /~ y = not (x ≅ y)
-- @nonl
-- @-node:gcross.20091113142219.1692:AlmostEq
-- @+node:gcross.20091211120042.1687:Pinnable
class Pinnable a where
    withPinnedTensor :: a → (Ptr (Complex Double) → IO b) → IO b
-- @nonl
-- @-node:gcross.20091211120042.1687:Pinnable
-- @-node:gcross.20091113142219.1691:Classes
-- @+node:gcross.20091114174920.1744:Functions
-- @+node:gcross.20091114174920.1746:echo
echo x = trace (show x) x
-- @-node:gcross.20091114174920.1746:echo
-- @-node:gcross.20091114174920.1744:Functions
-- @+node:gcross.20091116222034.1788:Linear Algebra
-- @+node:gcross.20091116222034.1789:normalize
normalize :: Int → Ptr (Complex Double) → IO ()
normalize array_length p_array =
    norm array_length p_array
    >>=
    return . (:+ 0) . (1/)
    >>=
    divAll array_length p_array
  where
    divAll :: Int → Ptr (Complex Double) → Complex Double → IO ()
    divAll 0 _ _ = return ()
    divAll n p_array multiplier =
        peek p_array
        >>=
        poke p_array . (* multiplier)
        >>
        divAll (n-1) (nextComplexPtr p_array) multiplier
-- @nonl
-- @-node:gcross.20091116222034.1789:normalize
-- @+node:gcross.20091116222034.1790:dotArrays
dotArrays :: Int → Ptr (Complex Double) → Ptr (Complex Double) → IO (Complex Double)
dotArrays = go 0
  where
    go :: Complex Double → Int → Ptr (Complex Double) → Ptr (Complex Double) → IO (Complex Double)
    go acc 0 _ _ = return acc
    go acc n p1 p2 = do
        v1 ← peek p1
        v2 ← peek p2
        go (acc + ((conjugate v1) * v2)) (n-1) (nextComplexPtr p1) (nextComplexPtr p2)
-- @nonl
-- @-node:gcross.20091116222034.1790:dotArrays
-- @+node:gcross.20100520170650.1770:norm/normSquared
normSquared :: Int → Ptr (Complex Double) → IO Double
normSquared = go 0
  where
    go :: Double → Int → Ptr (Complex Double) → IO Double
    go acc 0 _ = return acc
    go acc n p =
        peek p
        >>=
        \(r :+ i) →
            go (acc + r*r + i*i) (n-1) (nextComplexPtr p)

norm size = fmap sqrt . normSquared size
-- @nonl
-- @-node:gcross.20100520170650.1770:norm/normSquared
-- @+node:gcross.20091116222034.1791:orthogonalize2
orthogonalize2 :: Int → Ptr (Complex Double) → Ptr (Complex Double) → IO ()
orthogonalize2 array_length p_array1 p_array2 =
    normalize array_length p_array1
    >>
    dotArrays array_length p_array1 p_array2
    >>=
    subtractOverlap array_length p_array1 p_array2
    >>
    normalize array_length p_array2
  where
    subtractOverlap :: Int → Ptr (Complex Double) → Ptr (Complex Double) → Complex Double → IO ()
    subtractOverlap 0 _ _ _ = return ()
    subtractOverlap n p1 p2 factor = do
        v1 ← peek p1
        v2 ← peek p2
        poke p2 (v2 - v1*factor)
        subtractOverlap (n-1) (nextComplexPtr p1) (nextComplexPtr p2) factor
-- @nonl
-- @-node:gcross.20091116222034.1791:orthogonalize2
-- @-node:gcross.20091116222034.1788:Linear Algebra
-- @+node:gcross.20091116222034.1782:Instances
-- @+node:gcross.20091116222034.1783:Storable (Complex a)
complexPtrToRealAndImagPtrs :: Ptr (Complex Double) → (Ptr Double, Ptr Double)
complexPtrToRealAndImagPtrs p_complex =
    let p_real_part = castPtr p_complex
        p_imag_part = plusPtr p_real_part . sizeOf $ (undefined :: Double)
    in (p_real_part, p_imag_part)

instance Storable (Complex Double) where
    sizeOf _ = 2 * sizeOf (undefined :: Double)
    alignment _ = 2 * alignment (undefined :: Double)
    peek = uncurry (liftM2 (:+)) . (peek *** peek) . complexPtrToRealAndImagPtrs
    poke p_complex (real_part :+ imag_part) =
        let (p_real_part, p_imag_part) = complexPtrToRealAndImagPtrs p_complex
        in poke p_real_part real_part >> poke p_imag_part imag_part

nextComplexPtr = (`plusPtr` sizeOf (undefined :: Complex Double))
-- @nonl
-- @-node:gcross.20091116222034.1783:Storable (Complex a)
-- @-node:gcross.20091116222034.1782:Instances
-- @-others
-- @-node:gcross.20091113142219.1688:@thin Miscellaneous.hs
-- @-leo
