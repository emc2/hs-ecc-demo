{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Discrete Elliptic curves over a prime finite field.
module Math.EC.Discrete.PrimeField(
       module Math.EC.Discrete.Class,

       PrimeFieldEC(..),

       primeFieldEC,
       montgomeryLadder,
       enumeratePoints,
       ) where

import Data.Bits
import Math.EC.Discrete.Class
import Math.EC.Point.Class

-- | Discrete Elliptic curves over a prime finite field.
data PrimeFieldEC curvety =
  PrimeFieldEC {
    -- | The underlying curve.
    curve :: !curvety,
    -- | The modulus (must be a prime number!).
    modulus :: !Integer
  }

-- | Get number of bits from the modulus.
bits :: PrimeFieldEC curvety -> Int
bits ec =
  let
    p = modulus ec

    bits' n
      | p `shiftR` n == 0 = n
      | otherwise = bits' (n + 1)
  in
    bits' 1

primeFieldEC :: curvety -> Integer -> PrimeFieldEC curvety
primeFieldEC c p = PrimeFieldEC { curve = c, modulus = p }

-- | The Montgomery ladder point-multiply algorithm.  It goes down the
-- scalar highest bit to lowest, peforming an addition and a double
-- operation at each step.  This has the significant advantage of
-- being constant-time
montgomeryLadder :: (Point Integer pointty,
                     EC pointty (PrimeFieldEC curvety)) =>
                    PrimeFieldEC curvety -> Integer -> pointty -> pointty
montgomeryLadder ec n p
  | valid ec p && n < modulus ec =
    let
      step m (r0, r1)
        | n `testBit` m =
          (add ec r0 r1, double ec r1)
        | otherwise =
          (double ec r0, add ec r0 r1)

      ladder 0 rs = step 0 rs
      ladder m rs = ladder (m - 1) (step m rs)

      (out, _) = ladder (bits ec) (infinity, p)
    in
      out
  | otherwise = error ("Invalid curve p (" ++
                       show (coordX p :: Integer) ++ ", " ++
                       show (coordY p :: Integer) ++ ")")

enumeratePoints :: (Point Integer pointty,
                    DiscreteEC pointty (PrimeFieldEC curvety)) =>
                   PrimeFieldEC curvety -> [pointty]
enumeratePoints ec =
  let
    p = modulus ec
  in
    filter (valid ec) [point x y | x <- [0..p-1], y <- [0..p-1]]

instance Show curvety => Show (PrimeFieldEC curvety) where
  show PrimeFieldEC { curve = c, modulus = p } = show c ++ " mod " ++ show p
