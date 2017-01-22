{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.EC.Discrete.Class(
       DiscreteEC(..),
       DiscreteECGroup(..)
       ) where

import Data.Bits
import Math.EC.Discrete.Point.Class

-- | Class for discrete elliptic curves (i.e. elliptic curves mod p,
-- for some integer p)
class DiscreteEC curvety where
  -- | The modulus of the discrete elliptic curve.
  modulus :: curvety -> Integer

  -- | Number of bits in the modulus
  bits :: curvety -> Int
  bits curve =
    let
      p = modulus curve

      bits' n
        | p `shiftR` n == 0 = n
        | otherwise = bits' (n + 1)
    in
      bits' 1

  -- | Get the discriminant of the elliptic curve.
  discriminant :: curvety -> Integer

  -- | Get the j-invariant of the elliptic curve.
  j_invariant :: curvety -> Integer

  -- | Write out a gnuplot file with the curve plotted.
  plotCurve :: curvety -> FilePath -> IO ()

  -- | Write out a gnuplot file with all the points plotted.
  plotPoints  :: curvety -> FilePath -> IO ()

-- | Class for group operations on discrete elliptic curves.
class (Point pointty, Eq pointty, DiscreteEC curvety) =>
      DiscreteECGroup pointty curvety where
  -- | The point addition operation.
  add :: curvety -> pointty -> pointty -> pointty

  double :: curvety -> pointty -> pointty

  -- | The scalar multiply operation.
  mul :: curvety -> Integer -> pointty -> pointty
  mul curve n point
    | valid curve point && n < modulus curve =
      let
        step m (r0, r1)
          | testBit n m =
            (add curve r0 r1, double curve r1)
          | otherwise =
            (double curve r0, add curve r0 r1)

        ladder 0 rs = step 0 rs
        ladder m rs = ladder (m - 1) (step m rs)

        (out, _) = ladder (bits curve) (infinity, point)
      in
        out

    | valid curve point = mul curve (n `mod` modulus curve) point
    | otherwise = error ("Invalid curve point " ++ defaultShow point)

  -- | Check whether a given point is a valid point on the discrete
  -- curve.
  valid :: curvety -> pointty -> Bool

  -- | Enumerate all points in the curve.  This will take a very long
  -- time for curves with a high modulus!
  enumerate :: curvety -> [pointty]

  -- | Generate the entire addition group from a starting point.  This
  -- will take a very long time for curves with a high modulus!
  generate :: curvety -> pointty -> [pointty]
