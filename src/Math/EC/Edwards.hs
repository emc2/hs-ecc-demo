{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Edwards-form elliptic curves.
module Math.EC.Edwards(
       module Math.EC.Class,
       module Math.EC.Discrete.Class,

       Edwards(..),
       discreteEdwards
       ) where

import Data.Ratio
import Math.EC.Class
import Math.EC.Discrete.BasicPoint
import Math.EC.Discrete.Class
import Math.EC.Discrete.PrimeField
import Math.PrimeField

-- | A Edwards-form elliptic curve.  This is a curve of the
-- form
--
-- > y^2 + x^2 = 1 + D * x^2 * y^2 mod P
data Edwards coordty =
  Edwards {
    -- | The value of D.
    edwardsD :: !coordty
  }

-- | Create a discrete Edwards-form elliptic curve.  This is a
-- curve of the form
--
-- > y^2 + x^2 = 1 + D * x^2 * y^2 mod P
discreteEdwards :: Rational
                -- ^ The D coefficient.
                -> Integer
                -- ^ The modulus
                -> PrimeFieldEC (Edwards Integer)
                -- ^ The new curve.
discreteEdwards d p =
  let
      pfdiv = primeFieldDiv p
      dval = (numerator d) `pfdiv` (denominator d)
  in
    PrimeFieldEC { curve = Edwards { edwardsD = dval }, modulus = p }

instance EC BasicPoint (PrimeFieldEC (Edwards Integer)) where
  add ec Infinity p
    | valid ec p = p
    | otherwise = error ("Invalid curve point " ++ show p)
  add ec p Infinity
    | valid ec p = p
    | otherwise = error ("Invalid curve point " ++ show p)
  add ec @ PrimeFieldEC { curve = Edwards { edwardsD = d }, modulus = m }
      p1 @ BasicPoint { basicX = xp, basicY = yp }
      p2 @ BasicPoint { basicX = xq, basicY = yq }
    | valid ec p1 && valid ec p2 && xp /= xq && yp == yq = Infinity
    | valid ec p1 && valid ec p2 =
      let
        pfadd = primeFieldAdd m
        pfdiv = primeFieldDiv m
        pfmul = primeFieldMul m
        pfsub = primeFieldSub m

        xpyq = xp `pfmul` yq
        xqyp = xq `pfmul` yp
        dxxyy = d `pfmul` xpyq `pfmul` xqyp
        xval = (xpyq `pfadd` xqyp) `pfdiv` (1 `pfadd` dxxyy)

        xx = xp `pfmul` xq
        yy = yp `pfmul` yq
        yval = (yy `pfsub` xx) `pfdiv` (1 `pfsub` dxxyy)
     in
        BasicPoint { basicX = xval, basicY = yval }
    | valid ec p1 = error ("Invalid curve point " ++ show p2)
    | valid ec p2 = error ("Invalid curve point " ++ show p1)
    | otherwise = error ("Invalid curve points " ++ show p1 ++ ", " ++ show p2)

  double ec p = add ec p p

  mul = montgomeryLadder

  valid _ Infinity = True
  valid PrimeFieldEC { curve = Edwards { edwardsD = d }, modulus = m } p =
    let
      pfadd = primeFieldAdd m
      pfmul = primeFieldMul m

      x = coordX p
      y = coordY p
      rhs = (x `pfmul` x) `pfadd` (y `pfmul` y)
      lhs = 1 `pfadd` (d `pfmul` x `pfmul` x `pfmul` y `pfmul` y)
    in
      lhs == rhs


instance ECInfo (PrimeFieldEC (Edwards Integer)) where
  plotCurve PrimeFieldEC { curve = Edwards { edwardsD = d } } =
    let
      outstr = "f(x,y) = " ++ show d ++ " * x**2 * y**2 - x**2 - y**2" ++
               "set contour\n" ++
               "set cntrparam levels discrete 0\n" ++
               "set view 0,0\n" ++
               "set isosamples 500,500\n" ++
               "unset surface\n" ++
               "unset ztics\n" ++
               "splot f(x,y)\n"
    in
      (flip writeFile) outstr

instance DiscreteECInfo (PrimeFieldEC (Edwards Integer)) where
  discriminant PrimeFieldEC { curve = Edwards { edwardsD = d },
                              modulus = m } =
    let
      pfsub = primeFieldSub m
      pfmul = primeFieldMul m
    in
      d `pfmul` (1 `pfsub` d)

  j_invariant _ = error "Not implemented"
  plotPoints _ = error "Not implemented"

instance DiscreteEC BasicPoint (PrimeFieldEC (Edwards Integer)) where
  enumerate = enumeratePoints

instance Show coeffty => Show (Edwards coeffty) where
  show Edwards { edwardsD = d } =
    "x^2 + y^2 = 1 + " ++ show d ++ " * x^2 * x^2"
