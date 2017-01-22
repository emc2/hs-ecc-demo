{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Math.EC.Weierstrass(
       module Math.EC.Class,
       module Math.EC.Discrete.Class,

       Weierstrass(..),
       discreteWeierstrass
       ) where

import Data.Ratio
import Math.EC.Class
import Math.EC.Discrete.BasicPoint
import Math.EC.Discrete.Class
import Math.EC.Discrete.PrimeField
import Math.PrimeField

-- | A Weierstrass-form elliptic curve.  This is a curve of the
-- form
-- > y^2 = x^3 + Ax + B mod P
data Weierstrass coordty =
  Weierstrass {
    -- | The value of A.
    weierstrassA :: !coordty,
    -- | The value of B.
    weierstrassB :: !coordty
  }

-- | Create a discrete Weierstrass-form elliptic curve.  This is a
-- curve of the form
--
-- > y^2 = x^3 + Ax + B
discreteWeierstrass :: Rational
                    -- ^ The A coefficient.
                    -> Rational
                    -- ^ The B coefficient.
                    -> Integer
                    -- ^ The modulus
                    -> PrimeFieldEC (Weierstrass Integer)
                    -- ^ The new curve.
discreteWeierstrass a b p =
  let
      pfdiv = primeFieldDiv p

      aval = (numerator a) `pfdiv` (denominator a)
      bval = (numerator b) `pfdiv` (denominator b)
  in
    PrimeFieldEC { curve = Weierstrass { weierstrassA = aval,
                                         weierstrassB = bval },
                   modulus = p }

instance EC BasicPoint (PrimeFieldEC (Weierstrass Integer)) where
  add ec Infinity p
    | valid ec p = p
    | otherwise = error ("Invalid curve point " ++ show p)
  add ec p Infinity
    | valid ec p = p
    | otherwise = error ("Invalid curve point " ++ show p)
  add ec @ PrimeFieldEC { modulus = m }
      p1 @ BasicPoint { basicX = xp, basicY = yp }
      p2 @ BasicPoint { basicX = xq, basicY = yq }
    | valid ec p1 && valid ec p2 && xp == xq && yp /= yq = Infinity
    | valid ec p1 && valid ec p2 && p1 == p2 = double ec p1
    | valid ec p1 && valid ec p2 && p1 /= p2 =
      let
        pfdiv = primeFieldDiv m
        pfmul = primeFieldMul m
        pfsub = primeFieldSub m

        lambda = (yq `pfsub` yp) `pfdiv` (xq `pfsub` xp)
        xval = (lambda `pfmul` lambda) `pfsub` xp `pfsub` xq
        yval = (lambda `pfmul` (xp `pfsub` xval)) `pfsub` yp
      in
        BasicPoint { basicX = xval, basicY = yval }
    | valid ec p1 = error ("Invalid curve point " ++ show p2)
    | valid ec p2 = error ("Invalid curve point " ++ show p1)
    | otherwise = error ("Invalid curve points " ++ show p1 ++ ", " ++ show p2)

  double _ Infinity = Infinity
  double ec @ PrimeFieldEC { curve = Weierstrass { weierstrassA = a },
                             modulus = m }
         p @ BasicPoint { basicX = x, basicY = y }
    | valid ec p =
      let
        pfadd = primeFieldAdd m
        pfdiv = primeFieldDiv m
        pfmul = primeFieldMul m
        pfsub = primeFieldSub m

        lambda = ((3 `pfmul` x `pfmul` x) `pfadd` a) `pfdiv` (2 `pfmul` y)
        xval = (lambda `pfmul` lambda) `pfsub` (fromInteger (2 `pfmul` x))
        yval = (lambda `pfmul` (x `pfsub` xval)) `pfsub` y
      in
        BasicPoint { basicX = xval, basicY = yval }
    | otherwise = error ("Invalid curve point " ++ show p)

  mul = montgomeryLadder

  valid _ Infinity = True
  valid PrimeFieldEC { curve = Weierstrass { weierstrassA = a,
                                             weierstrassB = b },
                       modulus = m } p =
    let
      pfadd = primeFieldAdd m
      pfmul = primeFieldMul m

      x = coordX p
      y = coordY p
      rhs = (x `pfmul` x `pfmul` x) `pfadd` (a `pfmul` x) `pfadd` b
      lhs = y `pfmul` y
    in
      lhs == rhs

instance ECInfo (PrimeFieldEC (Weierstrass Integer)) where
  plotCurve PrimeFieldEC { curve = Weierstrass { weierstrassA = a,
                                                 weierstrassB = b } } =
    let
      outstr = "f(x,y) = x**3 + " ++ show a ++ " * x + " ++
               show b ++ " - y**2\n" ++
               "set contour\n" ++
               "set cntrparam levels discrete 0\n" ++
               "set view 0,0\n" ++
               "set isosamples 500,500\n" ++
               "unset surface\n" ++
               "unset ztics\n" ++
               "splot f(x,y)\n"
    in
      (flip writeFile) outstr

instance DiscreteECInfo (PrimeFieldEC (Weierstrass Integer)) where
  discriminant PrimeFieldEC { curve = Weierstrass { weierstrassA = a,
                                                    weierstrassB = b },
                              modulus = p } =
    let
      pfadd = primeFieldAdd p
      pfmul = primeFieldMul p
    in
      (-16) `pfmul` ((4 `pfmul` a `pfmul` a `pfmul` a) `pfadd`
                     (27 `pfmul` b `pfmul` b))

  j_invariant ec @ PrimeFieldEC { curve = Weierstrass { weierstrassA = a },
                                  modulus = p } =
    let
      pfmul = primeFieldMul p
      pfdiv = primeFieldDiv p
    in
      (1728 `pfmul` 4 `pfmul` a `pfmul` a `pfmul` a) `pfdiv`
      (discriminant ec)

  plotPoints _ = error "Not implemented"

instance DiscreteEC BasicPoint (PrimeFieldEC (Weierstrass Integer)) where
  enumerate = enumeratePoints

instance Show coeffty => Show (Weierstrass coeffty) where
  show Weierstrass { weierstrassA = a, weierstrassB = b } =
    "y^2 = x^3 + (" ++ show a ++ " * x) + " ++ show b
