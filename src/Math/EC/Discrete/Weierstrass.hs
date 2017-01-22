{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.EC.Discrete.Weierstrass(
       module Math.EC.Discrete.Class,

       Weierstrass(..),
       weierstrass
       ) where

import Data.Ratio
import Math.EC.Discrete.BasicPoint
import Math.EC.Discrete.Class
import Math.EC.Discrete.Point.Class
import Math.PrimeField

-- | A Weierstrass-form elliptic curve.  This is a curve of the
-- form
-- > y^2 = x^3 + Ax + B mod P
data Weierstrass =
  Weierstrass {
    -- | The value of A.
    weierstrassA :: !Integer,
    -- | The value of B.
    weierstrassB :: !Integer,
    -- | The modulus.
    weierstrassModulus :: !Integer
  }

-- | Create a Weierstrass-form elliptic curve.  This is a curve of the
-- form
-- > y^2 = x^3 + Ax + B mod P
weierstrass :: Rational
            -- ^ The A coefficient.
            -> Rational
            -- ^ The B coefficient.
            -> Integer
            -- ^ The modulus P.
            -> Weierstrass
            -- ^ The new curve.
weierstrass a b p =
  let
      pfdiv = primeFieldDiv p

      aval = (numerator a) `pfdiv` (denominator a)
      bval = (numerator b) `pfdiv` (denominator b)
  in
    Weierstrass { weierstrassA = aval, weierstrassB = bval,
                  weierstrassModulus = p }

instance DiscreteEC Weierstrass where
  discriminant Weierstrass { weierstrassA = a, weierstrassB = b,
                             weierstrassModulus = p } =
    let
      pfadd = primeFieldAdd p
      pfmul = primeFieldMul p
    in
      (-16) `pfmul` ((4 `pfmul` a `pfmul` a `pfmul` a) `pfadd`
                     (27 `pfmul` b `pfmul` b))

  j_invariant curve @ Weierstrass { weierstrassA = a, weierstrassModulus = p } =
    let
      pfmul = primeFieldMul p
      pfdiv = primeFieldDiv p
    in
      (1728 `pfmul` 4 `pfmul` a `pfmul` a `pfmul` a) `pfdiv`
      (discriminant curve)

  modulus Weierstrass { weierstrassModulus = p } = p

  plotCurve Weierstrass { weierstrassA = a, weierstrassB = b } =
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

  plotPoints _ = error "Not implemented"

instance DiscreteECGroup BasicPoint Weierstrass where
  add curve Infinity point
    | valid curve point = point
    | otherwise = error ("Invalid curve point " ++ show point)
  add curve point Infinity
    | valid curve point = point
    | otherwise = error ("Invalid curve point " ++ show point)
  add curve @ Weierstrass { weierstrassModulus = p }
      p1 @ BasicPoint { basicX = xp, basicY = yp }
      p2 @ BasicPoint { basicX = xq, basicY = yq }
    | valid curve p1 && valid curve p2 && xp == xq && yp /= yq = Infinity
    | valid curve p1 && valid curve p2 && p1 == p2 = double curve p1
    | valid curve p1 && valid curve p2 && p1 /= p2 =
      let
        pfdiv = primeFieldDiv p
        pfmul = primeFieldMul p
        pfsub = primeFieldSub p

        lambda = (yq `pfsub` yp) `pfdiv` (xq `pfsub` xp)
        xval = (lambda `pfmul` lambda) `pfsub` xp `pfsub` xq
        yval = (lambda `pfmul` (xp `pfsub` xval)) `pfsub` yp
      in
        BasicPoint { basicX = xval, basicY = yval }
    | valid curve p1 = error ("Invalid curve point " ++ show p2)
    | valid curve p2 = error ("Invalid curve point " ++ show p1)
    | otherwise = error ("Invalid curve points " ++ show p1 ++ ", " ++ show p2)

  double _ Infinity = Infinity
  double curve @ Weierstrass { weierstrassA = a, weierstrassModulus = p }
         point @ BasicPoint { basicX = x, basicY = y }
    | valid curve point =
      let
        pfadd = primeFieldAdd p
        pfdiv = primeFieldDiv p
        pfmul = primeFieldMul p
        pfsub = primeFieldSub p

        lambda = ((3 `pfmul` x `pfmul` x) `pfadd` a) `pfdiv` (2 `pfmul` y)
        xval = (lambda `pfmul` lambda) `pfsub` (fromInteger (2 `pfmul` x))
        yval = (lambda `pfmul` (x `pfsub` xval)) `pfsub` y
      in
        BasicPoint { basicX = xval, basicY = yval }
    | otherwise = error ("Invalid curve point " ++ show point)

  valid _ Infinity = True
  valid Weierstrass { weierstrassA = a, weierstrassB = b,
                      weierstrassModulus = p } point =
    let
      pfadd = primeFieldAdd p
      pfmul = primeFieldMul p

      x = coordX point
      y = coordY point
      rhs = (x `pfmul` x `pfmul` x) `pfadd` (a `pfmul` x) `pfadd` b
      lhs = (y `pfmul` y) `mod` p
    in
      lhs == rhs

  enumerate curve @ Weierstrass { weierstrassModulus = p } =
    filter (valid curve) [BasicPoint { basicX = x, basicY = y } |
                          x <- [0..p-1], y <- [0..p-1]]

  generate curve base =
    let
      genfunc accum point =
        let
          newpoint = add curve point base
        in
          if newpoint == Infinity
            then reverse accum
            else genfunc (newpoint : accum) newpoint
    in
      genfunc [base] base

instance Show Weierstrass where
  show Weierstrass { weierstrassA = a, weierstrassB = b,
                     weierstrassModulus = p } =
    "y^2 = x^3 + (" ++ show a ++ " * x) + " ++ show b ++ " mod " ++ show p
