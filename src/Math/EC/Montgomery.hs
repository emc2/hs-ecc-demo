{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Montgomery-form elliptic curves.
module Math.EC.Montgomery(
       module Math.EC.Class,
       module Math.EC.Discrete.Class,

       Montgomery(..),
       discreteMontgomery
       ) where

import Data.Ratio
import Math.EC.Class
import Math.EC.Discrete.BasicPoint
import Math.EC.Discrete.Class
import Math.EC.Discrete.PrimeField
import Math.PrimeField

-- | A Montgomery-form elliptic curve.  This is a curve of the
-- form
--
-- > B y^2 = x^3 + Ax^2 + x mod P
data Montgomery coordty =
  Montgomery {
    -- | The value of A.
    montgomeryA :: !coordty,
    -- | The value of B.
    montgomeryB :: !coordty
  }

-- | Create a discrete Montgomery-form elliptic curve.  This is a
-- curve of the form
--
-- > B y^2 = x^3 + Ax^2 + x mod P
discreteMontgomery :: Rational
                    -- ^ The A coefficient.
                    -> Rational
                    -- ^ The B coefficient.
                    -> Integer
                    -- ^ The modulus
                    -> PrimeFieldEC (Montgomery Integer)
                    -- ^ The new curve.
discreteMontgomery a b p =
  let
      pfdiv = primeFieldDiv p

      aval = (numerator a) `pfdiv` (denominator a)
      bval = (numerator b) `pfdiv` (denominator b)
  in
    PrimeFieldEC { curve = Montgomery { montgomeryA = aval,
                                        montgomeryB = bval },
                   modulus = p }

instance EC BasicPoint (PrimeFieldEC (Montgomery Integer)) where
  add ec Infinity p
    | valid ec p = p
    | otherwise = error ("Invalid curve point " ++ show p)
  add ec p Infinity
    | valid ec p = p
    | otherwise = error ("Invalid curve point " ++ show p)
  add ec @ PrimeFieldEC { curve = Montgomery { montgomeryA = a,
                                               montgomeryB = b },
                          modulus = m }
      p1 @ BasicPoint { basicX = xp, basicY = yp }
      p2 @ BasicPoint { basicX = xq, basicY = yq }
    | valid ec p1 && valid ec p2 && xp == xq && yp /= yq = Infinity
    | valid ec p1 && valid ec p2 && p1 == p2 = double ec p1
    | valid ec p1 && valid ec p2 && p1 /= p2 =
      let
        pfadd = primeFieldAdd m
        pfdiv = primeFieldDiv m
        pfmul = primeFieldMul m
        pfsub = primeFieldSub m

        xdiff = xq `pfsub` xp
        xdiff2 = xdiff `pfmul` xdiff
        xdiff3 = xdiff2 `pfmul` xdiff
        xydiff = (xq `pfmul` yp) `pfsub` (xp `pfmul` yq)
        xval = (b `pfmul` (xydiff `pfmul` xydiff)) `pfdiv`
               (xp `pfmul` xq `pfmul` xdiff2)
        ydiff = yq `pfsub` yp
        ydiff2 = ydiff `pfmul` ydiff
        ydiff3 = ydiff2 `pfmul` ydiff
        yval = ((((2 `pfmul` xp) `pfadd` xq `pfadd` a) `pfmul` ydiff) `pfdiv`
                xdiff) `pfsub`
               ((b `pfmul` ydiff3) `pfdiv` xdiff3) `pfsub` yp
      in
        BasicPoint { basicX = xval, basicY = yval }
    | valid ec p1 = error ("Invalid curve point " ++ show p2)
    | valid ec p2 = error ("Invalid curve point " ++ show p1)
    | otherwise = error ("Invalid curve points " ++ show p1 ++ ", " ++ show p2)

  double _ Infinity = Infinity
  double ec @ PrimeFieldEC { curve = Montgomery { montgomeryA = a,
                                                  montgomeryB = b },
                             modulus = m }
         p @ BasicPoint { basicX = x, basicY = y }
    | valid ec p =
      let
        pfadd = primeFieldAdd m
        pfdiv = primeFieldDiv m
        pfmul = primeFieldMul m
        pfsub = primeFieldSub m

        x2 = x `pfmul` x
        ax = a `pfmul` x
        x2_m1 = x2 `pfsub` 1
        x2_m1_2 = x2_m1 `pfmul` x2_m1
        ax_1 = ax `pfadd` 1
        x2_ax_1 = x2 `pfadd` ax_1
        xval = x2_m1_2 `pfdiv` (4 `pfmul` x `pfmul` x2_ax_1)

        twoax_1 = ax `pfadd` ax_1
        threex = 3 `pfmul` x
        threex_a = threex `pfadd` a
        threex2 = 3 `pfmul` x2
        threex2_2ax_1 = threex2 `pfadd` twoax_1
        threex2_2ax_1_3 = threex2_2ax_1 `pfmul` threex2_2ax_1 `pfmul`
                          threex2_2ax_1
        twoby = 2 `pfmul` b `pfmul` y
        twoby3 = twoby `pfmul` twoby `pfmul` twoby

        yval = ((threex_a `pfmul` threex2_2ax_1) `pfdiv` twoby) `pfsub`
               (b `pfmul` threex2_2ax_1_3 `pfdiv` twoby3) `pfsub` y
      in
        BasicPoint { basicX = xval, basicY = yval }
    | otherwise = error ("Invalid curve point " ++ show p)

  mul = montgomeryLadder

  valid _ Infinity = True
  valid PrimeFieldEC { curve = Montgomery { montgomeryA = a,
                                            montgomeryB = b },
                       modulus = m } p =
    let
      pfadd = primeFieldAdd m
      pfmul = primeFieldMul m

      x = coordX p
      y = coordY p
      rhs = (x `pfmul` x `pfmul` x) `pfadd` (a `pfmul` x`pfmul` x) `pfadd` x
      lhs = b `pfmul` y `pfmul` y
    in
      lhs == rhs


instance ECInfo (PrimeFieldEC (Montgomery Integer)) where
  plotCurve PrimeFieldEC { curve = Montgomery { montgomeryA = a,
                                                montgomeryB = b } } =
    let
      outstr = "f(x,y) = x**3 + " ++ show a ++ " * x**2 + x - " ++
               show b ++ " * y**2\n" ++
               "set contour\n" ++
               "set cntrparam levels discrete 0\n" ++
               "set view 0,0\n" ++
               "set isosamples 500,500\n" ++
               "unset surface\n" ++
               "unset ztics\n" ++
               "splot f(x,y)\n"
    in
      (flip writeFile) outstr

instance DiscreteECInfo (PrimeFieldEC (Montgomery Integer)) where
  discriminant _ = error "Not implemented"
  j_invariant _ = error "Not implemented"
  plotPoints _ = error "Not implemented"

instance DiscreteEC BasicPoint (PrimeFieldEC (Montgomery Integer)) where
  enumerate = enumeratePoints

instance Show coeffty => Show (Montgomery coeffty) where
  show Montgomery { montgomeryA = a, montgomeryB = b } =
    show b ++ " * y^2 = x^3 + (" ++ show a ++ " * x) + x"
