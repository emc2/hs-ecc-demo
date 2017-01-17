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

-- | A Weierstrass-form elliptic curve.  This is a curve of the
-- form
-- > y^2 = x^3 + Ax + B mod P
data Weierstrass =
  Weierstrass {
    -- | The value of A.
    weierstrassA :: !Rational,
    -- | The value of B.
    weierstrassB :: !Rational,
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
weierstrass a b p = Weierstrass { weierstrassA = a, weierstrassB = b,
                                  weierstrassModulus = p }

instance DiscreteEC Weierstrass where
  modulus Weierstrass { weierstrassModulus = p } = p

  plotPoints _ = error "Not implemented"

instance DiscreteECGroup BasicPoint Weierstrass where
  valid Weierstrass { weierstrassA = a, weierstrassB = b,
                      weierstrassModulus = p } point =
    let
      x = fromInteger (coordX point)
      y = fromInteger (coordY point)
      rhsfrac = (x * x * x) + (a * x) + b
      lhs = (y * y) `mod` p
    in
      denominator rhsfrac == 1 &&
      lhs == numerator rhsfrac `mod` p

instance Show Weierstrass where
  show Weierstrass { weierstrassA = a, weierstrassB = b,
                     weierstrassModulus = p } =
    let
      astr | denominator a == 1 = show (numerator a)
           | otherwise = "(" ++ show (numerator a) ++ "/" ++
                          show (denominator a) ++ ")"
      bstr | denominator b == 1 = show (numerator b)
           | otherwise = "(" ++ show (numerator b) ++ "/" ++
                          show (denominator b) ++ ")"
    in
      "y^2 = x^3 + (" ++ astr ++ " * x) + " ++ bstr ++ " mod " ++ show p
