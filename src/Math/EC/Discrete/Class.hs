{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Math.EC.Discrete.Class(
       DiscreteEC(..)
       ) where

{-
data ECInfo pointty =
  ECInfo {
    modulus :: !Integer,
    basePoint :: pointty,
    order :: !Integer,
    cofactor :: !Integer

  }
-}
-- | Class for discrete elliptic curves (i.e. elliptic curves mod p,
-- for some integer p)
class Eq pointty => DiscreteEC pointty curvety | curvety -> pointty where
  -- | The modulus of the discrete elliptic curve.
  modulus :: curvety -> Integer

  -- | The point for infinity.
  infinity :: curvety -> pointty

  -- | The point addition operation.
  add :: curvety -> pointty -> pointty -> pointty

  -- | The scalar multiply operation.
  mult :: curvety -> Integer -> pointty -> pointty

  -- | Check whether a given point is a valid point on the discrete
  -- curve.
  valid :: curvety -> pointty -> Bool

  -- | Enumerate all points in the curve.  This will take a very long
  -- time for curves with a high modulus!
  enumerate :: curvety -> [pointty]

  -- | Write out a gnuplot file with the curve equation graphed.
  plotCurve :: curvety -> FilePath -> IO ()

  -- | Write out a gnuplot file with all the points graphed.
  plotPoints  :: curvety -> FilePath -> IO ()
