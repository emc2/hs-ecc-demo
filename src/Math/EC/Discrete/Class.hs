{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.EC.Discrete.Class(
       DiscreteEC(..),
       DiscreteECGroup(..)
       ) where

import Math.EC.Discrete.Point.Class

-- | Class for discrete elliptic curves (i.e. elliptic curves mod p,
-- for some integer p)
class DiscreteEC curvety where
  -- | The modulus of the discrete elliptic curve.
  modulus :: curvety -> Integer

  -- | Write out a gnuplot file with all the points graphed.
  plotPoints  :: curvety -> FilePath -> IO ()

-- | Class for group operations on discrete elliptic curves.
class (Point pointty, DiscreteEC curvety) =>
      DiscreteECGroup pointty curvety where
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
