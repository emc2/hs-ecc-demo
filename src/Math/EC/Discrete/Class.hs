{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.EC.Discrete.Class(
       module Math.EC.Class,

       DiscreteECInfo(..),
       DiscreteEC(..)
       ) where

import Math.EC.Class

-- | Information about a discrete elliptic curve
class ECInfo curvety => DiscreteECInfo curvety where
  -- | Get the discriminant of the elliptic curve.
  discriminant :: curvety -> Integer

  -- | Get the j-invariant of the elliptic curve.
  j_invariant :: curvety -> Integer

  -- | Write out a gnuplot file with all the points plotted.
  plotPoints  :: curvety -> FilePath -> IO ()

-- | Class for discrete elliptic curves (i.e. elliptic curves mod p,
-- for some integer p)
class (EC pointty curvety, DiscreteECInfo curvety) =>
      DiscreteEC pointty curvety where
  -- | Enumerate all points in the curve.  This will take a very long
  -- time for curves with a high modulus!
  enumerate :: curvety -> [pointty]
