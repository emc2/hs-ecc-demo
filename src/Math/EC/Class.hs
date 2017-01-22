{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.EC.Class(
       ECInfo(..),
       EC(..)
       ) where

-- | Information about a discrete elliptic curve
class ECInfo curvety where
  -- | Write out a gnuplot file with the curve plotted.
  plotCurve :: curvety -> FilePath -> IO ()

-- | Basic elliptic curve.
class ECInfo curvety => EC pointty curvety where
  -- | The point addition operation.
  add :: curvety -> pointty -> pointty -> pointty

  -- | Add a point on a curve to itself.
  double :: curvety -> pointty -> pointty

  -- | The scalar multiply operation.
  mul :: curvety -> Integer -> pointty -> pointty

  -- | Check whether a given point is a valid point on the discrete
  -- curve.
  valid :: curvety -> pointty -> Bool
