{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.EC.Discrete.Group(
       DiscreteECGroup(..),

       generate
       ) where

import Math.EC.Class
import Math.EC.Point.Class

data DiscreteECGroup pointty curvety =
  DiscreteECGroup {
    -- | The underlying discrete curve.
    discreteCurve :: !curvety,
    -- | The generator point.
    base :: !pointty
  }

-- | Generate the entire addition group from a starting point.  This
-- will take a very long time for curves with a high modulus!
generate :: (Point Integer pointty, EC pointty curvety, Eq pointty) =>
            DiscreteECGroup pointty curvety -> [pointty]
generate DiscreteECGroup { discreteCurve = ec, base = g } =
  let
    genfunc accum p =
      let
        newpoint = add ec p g
      in
        if newpoint == infinity
          then reverse accum
          else genfunc (newpoint : accum) newpoint
  in
    genfunc [g] g
