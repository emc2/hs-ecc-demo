{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.EC.Discrete.BasicPoint(
       module Math.EC.Point.Class,

       BasicPoint(..),
       ) where

import Math.EC.Point.Class

-- | Uncompressed representation of elliptic curve points.
data BasicPoint =
    BasicPoint {
      -- | The X coordinate
      basicX :: !Integer,
      -- | The Y coordinate
      basicY :: !Integer
    }
  | Infinity
    deriving Eq

instance PointInfinity BasicPoint where
  infinity = Infinity

instance Point Integer BasicPoint where
  point x y = BasicPoint { basicX = x, basicY = y }
  coordX = basicX
  coordY = basicY

instance Show BasicPoint where
  show p
     | p == infinity = "infinity"
     | otherwise = "(" ++ show (coordX p :: Integer) ++ "," ++
                   show (coordY p :: Integer) ++ ")"
