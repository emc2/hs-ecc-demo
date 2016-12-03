{-# OPTIONS_GHC -Wall -Werror #-}

module Math.EC.BasicPoint(
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

instance Point BasicPoint where
  infinity = Infinity
  coordX = basicX
  coordY = basicY

instance Show BasicPoint where
  show = defaultShow
