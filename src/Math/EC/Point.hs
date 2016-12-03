{-# OPTIONS_GHC -Wall -Werror #-}

module Math.EC.Point(
       Point(..),
       ) where

-- | Uncompressed representation of elliptic curve points.
data Point =
    Point {
      -- | The X coordinate
      pointX :: !Integer,
      -- | The Y coordinate
      pointY :: !Integer
    }
  | Infinity
    deriving Eq
