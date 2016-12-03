{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Math.EC.Point.Class(
       Point(..),
       defaultShow
       ) where

-- | Class for discrete elliptic curves (i.e. elliptic curves mod p,
-- for some integer p)
class Point p where
  -- | The point for infinity.
  infinity :: p

  -- | Get the X coordinate.  Cannot be called on infinity.
  coordX :: p -> Integer

  -- | Get the Y coordinate.  Cannot be called on infinity.
  coordY :: p -> Integer

defaultShow :: (Eq p, Point p) => p -> String
defaultShow p
          | p == infinity = "infinity"
          | otherwise = "(" ++ show (coordX p) ++ "," ++ show (coordY p) ++ ")"
