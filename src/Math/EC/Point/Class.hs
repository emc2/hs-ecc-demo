{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Math.EC.Point.Class(
       PointInfinity(..),
       Point(..),
       ) where

class PointInfinity p where
  -- | The point for infinity.
  infinity :: p

-- | Class for discrete elliptic curves (i.e. elliptic curves mod p,
-- for some integer p)
class PointInfinity p => Point coordty p where
  -- | Create a point
  point :: coordty -> coordty -> p

  -- | Get the X coordinate.  Cannot be called on infinity.
  coordX :: p -> coordty

  -- | Get the Y coordinate.  Cannot be called on infinity.
  coordY :: p -> coordty
