{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Crypto.EC.ECDSA(
       ECDSA
       ) where

import Crypto.Signature
import Math.EC.Discrete.Class
import Math.EC.Discrete.Point.Class

data ECDSA =
  ECDSA {
    ecdsaMsg :: !Integer,
    ecdsaR :: !Integer,
    ecdsaS :: !Integer
  }

instance (DiscreteECGroup pubty paramty) =>
         Verify paramty pubty Integer ECDSA where
  verify curve otherpub ECDSA { ecdsaMsg = msg, ecdsaR = r, ecdsaS = s } =
    _
