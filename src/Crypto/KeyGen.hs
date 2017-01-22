{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Crypto.KeyGen(
       KeyGen(..)
       ) where

import Math.EC.Class
import Math.EC.Point.Class
import Math.EC.Discrete.Group
import Crypto.KeyPair

class KeyGen paramty keyty where
  keygen :: paramty -> Integer -> keyty

instance (Point Integer pointty, EC pointty curvety) =>
         KeyGen (DiscreteECGroup pointty curvety)
                (KeyPair pointty Integer) where
  keygen ecg n = KeyPair { publicKey = mul (discreteCurve ecg) n (base ecg),
                                 privateKey = n }
