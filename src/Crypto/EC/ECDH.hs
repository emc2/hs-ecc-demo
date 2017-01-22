{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Crypto.EC.ECDH(
       module Crypto.KeyAgreement,

       ECDH(..)
       ) where

import Crypto.KeyAgreement
import Crypto.KeyPair
import Math.EC.Discrete.Class
import Math.EC.Discrete.Point.Class

-- | Elliptic Curve Diffie-Hellman key agreement protocol.  This is
-- very simple, consisting of a single message.
data ECDH =
    -- | Protocol name.
    ECDH
    -- | Agreement message.
  | Agreement { agreement :: !Integer }
    deriving (Eq, Ord, Show)

instance (Point pubty, DiscreteECGroup pubty paramty) =>
         KeyAgreement paramty pubty Integer ECDH where
  keyAgreement _ _ _ out @ Agreement {} = out
  keyAgreement curve KeyPair { keyPairPriv = priv } otherpub ECDH =
    let
      -- Their public key is n_b * G, our public key is n_a * G.  Both
      -- parties multiply by their private keys, giving n_a * n_b * G
      -- on our side, n_b * n_a * G on their side, which are both
      -- equal by field laws.
      agreepoint = mul curve priv otherpub
    in
      -- The agreement is the X coordinate
      Agreement { agreement = coordX agreepoint }
