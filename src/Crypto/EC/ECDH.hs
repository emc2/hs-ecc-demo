{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- | The Elliptic-Curve Diffie-Hellman key agreement protocol.  Allows
-- two parties to agree on a shared secret using their own key-pair
-- and the counterparty's public key.
module Crypto.EC.ECDH(
       module Crypto.KeyAgreement,

       ECDH(..)
       ) where

import Crypto.KeyAgreement
import Crypto.KeyPair
import Math.EC.Discrete.Class
import Math.EC.Point.Class

-- | Elliptic Curve Diffie-Hellman key agreement protocol.  This is
-- very simple, consisting of a single message.
data ECDH =
    -- | Protocol name.
    ECDH
    -- | Agreement message.
  | Agreement { agreement :: !Integer }
    deriving (Eq, Ord, Show)

instance (Point Integer pointty, DiscreteEC pointty curvety) =>
         KeyAgreement curvety pointty Integer ECDH where
  keyAgreement _ _ _ out @ Agreement {} = out
  keyAgreement curve KeyPair { privateKey = priv } otherpub ECDH =
    let
      -- Their public key is n_b * G, our public key is n_a * G.  Both
      -- parties multiply by their private keys, giving n_a * n_b * G
      -- on our side, n_b * n_a * G on their side, which are both
      -- equal by field laws.
      agreepoint = mul curve priv otherpub
    in
      -- The agreement is the X coordinate
      Agreement { agreement = coordX agreepoint }
