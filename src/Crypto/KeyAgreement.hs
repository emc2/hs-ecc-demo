{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Key agreement protocol interface.  In key agreement, we have two
-- parties that each have a key pair as well as eachothers' public
-- key.  These parties exchange a series of messages, culminating in
-- an agreement, wherein both parties have derived the same secret
-- value.
module Crypto.KeyAgreement(
       KeyAgreement(..)
       ) where

import Crypto.KeyPair

-- | Class for key agreement protocols.  This is stated in terms of
-- the types of public and private keys, as well as the type of the
-- protocol.
--
-- The protocol type must define the kinds of messages we can see.  In
-- a multi-stage protocol, this will be a series of messages exchanged
-- between the two parties.  At the very least, this should include a
-- "complete" message, which provides the shared secret.x
class KeyAgreement paramty pubty privty prototy where
  keyAgreement :: paramty
               -- ^ Parameters for the key agreement protocol.
               -> KeyPair pubty privty
               -- ^ Out key pair.
               -> pubty
               -- ^ Their public key.
               -> prototy
               -- ^ The protocol message.
               -> prototy
               -- ^ The next protocol message.
