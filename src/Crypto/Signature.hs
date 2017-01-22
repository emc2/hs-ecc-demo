{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Signature(
       Signature(..),
       Verify(..)
       ) where

import Crypto.KeyPair

class Verify paramty pubty msgty signedty =>
      Signature paramty pubty privty msgty signedty where
  -- | Sign a message.
  sign :: paramty
       -- ^ The signature algorithm parameters.
       -> KeyPair pubty privty
       -- ^ Our key pair.
       -> msgty
       -- ^ The message to sign.
       -> signedty
       -- ^ A signed message.

class Verify paramty pubty msgty signedty where
  -- | Verify (and extract) a signed message.
  verify :: paramty
         -- ^ The signature algorithm parameters.
         -> pubty
         -- ^ Their public key.
         -> signedty
         -- ^ A signed message.
         -> Maybe msgty
         -- ^ The underlying message, if the signature checks out.
