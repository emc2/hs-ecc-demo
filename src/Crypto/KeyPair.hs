{-# OPTIONS_GHC -Wall -Werror #-}

module Crypto.KeyPair(
       KeyPair(..)
       ) where

-- | Abstract key pair type.
data KeyPair pubty privty =
  KeyPair {
    keyPairPriv :: !privty,
    keyPairPub :: !pubty
  }
  deriving (Eq, Ord)

instance (Show pubty, Show privty) => Show (KeyPair pubty privty) where
  show KeyPair { keyPairPub = pub, keyPairPriv = priv } =
    "public key = " ++ show pub ++ ", private key = " ++ show priv
