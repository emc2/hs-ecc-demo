{-# OPTIONS_GHC -Wall -Werror #-}

-- | Field operations on a prime-number finite field.
module Math.PrimeField(
       primeFieldAdd,
       primeFieldNeg,
       primeFieldSub,
       primeFieldMul,
       primeFieldInv,
       primeFieldDiv
       ) where

-- | Finite field addition.
primeFieldAdd :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldAdd m a b = (a + b) `mod` m

-- | Finite field negation.
primeFieldNeg :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Value to invert.
              -> Integer
primeFieldNeg m a = m - a

-- | Finite field subtraction.
primeFieldSub :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldSub m a = primeFieldAdd m a . primeFieldNeg m

-- | Finite field multiply.
primeFieldMul :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldMul m a b = (a * b) `mod` m

-- | Finite field inverse.
primeFieldInv :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Value to invert.
              -> Integer
-- This is the only non-trivial one.  Observe that @a ^ (m - 1) `mod`
-- m = a@ because @m@ is prime.  We can therefore easily find the
-- multiplicative inverse as @a ^ (-1) = a ^ (m - 2)@.
primeFieldInv m a = (a ^ (m - 2)) `mod` m

-- | Finite field division.
primeFieldDiv :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldDiv m a = primeFieldMul m a . primeFieldInv m
