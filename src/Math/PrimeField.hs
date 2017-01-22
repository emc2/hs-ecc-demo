module Math.PrimeField(
       primeFieldAdd,
       primeFieldNeg,
       primeFieldSub,
       primeFieldMul,
       primeFieldInv,
       primeFieldDiv
       ) where

primeFieldAdd :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldAdd m a b = (a + b) `mod` m

primeFieldNeg :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Value to invert.
              -> Integer
primeFieldNeg m a = m - a

primeFieldSub :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldSub m a = primeFieldAdd m a . primeFieldNeg m

primeFieldMul :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldMul m a b = (a * b) `mod` m

primeFieldInv :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Value to invert.
              -> Integer
primeFieldInv m a = (a ^ (m - 2)) `mod` m

primeFieldDiv :: Integer
              -- ^ Modulus of the prime field.
              -> Integer
              -- ^ Left-hand side
              -> Integer
              -- ^ Right-hand side
              -> Integer
primeFieldDiv m a = primeFieldMul m a . primeFieldInv m
