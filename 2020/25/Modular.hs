module Modular where

import           ModularArithmetic

modulus :: Integer
modulus = 20201227

solve :: (Integer, Integer) -> Integer
solve (k1, k2) | r1 == r2  = r1
               | otherwise = error "Encryption keys do not match up"
 where
  l1 = discreteLog 7 k1 modulus
  l2 = discreteLog 7 k2 modulus
  r1 = modularExp k1 l2 modulus
  r2 = modularExp k2 l1 modulus
