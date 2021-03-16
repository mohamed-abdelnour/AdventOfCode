module ModularArithmetic where

import           Data.Either
import qualified Data.HashMap.Strict           as HM
import           Data.HashMap.Strict            ( HashMap )

modularExp :: Integer -> Integer -> Integer -> Integer
modularExp b e m = modularExp' b e m 1

modularExp' :: Integer -> Integer -> Integer -> Integer -> Integer
modularExp' _ 0 _ r = r
modularExp' b e m r | even e    = modularExp' b' e' m r
                    | otherwise = modularExp' b' e' m r'
 where
  b' = (b ^ (2 :: Integer)) `mod` m
  e' = e `div` 2
  r' = (r * b) `mod` m

initValues
  :: Integer
  -> Integer
  -> Integer
  -> HashMap Integer Integer
  -> Integer
  -> Integer
  -> HashMap Integer Integer
initValues a m current values n i
  | i == n + 1 = values
  | otherwise  = initValues a m next values' n (i + 1)
 where
  next    = (current * a) `mod` m
  values' = HM.insert current i values

findDiscreteLog
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> HashMap Integer Integer
  -> Integer
  -> Integer
  -> Integer
findDiscreteLog an m offset current values n i
  | i == n + 1               = -1
  | HM.member current values = (n * i) - (values HM.! current) + offset
  | otherwise                = findDiscreteLog an m offset next values n (i + 1)
  where next = (current * an) `mod` m

fixNonCoprime
  :: Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> Either (Integer, Integer, Integer, Integer) Integer
fixNonCoprime a b m g k i | g <= 1         = Left (k, i, b, m)
                          | b == k         = Right i
                          | b `mod` g /= 0 = Right (-1)
                          | otherwise      = fixNonCoprime a b' m' g' k' (i + 1)
 where
  b' = b `div` g
  m' = m `div` g
  k' = (k * (a `div` g)) `mod` m
  g' = gcd a m'

discreteLog :: Integer -> Integer -> Integer -> Integer
discreteLog a b m | isRight x = fromRight (-1) x
                  | otherwise = findDiscreteLog an m' i current values n 1
 where
  g              = gcd a m
  x              = fixNonCoprime a b m g 1 0
  (k, i, b', m') = fromLeft (-1, -1, -1, -1) x
  n              = ceiling . sqrt . (fromIntegral :: Integer -> Double) $ m'
  an             = modularExp a n m'
  values         = initValues a m' b' HM.empty n 0
  current        = (k * an) `mod` m'
