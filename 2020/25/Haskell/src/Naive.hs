module Naive where

modulus :: Int
modulus = 20201227

transformBy :: ((Int, Int) -> Bool) -> Int -> Int -> Int -> (Int, Int)
transformBy f i x x0 | x == (-1) = transformBy f 1 x0 x0
                     | f (x, i)  = (x, i)
                     | otherwise = transformBy f (i + 1) x' x0
  where x' = (x * x0) `mod` modulus

solve :: (Int, Int) -> Int
solve (k1, k2) = r1
 where
  l1 = snd . transformBy ((== k1) . fst) (-1) (-1) $ 7
  r1 = fst . transformBy ((== l1) . snd) (-1) (-1) $ k2
