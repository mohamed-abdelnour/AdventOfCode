module Part2 where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM

type Memory = HashMap Int Int

parseMap :: String -> [Int]
parseMap = map fst . filter ((== '1') . snd) . zip [0 ..] . reverse

binToDec :: String -> Int
binToDec = sum . map (2 ^) . parseMap

decToBin :: Int -> [Int]
decToBin 0 = []
decToBin n = decToBin (n `div` 2) ++ [n `mod` 2]

xCount :: String -> Int
xCount = length . filter (== 'X')

maskAddress :: String -> String -> String
maskAddress []       m        = reverse m
maskAddress (a : as) (m : ms) = maskAddress as ms ++ [next]
 where
  next | m == '0'  = a
       | otherwise = m
maskAddress _ _ = []

genAddressHelper :: String -> String -> String
genAddressHelper []       _ = []
genAddressHelper (x : xs) s = nextX : genAddressHelper xs nextS
 where
  nextX | x == 'X'  = head s
        | otherwise = x
  nextS | x == 'X'  = drop 1 s
        | otherwise = s

genAddress :: String -> Int -> Int -> [String]
genAddress _ n0 n | 2 ^ n0 == n = []
genAddress m n0 n               = nextMask : genAddress m n0 (n + 1)
 where
  c               = concatMap show . decToBin $ n
  b               = replicate (n0 - length c) '0'
  nextCombination = b ++ c
  nextMask        = genAddressHelper m nextCombination

stepMemory :: Memory -> Int -> [Int] -> Memory
stepMemory m _ []       = m
stepMemory m v (a : as) = stepMemory nextM v as where nextM = HM.insert a v m

stepInput :: String -> Memory -> [String] -> Memory
stepInput _    memory []        = memory
stepInput mask memory ([] : xs) = stepInput mask memory xs
stepInput mask memory (x : xs)
  | identifier == "mask" = stepInput value memory xs
  | otherwise            = stepInput mask memory' xs
 where
  [identifier, value] = filter (/= "=") . words $ x
  address             = reverse . drop 1 . reverse . drop 4 $ identifier
  maskedAddress       = maskAddress address' mask'
  count               = xCount maskedAddress
  nextAs              = map binToDec . genAddress maskedAddress count $ 0
  memory'             = stepMemory memory (read value) nextAs
  mask'               = reverse mask
  address'            = reverse . concatMap show . decToBin . read $ address

part2 :: [String] -> Int
part2 = sum . HM.elems . stepInput [] HM.empty
