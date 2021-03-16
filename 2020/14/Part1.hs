module Part1 where

import           Data.Bits
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM

type Memory = HashMap Int Int

applyMask :: [(Int, Char)] -> Int -> Int
applyMask []            value = value
applyMask ((a, v) : ms) value = applyMask ms value'
 where
  value' | v == '0'  = value `clearBit` a
         | otherwise = value `setBit` a

stepMemory :: String -> Memory -> [String] -> Memory
stepMemory _    memory []        = memory
stepMemory mask memory ([] : xs) = stepMemory mask memory xs
stepMemory mask memory (x : xs)
  | identifier == "mask" = stepMemory value memory xs
  | otherwise            = stepMemory mask memory' xs
 where
  [identifier, value] = filter (/= "=") . words $ x
  address             = read . reverse . drop 1 . reverse . drop 4 $ identifier
  mask' = filter ((`elem` "01") . snd) . zip [0 ..] . reverse $ mask
  value'              = applyMask mask' (read value)
  memory'             = HM.insert address value' memory

part1 :: [String] -> Int
part1 = sum . HM.elems . stepMemory [] HM.empty
