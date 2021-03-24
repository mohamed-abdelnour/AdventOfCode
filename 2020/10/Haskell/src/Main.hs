import           Data.List
import           System.Environment
import           Text.Printf

initInput :: [String] -> [Int]
initInput x = 0 : maximum y + 3 : y where y = map read x

chain :: [Int] -> [Int]
chain []           = []
chain [_         ] = []
chain (x : y : zs) = y - x : chain (y : zs)

connections :: [Int] -> [Int]
connections []       = []
connections [_     ] = [0]
connections (x : xs) = c : connections xs
  where c = length . takeWhile (<= (x + 3)) $ xs

combinations :: [Int] -> [Integer]
combinations []       = []
combinations [0     ] = [1]
combinations (x : xs) = n : m
 where
  m = combinations xs
  n = sum . take x $ m

part1 :: [String] -> Int
part1 = product . map length . group . sort . chain . sort . initInput

part2 :: [String] -> Integer
part2 = head . combinations . connections . sort . initInput

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
