import           Data.List
import           System.Environment
import           Text.Printf

sumTwo :: [Int] -> Int -> Bool
sumTwo [] _ = False
sumTwo (x : xs) n | (n - x) `elem` xs = True
                  | otherwise         = sumTwo xs n

findErroneous :: Int -> [Int] -> Int
findErroneous _ [] = 0
findErroneous n x | sumTwo preamble input = findErroneous n (drop 1 x)
                  | otherwise             = input
 where
  preamble = sort . take n $ x
  input    = x !! n

contiguousSum :: Int -> [(Int, Int)] -> Int
contiguousSum _ [] = -1
contiguousSum n (x : xs) | remainder == 0 = fst x
                         | remainder < 0  = -1
                         | otherwise      = contiguousSum remainder xs
  where remainder = n - snd x

contiguousSums :: Int -> [(Int, Int)] -> [[Int]]
contiguousSums _ [] = []
contiguousSums n ys@(x : xs) | index == -1 = contiguousSums n xs
                             | otherwise = [fst x, index] : contiguousSums n xs
  where index = contiguousSum n ys

part1 :: Int -> [String] -> Int
part1 n = findErroneous n . map read

part2 :: Int -> [String] -> Int
part2 n s = minimum elements + maximum elements
 where
  numbers   = map read s
  indexed   = zip [0 ..] numbers
  erroneous = findErroneous n numbers
  [low, high] =
    maximumBy (\[a, b] [c, d] -> compare (b - a) (d - c))
      . contiguousSums erroneous
      $ indexed
  elements = map snd . filter ((`elem` [low .. high]) . fst) $ indexed

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 25 $ input
  printf "  Part 2: %d\n" . part2 25 $ input

main :: IO ()
main = getArgs >>= mapM_ output
