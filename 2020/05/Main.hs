import           Data.List
import           System.Environment
import           Text.Printf

halve :: [a] -> Char -> [a]
halve x c | c `elem` "FL" = take l x
          | otherwise     = drop l x
  where l = length x `div` 2

decode :: [Int] -> String -> Int
decode [x] _        = x
decode x   (c : cs) = decode (halve x c) cs
decode _   []       = -1

part1 :: String -> Int
part1 s = 8 * row + column
 where
  (sr, sc) = splitAt 7 s
  row      = decode [0 .. 127] sr
  column   = decode [0 .. 7] sc

firstDifferent :: [Int] -> [Int] -> Int
firstDifferent [] _  = -1
firstDifferent _  [] = -1
firstDifferent (x : xs) (y : ys) | x == y    = firstDifferent xs ys
                                 | otherwise = x

part2 :: [String] -> Int
part2 s = firstDifferent range input
 where
  input = sort . map part1 $ s
  range = [head input ..]

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1:  %d\n" . maximum . map part1 $ input
  printf "  Part 2:  %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
