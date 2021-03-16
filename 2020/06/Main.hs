import           Data.List
import           System.Environment
import           Text.Printf

parseInput :: [String] -> [[String]]
parseInput [] = []
parseInput x  = groups : parseInput remaining
 where
  groups    = takeWhile (not . null) x
  remaining = drop 1 . dropWhile (not . null) $ x

intersections :: [String] -> [String]
intersections [x         ] = [x]
intersections (x : y : xs) = intersections $ x `intersect` y : xs
intersections []           = []

part1 :: [String] -> Int
part1 =
  sum . map (length . group . sort . filter (/= ' ') . unwords) . parseInput

part2 :: [String] -> Int
part2 =
  sum
    . map length
    . filter (not . null)
    . map (unwords . intersections)
    . parseInput

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
