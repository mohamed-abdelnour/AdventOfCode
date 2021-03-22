import           System.Environment
import           Text.Printf

findTrees :: [String] -> Int -> (Int, Int) -> Int
findTrees []           _ _        = 0
findTrees (   [] : xs) n (dx, dy) = findTrees xs n (dx, dy)
findTrees ys@(x  : _ ) n (dx, dy) = count
  + findTrees (drop dx ys) (n + 1) (dx, dy)
 where
  p = (n * dy) `mod` length x
  count | x !! p == '#' = 1
        | otherwise     = 0

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  printf "File: %s\n" path
  printf "  Part 1: %d\n" $ findTrees input 0 (1, 3)
  printf "  Part 2: %d\n" . product $ map (findTrees input 0) slopes

main :: IO ()
main = getArgs >>= mapM_ output
