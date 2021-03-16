import           Data.List
import           System.Environment
import           Text.Printf

getRange :: String -> String
getRange [] = []
getRange (x : xs) | x /= '-'  = x : getRange xs
                  | otherwise = ' ' : getRange xs

valid1 :: [String] -> Bool
valid1 x = inRange
 where
  char           = (!! 0) $ takeWhile (/= ':') (x !! 1)
  count          = length . filter (== char) $ sort (x !! 2)
  [lower, upper] = map read . words . getRange $ head x
  inRange        = count >= lower && count <= upper

valid2 :: [String] -> Bool
valid2 x = passes
 where
  char     = (!! 0) $ takeWhile (/= ':') (x !! 1)
  string   = last x
  [p1, p2] = map (((-1) +) . read) . words . getRange $ head x
  passes   = (char == string !! p1) /= (char == string !! p2)

solveBy :: ([String] -> Bool) -> [[String]] -> Int
solveBy f = length . filter f

output :: String -> IO ()
output path = do
  input <- map words . filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . solveBy valid1 $ input
  printf "  Part 2: %d\n" . solveBy valid2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
