import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           System.Environment
import           Text.Printf

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p xs = x : splitOn p xs'
 where
  x   = takeWhile p xs
  xs' = drop 1 . dropWhile p $ xs

parseInput :: [String] -> ([Int], [Int])
parseInput =
  (\[x, y] -> (x, y)) . map (map read . drop 1) . splitOn (not . null)

combat :: ([Int], [Int]) -> [Int]
combat (w     , []    ) = w
combat ([]    , w     ) = w
combat (a : as, x : xs) = combat (as', xs')
 where
  (as', xs') | a > x     = (as ++ [a] ++ [x], xs)
             | x > a     = (as, xs ++ [x] ++ [a])
             | otherwise = error "Equal cards"

recursiveCombat
  :: HashMap ([Int], [Int]) Bool -> ([Int], [Int]) -> (Int, [Int])
recursiveCombat _ (w , []) = (1, w)
recursiveCombat _ ([], w ) = (2, w)
recursiveCombat r (b@(a : as), y@(x : xs))
  | HM.member (b, y) r = (1, b)
  | otherwise          = recursiveCombat r' (as', xs')
 where
  r' = HM.insert (b, y) True r
  w' | a > x     = 1
     | x > a     = 2
     | otherwise = error "Equal cards"
  w
    | length as >= a && length xs >= x
    = fst . recursiveCombat HM.empty $ (take a as, take x xs)
    | otherwise
    = w'
  (as', xs') | w == 1    = (as ++ [a] ++ [x], xs)
             | otherwise = (as, xs ++ [x] ++ [a])

solve :: (([Int], [Int]) -> [Int]) -> [String] -> Int
solve f = sum . zipWith (*) [1 ..] . reverse . f . parseInput

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . solve combat $ input
  printf "  Part 2: %d\n" . solve (snd . recursiveCombat HM.empty) $ input

main :: IO ()
main = getArgs >>= mapM_ output
