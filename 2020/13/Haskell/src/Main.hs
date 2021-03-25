import           System.Environment
import           Text.Printf

idHelper :: String -> String -> String
idHelper _ [] = []
idHelper s (x : xs) | x `elem` s = ' ' : idHelper s xs
                    | otherwise  = x : idHelper s xs

parseInput :: String -> [String] -> (Int, [String])
parseInput s x = (time, idList)
 where
  [t, i] = x
  time   = read t
  idList = words . idHelper s $ i

initInput :: (Int, [String]) -> (Int, [Int])
initInput (t, x) = (t, i) where i = map read x

closestBus :: (Int, Int, [Int]) -> Int
closestBus (t0, t, i) | null busID = closestBus (t0, t + 1, i)
                      | otherwise  = (*) (t - t0) . snd . head $ busID
 where
  times = map (t `mod`) i
  busID = filter ((== 0) . fst) . zip times $ i

baseTimes :: Int -> [String] -> [(Int, Int)]
baseTimes _ [] = []
baseTimes n (x : xs) | x == "x"  = baseTimes (n + 1) xs
                     | otherwise = (n, i) : baseTimes (n + 1) xs
  where i = read x

findModHelper :: (Int, Int) -> Int -> Int
findModHelper (a, b) n | (a * n) `mod` b == 1 = n
                       | otherwise            = findModHelper (a, b) (n + 1)

findMod :: [[Int]] -> [Int]
findMod [[]    , []    ] = []
findMod [x : xs, y : ys] = findModHelper (x, y) 1 : findMod [xs, ys]
findMod _                = []

getRemainder :: [(Int, Int)] -> Int
getRemainder x = sums `mod` productTimes
 where
  remainders     = map fst x
  times          = map snd x
  diff           = zipWith (-) times remainders
  productTimes   = product times
  productDivSelf = map (productTimes `div`) times
  divs           = zipWith mod productDivSelf times
  mods           = findMod [divs, times]
  sums           = sum . zipWith (*) mods . zipWith (*) productDivSelf $ diff

part1 :: [String] -> Int
part1 x = closestBus (t, t, i) where (t, i) = initInput . parseInput "x," $ x

part2 :: [String] -> Int
part2 = getRemainder . baseTimes 0 . snd . parseInput ","

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
