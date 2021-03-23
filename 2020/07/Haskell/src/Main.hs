import           Data.List
import           System.Environment
import           Text.Printf

isNotCruft :: String -> Bool
isNotCruft s | take 3 s == "bag" = False
             | s == "contain"    = False
             | s == "no"         = False
             | s == "other"      = False
             | otherwise         = True

isNotNum :: String -> Bool
isNotNum = not . all (`elem` ['0' .. '9'])

fields :: [String] -> [String]
fields [] = []
fields ys@(x : xs) | isNotNum x = a : fields b
                   | otherwise  = x : fields xs
 where
  a = unwords . takeWhile isNotNum $ ys
  b = dropWhile isNotNum ys

parseInput :: [String] -> [[String]]
parseInput []        = []
parseInput ([] : xs) = parseInput xs
parseInput (x  : xs) = line : parseInput xs
  where line = fields . filter isNotCruft . words $ x

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

findLine :: [[String]] -> String -> [String]
findLine [] _ = []
findLine (x : xs) s | head x == s = x ++ findLine xs s
                    | otherwise   = findLine xs s

getParents :: [[String]] -> String -> [String]
getParents [] _ = []
getParents (x : xs) s | null parents = getParents xs s
                      | otherwise    = last parents : getParents xs s
  where parents = drop 1 . dropWhile (/= s) . reverse $ x

mapParents :: [String] -> [[String]] -> [String]
mapParents [] _ = []
mapParents s  x = level ++ mapParents level x
  where level = concatMap (getParents x) s

countChildren :: [[String]] -> [String] -> Int
countChildren _ [_] = 0
countChildren s x   = sum $ count ++ quantities
 where
  quantities = map read . filter (not . isNotNum) $ x
  children   = filter isNotNum . drop 1 $ x
  next       = map (countChildren s . findLine s) children
  count      = zipWith (*) quantities next

part1 :: [String] -> Int
part1 = length . removeDuplicates . mapParents ["shiny gold"] . parseInput

part2 :: [String] -> Int
part2 s = countChildren x line
 where
  x    = parseInput s
  line = findLine x "shiny gold"

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
