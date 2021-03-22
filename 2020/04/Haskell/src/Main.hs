import           Data.List
import           System.Environment
import           Text.Printf

passports :: [String] -> [[String]]
passports [] = []
passports ys = fields : passports remaining
 where
  parse []       = []
  parse (r : rs) = r ++ " " ++ parse rs
  fields    = words . parse . takeWhile (not . null) $ ys
  remaining = drop 1 . dropWhile (not . null) $ ys

isNotCID :: String -> Bool
isNotCID = (/= "cid") . take 3

validNum :: String -> (Int, Int) -> Bool
validNum s (a, b) = n >= a && n <= b where n = read s

validBYR :: String -> Bool
validBYR s = validNum s (1920, 2002)

validECL :: String -> Bool
validECL s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validEYR :: String -> Bool
validEYR s = validNum s (2020, 2030)

validHCL :: String -> Bool
validHCL []       = False
validHCL (x : xs) = a && b
 where
  a = x == '#'
  b = c == d
  c = replicate 6 True
  d = map (`elem` (['a' .. 'f'] ++ ['0' .. '9'])) xs

validHGT :: String -> Bool
validHGT s = valid
 where
  unit = take 2 . reverse $ s
  num  = reverse . drop 2 . reverse $ s
  valid | unit == "mc" = validNum num (150, 193)
        | unit == "ni" = validNum num (59, 76)
        | otherwise    = False

validIYR :: String -> Bool
validIYR s = validNum s (2010, 2020)

validPID :: String -> Bool
validPID = (== 9) . length

valid1 :: [[String]] -> [[String]]
valid1 [] = []
valid1 (x : xs) | null validPasswords = valid1 xs
                | otherwise           = validPasswords : valid1 xs
 where
  fieldsPresent = sort . filter (/= "cid") . map (takeWhile (/= ':')) $ x
  fieldsNeeded  = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]
  validPasswords | fieldsPresent == fieldsNeeded = x
                 | otherwise                     = []

valid2 :: [[String]] -> [[String]]
valid2 [] = []
valid2 (x : xs) | null validPasswords = valid2 xs
                | otherwise           = validPasswords : valid2 xs
 where
  fields = map (drop 1 . dropWhile (/= ':')) . filter isNotCID . sort $ x
  validPasswords
    | validBYR (head fields)
      && validECL (fields !! 1)
      && validEYR (fields !! 2)
      && validHCL (fields !! 3)
      && validHGT (fields !! 4)
      && validIYR (fields !! 5)
      && validPID (last fields)
    = x
    | otherwise
    = []

part1 :: [[String]] -> Int
part1 = length . valid1

part2 :: [[String]] -> Int
part2 = length . valid2 . valid1

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1:  %d\n" . part1 . passports $ input
  printf "  Part 2:  %d\n" . part2 . passports $ input

main :: IO ()
main = getArgs >>= mapM_ output
