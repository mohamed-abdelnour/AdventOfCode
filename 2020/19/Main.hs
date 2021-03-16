import           Data.ByteString.UTF8           ( ByteString )
import qualified Data.ByteString.UTF8          as B
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           System.Environment
import           Text.Printf
import           Text.Regex.PCRE

type Rule = (Int, String)
type RuleSet = HashMap Int String

parseInput :: [String] -> ([String], [String])
parseInput x = (rules, strings)
 where
  rules   = takeWhile (not . null) x
  strings = drop 1 . dropWhile (not . null) $ x

initRule :: String -> Rule
initRule x = (k, v)
 where
  k = read . takeWhile (/= ':') $ x
  v = filter (/= '"') . drop 2 . dropWhile (/= ':') $ x

initRuleSet :: RuleSet -> [String] -> RuleSet
initRuleSet s []       = s
initRuleSet s (r : rs) = initRuleSet s' rs
 where
  (k, v) = initRule r
  s'     = HM.insert k v s

isIndependent :: String -> Bool
isIndependent = all (`elem` string)
  where string = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "(|){}+ "

parseRule :: RuleSet -> [String] -> String
parseRule _ [] = []
parseRule s (x : xs) | isIndependent x = x ++ parseRule s xs
                     | isIndependent v = v ++ parseRule s xs
                     | otherwise       = "( " ++ v ++ " )" ++ parseRule s xs
  where v = HM.findWithDefault "" (read x) s

resolveRule :: RuleSet -> String -> String
resolveRule s = (!! 1) . dropWhile (not . isIndependent) . iterate f
  where f = parseRule s . words

generateRegex :: RuleSet -> String
generateRegex ruleSet = exactRegex
 where
  rule       = HM.findWithDefault "" 0 ruleSet
  regex      = resolveRule ruleSet rule
  exactRegex = ('^' :) . (++ "$") $ regex

matchRegex :: ByteString -> [ByteString] -> Int
matchRegex r = length . filter (=~ r)

setQuantifier :: Int -> String -> String
setQuantifier n = concatMap (\x -> if x == '}' then show n ++ "}" else [x])

controlRegex :: Int -> ByteString -> Int
controlRegex n r | control =~ r = n
                 | otherwise    = controlRegex (n + 1) r
  where control = B.fromString . replicate n $ 'x'

minRegexLength :: String -> Int
minRegexLength s = controlRegex 1 wildcard
 where
  wildcard = B.fromString . map (\x -> if x `elem` "ab" then '.' else x) $ s

getBaseRegex :: String -> [String]
getBaseRegex s = [r1, r2]
 where
  r1 = (++ ")$") . takeWhile (/= '+') $ s
  r2 = setQuantifier 1 . ('^' :) . drop 2 . dropWhile (/= '+') $ s

part1 :: [String] -> Int
part1 x = matchRegex regex strings'
 where
  (rules, strings) = parseInput x
  ruleSet          = initRuleSet HM.empty rules
  regex            = B.fromString . generateRegex $ ruleSet
  strings'         = map B.fromString strings

part2 :: [String] -> Int
part2 x = sum matches
 where
  (rules, strings) = parseInput x
  longest =
    length . maximumBy (\a b -> compare (length a) (length b)) $ strings
  ruleSet =
    HM.insert 11 "42 {} 31 {}"
      . HM.insert 8 "42 +"
      . initRuleSet HM.empty
      $ rules
  regex    = generateRegex ruleSet
  [r1, r2] = map minRegexLength . getBaseRegex $ regex
  regex' =
    map (B.fromString . (`setQuantifier` regex))
      . takeWhile (\n -> r1 + r2 * n <= longest)
      $ [1 ..]
  strings' = map B.fromString strings
  matches  = map (`matchRegex` strings') regex'

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
