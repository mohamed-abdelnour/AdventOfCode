import           Data.Bifunctor
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Environment
import           Text.Printf

type FieldList = [(String, [Int])]
type FieldMap = HashMap String [Int]
type Rule = (String, [(Int, Int)])
type RuleMap = HashMap String [(Int, Int)]
type Ticket = [Int]

separateInput :: String -> [[Text]]
separateInput =
  filter (not . null) . map T.lines . T.splitOn (T.pack "\n\n") . T.pack

parseRange :: Text -> (Int, Int)
parseRange =
  (\x -> (head x, last x)) . map (read . T.unpack) . T.splitOn (T.pack "-")

parseRule :: Text -> Rule
parseRule x = (i, r)
 where
  [identifier, ranges] = T.splitOn (T.pack ":") x
  i = T.unpack identifier
  r = map (parseRange . T.drop 1) . T.splitOn (T.pack "or") $ ranges

parseTicket :: Text -> Ticket
parseTicket = map (read . T.unpack) . T.splitOn (T.pack ",")

parseInput :: [[Text]] -> ([Rule], Ticket, [Ticket])
parseInput [rules, myT, nearbyTs] = (r, t, ts)
 where
  r  = map parseRule rules
  t  = parseTicket . (!! 1) $ myT
  ts = map parseTicket . drop 1 $ nearbyTs
parseInput _ = ([], [], [])

inRange :: Int -> (Int, Int) -> Bool
n `inRange` (a, b) = n >= a && n <= b

inAnyRange :: Int -> [(Int, Int)] -> Bool
n `inAnyRange` rs = foldr ((||) . (n `inRange`)) False rs

invalidEntries :: [(Int, Int)] -> Int -> Int
invalidEntries r n | n `inAnyRange` r = 0
                   | otherwise        = n

mapRules :: [Rule] -> RuleMap -> RuleMap
mapRules []       m = m
mapRules (r : rs) m = mapRules rs nextM
 where
  (identifier, ranges) = r
  nextM                = HM.insert identifier ranges m

validateEntries :: [Int] -> [(Int, Int)] -> Bool
validateEntries t rs = foldr ((&&) . (`inAnyRange` rs)) True t

validateTickets :: [Ticket] -> [(Int, Int)] -> [Ticket]
validateTickets [] _ = []
validateTickets (t : ts) rs | valid     = t : validateTickets ts rs
                            | otherwise = validateTickets ts rs
  where valid = validateEntries t rs

findField :: [String] -> [Int] -> RuleMap -> [String]
findField [] _ _ = []
findField (k : ks) x m | valid     = k : findField ks x m
                       | otherwise = findField ks x m
 where
  ranges = HM.findWithDefault [] k m
  valid  = validateEntries x ranges

insertKeys :: [String] -> Int -> FieldMap -> FieldMap
insertKeys []       _ f = f
insertKeys (x : xs) n f = insertKeys xs n nextMap
 where
  thisV = HM.findWithDefault [-1] x f
  nextV | thisV == [-1] = [n]
        | otherwise     = n : thisV
  nextMap = HM.insert x nextV f

stepFields :: [Ticket] -> Int -> FieldMap -> RuleMap -> FieldMap
stepFields t i f m | i == length (head t) = f
                   | otherwise            = stepFields t (i + 1) fieldMap m
 where
  columns  = map (!! i) t
  keys     = HM.keys m
  fields   = findField keys columns m
  fieldMap = insertKeys fields i f

fieldPreprocess :: FieldList -> FieldList
fieldPreprocess =
  map (second sort) . sortBy (\(_, x) (_, y) -> compare (length x) (length y))

processFields :: [Int] -> FieldList -> [(String, Int)]
processFields _ []       = []
processFields n (x : xs) = (i, index) : processFields nextN xs
 where
  (i, ns) = x
  nextN   = ns ++ n
  index   = head . filter (`notElem` n) $ ns

part1 :: String -> Int
part1 x = sum check
 where
  (r, _, ts) = parseInput . separateInput $ x
  ranges     = concatMap snd r
  tickets    = concat ts
  check      = map (invalidEntries ranges) tickets

assignFields :: [Rule] -> [Ticket] -> [(String, Int)]
assignFields r ts = processedFields
 where
  mapInit         = mapRules r HM.empty
  ranges          = concatMap snd r
  validTickets    = validateTickets ts ranges
  fields          = stepFields validTickets 0 HM.empty mapInit
  processedFields = processFields [] . fieldPreprocess . HM.toList $ fields

part2 :: String -> Int
part2 x = solve
 where
  (r, t, ts)      = parseInput . separateInput $ x
  processedFields = assignFields r ts
  indices =
    sort
      . map snd
      . filter ((== "departure") . head . words . fst)
      $ processedFields
  solve = product . map (t !!) $ indices

output :: String -> IO ()
output path = do
  input <- readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
