import           System.Environment
import           Text.Printf

isOp :: String -> Bool
isOp "+" = True
isOp "*" = True
isOp _   = False

precedence :: Int -> String -> Int
precedence p "+" = p
precedence _ "*" = 1
precedence _ _   = 0

isLP :: String -> Bool
isLP "(" = True
isLP _   = False

isRP :: String -> Bool
isRP ")" = True
isRP _   = False

isDigit :: Char -> Bool
isDigit = (`elem` ['0' .. '9'])

isNumber :: String -> Bool
isNumber = all isDigit

parseInput :: String -> [String]
parseInput []         = []
parseInput (' ' : xs) = parseInput xs
parseInput y@(x : xs) | special x' = x' : parseInput xs
                      | otherwise  = n : parseInput y'
 where
  special t = isOp t || isLP t || isRP t
  x' = [x]
  n  = takeWhile isDigit y
  y' = dropWhile isDigit y

shuntingYardBy
  :: [String] -> [String] -> (String -> Int) -> [String] -> [String]
shuntingYardBy []       r _ [] = reverse r
shuntingYardBy (o : os) r p [] = shuntingYardBy os (o : r) p []
shuntingYardBy os r p ts'@(t : ts)
  | isNumber t = shuntingYardBy os (t : r) p ts
  | isOp t = case os of
    []        -> shuntingYardBy (t : os) r p ts
    (o : os') -> if p t <= p o
      then shuntingYardBy os' (o : r) p ts'
      else shuntingYardBy (t : os) r p ts
  | isLP t = shuntingYardBy (t : os) r p ts
  | isRP t = case os of
    ("(" : os') -> shuntingYardBy os' r p ts
    (o   : os') -> shuntingYardBy os' (o : r) p ts'
    _           -> error "Unbalanced parentheses"
shuntingYardBy _ _ _ _ = []

rpn :: [String] -> Int
rpn = head . foldl eval []
 where
  eval (x : y : zs) "+" = x + y : zs
  eval (x : y : zs) "*" = x * y : zs
  eval zs           d   = read d : zs

solve :: Int -> [String] -> [Int]
solve p = map evaluate
  where evaluate = rpn . shuntingYardBy [] [] (precedence p) . parseInput

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . sum . solve 1 $ input
  printf "  Part 2: %d\n" . sum . solve 2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
