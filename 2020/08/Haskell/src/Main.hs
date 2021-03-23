import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           System.Environment
import           Text.Printf

getOperation :: String -> String
getOperation = head . words

getArgument :: String -> Int
getArgument = read . dropWhile (== '+') . last . words

stepMachine :: [Int] -> Int -> Int -> Vector String -> (Int, Bool)
stepMachine y i a x | i == length x = (a, True)
                    | i `elem` y    = (a, False)
                    | otherwise     = stepMachine y' i' a' x
 where
  instruction = x V.! i
  operation   = getOperation instruction
  argument    = getArgument instruction
  y'          = i : y
  i' | operation == "jmp" = i + argument
     | otherwise          = i + 1
  a' | operation == "acc" = a + argument
     | otherwise          = a

getFixIndices :: Vector String -> [Int]
getFixIndices =
  V.toList
    . V.map fst
    . V.filter ((`elem` ["jmp", "nop"]) . getOperation . snd)
    . V.indexed

swapOperation :: Vector String -> Int -> Vector String
swapOperation v i = v'
 where
  this      = v V.! i
  operation = getOperation this
  argument  = last . words $ this
  next | operation == "jmp" = "nop " ++ argument
       | otherwise          = "jmp " ++ argument
  v' = v V.// [(i, next)]

findFixed :: [Vector String] -> Int
findFixed [] = 0
findFixed (x : xs) | fixed     = acc
                   | otherwise = findFixed xs
  where (acc, fixed) = stepMachine [] 0 0 x

part1 :: [String] -> Int
part1 = fst . stepMachine [] 0 0 . V.fromList

part2 :: [String] -> Int
part2 x = findFixed . map (swapOperation vector) . getFixIndices $ vector
  where vector = V.fromList x

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
