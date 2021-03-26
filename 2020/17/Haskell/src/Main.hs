import qualified Part1                         as P1
import qualified Part2                         as P2
import           System.Environment
import           Text.Printf

output :: String -> IO ()
output path = do
  input <- readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . P1.part1 $ input
  printf "  Part 2: %d\n" . P2.part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
