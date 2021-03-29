import qualified Part1                         as P1
import qualified Part2                         as P2
import           System.Environment
import           Text.Printf

output :: String -> IO ()
output path = do
  input <- head . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %s\n" . P1.part1 $ input
  (r1, r2, p) <- P2.solve input
  printf "  Part 2: %d * %d = %d\n" r1 r2 p

main :: IO ()
main = getArgs >>= mapM_ output
