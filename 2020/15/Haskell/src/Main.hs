import qualified Data.Text                     as T
import           Data.Vector.Unboxed.Mutable    ( IOVector )
import qualified Data.Vector.Unboxed.Mutable   as MV
import           System.Environment
import           Text.Printf

type Game = IOVector (Int, Int)

parseInput :: String -> [Int]
parseInput = map (read . T.unpack . T.strip) . T.splitOn (T.pack ",") . T.pack

initGame :: [Int] -> Game -> IO Game
initGame n v = mapM_ write base >> return v
 where
  base  = zip n [ (t, t) | t <- [1 ..] ]
  write = uncurry (MV.write v)

stepGame :: Int -> Int -> Int -> Game -> IO Int
stepGame i0 i n v
  | i == i0 = return n
  | otherwise = do
    (f, l) <- MV.read v n
    let n' = l - f
    (fn, ln) <- MV.read v n'
    let (fn', ln') | fn == 0   = (i, i)
                   | otherwise = (ln, i)
    MV.write v n' (fn', ln')
    stepGame i0 (i + 1) n' v

solve :: Int -> [Int] -> IO Int
solve i n = MV.replicate i (0, 0) >>= initGame n >>= stepGame (i + 1) offset n0
 where
  n0     = last n
  offset = length n + 1

output :: String -> IO ()
output path = do
  input <- map parseInput . filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  part1 <- mapM (solve 2020) input
  printf "  Part 1: %s\n" . extract $ part1
  part2 <- mapM (solve 30000000) input
  printf "  Part 2: %s\n" . extract $ part2
 where
  singleton [_] = True
  singleton _   = False
  extract x | singleton x = show . head $ x
            | otherwise   = show x

main :: IO ()
main = getArgs >>= mapM_ output
