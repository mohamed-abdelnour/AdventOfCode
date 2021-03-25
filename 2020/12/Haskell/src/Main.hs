import           System.Environment
import           Text.Printf

type Action = (Char, Int)
type Position = (Int, Int, Int)

parseInput :: [String] -> [Action]
parseInput = map (\x -> (head x, f x)) where f = read . drop 1

moveEntity :: Position -> Action -> Position
moveEntity (x, y, r) (k, v)
  | k == 'N'  = (x, y + v, r)
  | k == 'S'  = (x, y - v, r)
  | k == 'E'  = (x + v, y, r)
  | k == 'W'  = (x - v, y, r)
  | k == 'L'  = (x, y, r + v)
  | k == 'R'  = (x, y, r - v)
  | k == 'F'  = moveEntity (x, y, r) (k', v)
  | otherwise = error $ "Key should not have value " ++ [k]
 where
  r' = r `mod` 360
  k' | r' == 0   = 'E'
     | r' == 90  = 'N'
     | r' == 180 = 'W'
     | r' == 270 = 'S'
     | otherwise = error $ "Angle should not be " ++ show r

moveWaypoint :: Position -> Action -> Position
moveWaypoint i@(x, y, r) a@(k, v)
  | k == 'R'  = (m, n, r + v)
  | k == 'L'  = (p, q, r - v)
  | k == 'F'  = error "Key should not have value 'F'"
  | otherwise = moveEntity i a
 where
  v' = v `mod` 360
  (m, n) | v' == 0   = (x, y)
         | v' == 90  = (y, -x)
         | v' == 180 = (-x, -y)
         | v' == 270 = (-y, x)
         | otherwise = error $ "Angle should not be " ++ show v
  (p, q) | v' `elem` [0, 180]  = (m, n)
         | v' `elem` [90, 270] = (-m, -n)
         | otherwise           = error $ "Angle should not be " ++ show v

stepShip :: Position -> [Action] -> Position
stepShip p []       = p
stepShip p (a : as) = stepShip q as where q = moveEntity p a

shipToWaypoint :: [Position] -> Int -> [Position]
shipToWaypoint [(xs, ys, rs), w@(xw, yw, rw)] n = [next, w]
  where next = (xs + n * xw, ys + n * yw, rs + n * rw)
shipToWaypoint _ _ = []

stepWaypoint :: [Position] -> [Action] -> [Position]
stepWaypoint [s, w] []       = [s, w]
stepWaypoint [s, w] (a : as) = stepWaypoint next as
 where
  next | fst a /= 'F' = [s, moveWaypoint w a]
       | otherwise    = shipToWaypoint [s, w] . snd $ a
stepWaypoint _ _ = []

part1 :: [String] -> Int
part1 = (\(x, y, _) -> abs x + abs y) . stepShip (0, 0, 0) . parseInput

part2 :: [String] -> Int
part2 =
  (\[(x, y, _), _] -> abs x + abs y)
    . stepWaypoint [(0, 0, 0), (10, 1, 0)]
    . parseInput

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
