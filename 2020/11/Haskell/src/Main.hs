import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           System.Environment
import           Text.Printf

type Grid = Vector (Vector Char)
type Point = (Int, Int)
type Bounds = (Int, Int)

gridInit :: [String] -> Grid
gridInit = V.fromList . map V.fromList

getSeat :: Point -> Grid -> Char
getSeat (r, c) = (V.! c) . (V.! r)

getBounds :: Grid -> Bounds
getBounds grid =
  (\[x, y] -> (x, y)) . map ((-1) +) $ [V.length grid, (V.length . V.head) grid]

updatePoint :: Point -> Char -> Grid -> Grid
updatePoint (r, c) x g = g'
 where
  row  = g V.! r
  row' = row V.// [(c, x)]
  g'   = g V.// [(r, row')]

getAdjacent :: Grid -> Point -> [Point]
getAdjacent g p@(px, py) = adjacent
 where
  (x, y) = getBounds g
  adjacent =
    [ (r, c)
    | r <- [px - 1 .. px + 1]
    , r >= 0
    , r <= x
    , c <- [py - 1 .. py + 1]
    , c >= 0
    , c <= y
    , (r, c) /= p
    ]

getInLineOfSight :: Grid -> Point -> [Point]
getInLineOfSight g (px, py) = lineOfSight
 where
  (x, y)    = getBounds g
  firstSeat = take 1 . dropWhile ((== '.') . (`getSeat` g))
  north     = firstSeat [ (r, py) | r <- [px - 1, px - 2 .. 0] ]
  south     = firstSeat [ (r, py) | r <- [px + 1 .. x] ]
  east      = firstSeat [ (px, c) | c <- [py + 1 .. y] ]
  west      = firstSeat [ (px, c) | c <- [py - 1, py - 2 .. 0] ]
  northWest = firstSeat
    [ (r, c)
    | r <- [px - 1, px - 2 .. 0]
    , c <- [py - 1, py - 2 .. 0]
    , (px - r) == (py - c)
    ]
  southEast = firstSeat
    [ (r, c) | r <- [px + 1 .. x], c <- [py + 1 .. y], (r - px) == (c - py) ]
  northEast = firstSeat
    [ (r, c)
    | r <- [px - 1, px - 2 .. 0]
    , c <- [py + 1 .. y]
    , (px - r) == (c - py)
    ]
  southWest = firstSeat
    [ (r, c)
    | r <- [px + 1 .. x]
    , c <- [py - 1, py - 2 .. 0]
    , (r - px) == (py - c)
    ]
  lineOfSight = concat
    [north, south, east, west, northWest, southEast, northEast, southWest]

stepMap :: Grid -> Grid -> Int -> [Point] -> Grid
stepMap _ g _ [] = g
stepMap g0 g n (p : ps)
  | this == '.'                        = stepMap g0 g n ps
  | this == 'L' && occupied == 0       = stepMap g0 (insert '#') n ps
  | this == '#' && occupied >= (n + 3) = stepMap g0 (insert 'L') n ps
  | otherwise                          = stepMap g0 g n ps
 where
  this = getSeat p g0
  seatFunction | n == 1    = getAdjacent
               | n == 2    = getInLineOfSight
               | otherwise = error $ "Unexpected value " ++ show n
  occupied = length . filter ((== '#') . (`getSeat` g0)) . seatFunction g $ p
  insert x = updatePoint p x g

cycleMap :: Grid -> Int -> [Point] -> Grid
cycleMap g n p | g' == g   = g
                 | otherwise = cycleMap g' n p
  where g' = stepMap g g n p

solve :: Int -> [String] -> Int
solve n x = length . V.concatMap (V.filter (== '#')) $ grid'
 where
  grid      = gridInit x
  (r, c)    = getBounds grid
  positions = [ (a, b) | a <- [0 .. r], b <- [0 .. c] ]
  grid'     = cycleMap grid n positions

output :: String -> IO ()
output path = do
  input <- filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . solve 1 $ input
  printf "  Part 2: %d\n" . solve 2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
