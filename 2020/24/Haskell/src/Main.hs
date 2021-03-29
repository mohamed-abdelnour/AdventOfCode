import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           System.Environment
import           Text.Printf

type Point = (Int, Int)
type Tile = Bool -- (False -> White, True -> Black)
type TileGrid = HashMap Point Tile

parseLine :: String -> [String]
parseLine []          = []
parseLine ('e'   : x) = "e" : parseLine x
parseLine ('w'   : x) = "w" : parseLine x
parseLine (a : b : x) = [a, b] : parseLine x
parseLine _           = []

stepGrid :: Point -> String -> Point
stepGrid (x, y) "e"  = (x + 2, y)
stepGrid (x, y) "ne" = (x + 1, y + 1)
stepGrid (x, y) "se" = (x + 1, y - 1)
stepGrid (x, y) "w"  = (x - 2, y)
stepGrid (x, y) "nw" = (x - 1, y + 1)
stepGrid (x, y) "sw" = (x - 1, y - 1)
stepGrid _      _    = (0, 0)

findTile :: Point -> [String] -> Point
findTile p []       = p
findTile p (x : xs) = findTile p' xs where p' = stepGrid p x

stepInput :: TileGrid -> [String] -> TileGrid
stepInput t []        = t
stepInput t ([] : xs) = stepInput t xs
stepInput t (x  : xs) = stepInput t' xs
 where
  parsed = parseLine x
  tile   = findTile (0, 0) parsed
  value  = HM.findWithDefault False tile t
  t'     = HM.insert tile (not value) t

getAdjacent :: Point -> [Point]
getAdjacent (x, y) = diagonals ++ line
 where
  diagonals =
    [ (a, b) | a <- [x - 1 .. x + 1], a /= x, b <- [y - 1 .. y + 1], b /= y ]
  line = [(x - 2, y), (x + 2, y)]

nextTile :: Point -> TileGrid -> Tile
nextTile p t = tile
 where
  value    = HM.findWithDefault False p t
  adjacent = getAdjacent p
  count    = length . filter (\x -> HM.findWithDefault False x t) $ adjacent
  tile | not value && count == 2            = True
       | value && (count == 0 || count > 2) = False
       | otherwise                          = value

updateTiles :: TileGrid -> [Point] -> TileGrid -> TileGrid
updateTiles t [] t0 | HM.null t = updateTiles t (HM.keys t0) t0
                    | otherwise = t
updateTiles t (x : xs) t0 = updateTiles t' xs t0
 where
  tile = nextTile x t0
  t'   = HM.insert x tile t

cycleTiles :: Int -> Int -> TileGrid -> TileGrid
cycleTiles i0 i t | i == i0   = t
                  | otherwise = cycleTiles i0 (i + 1) next
 where
  keys = HM.keys . HM.filter (== True) $ t
  notIncluded =
    map head
      . group
      . sort
      . filter (not . (`HM.member` t))
      . concatMap getAdjacent
      $ keys
  n    = HM.fromList . zip notIncluded $ repeat False
  t'   = HM.union t n
  next = updateTiles HM.empty [] t'

part1 :: [String] -> Int
part1 = length . filter (== True) . HM.elems . stepInput HM.empty

part2 :: [String] -> Int
part2 =
  length . filter (== True) . HM.elems . cycleTiles 100 0 . stepInput HM.empty

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
