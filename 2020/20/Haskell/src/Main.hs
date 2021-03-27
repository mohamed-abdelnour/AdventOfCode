import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           System.Environment
import           Text.Printf

type Tile = [String]
type TileID = Int
type TileSet = HashMap TileID Tile
type AltTileSet = HashMap TileID [Tile]

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p xs = x : splitOn p xs'
 where
  x   = takeWhile p xs
  xs' = drop 1 . dropWhile p $ xs

initTiles :: TileSet -> [Tile] -> TileSet
initTiles t []       = t
initTiles t (x : xs) = initTiles t' xs
 where
  (k : v) = x
  k'      = read . init . last . words $ k
  t'      = HM.insert k' v t

initInput :: [String] -> TileSet
initInput = initTiles HM.empty . filter (not . null) . splitOn (not . null)

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

applyTransforms :: [[a]] -> [[[a]]]
applyTransforms x = rotated ++ flipped
 where
  rotated = take 4 . iterate rotate $ x
  flipped = map reverse rotated

initAltTiles :: TileSet -> AltTileSet
initAltTiles = HM.map applyTransforms

directionalMatch :: (Eq a) => Char -> [[a]] -> [[a]] -> Bool
directionalMatch 'T' t t' = last t == head t'
directionalMatch 'B' t t' = directionalMatch 'T' t' t
directionalMatch 'R' t t' = directionalMatch 'T' (transpose t) (transpose t')
directionalMatch 'L' t t' = directionalMatch 'R' t' t
directionalMatch 'A' t t' =
  directionalMatch 'T' t t'
    || directionalMatch 'B' t t'
    || directionalMatch 'L' t t'
    || directionalMatch 'R' t t'
directionalMatch _ _ _ = False

checkMatch :: Char -> Tile -> [Tile] -> [Tile]
checkMatch d v vs = matches where matches = filter (directionalMatch d v) vs

matchCount :: Char -> Tile -> [Tile] -> Int
matchCount d v vs = length $ checkMatch d v vs

checkAltMatches
  :: (Tile -> [Tile] -> Int) -> [TileID] -> AltTileSet -> [(TileID, [Int])]
checkAltMatches _ []       _ = []
checkAltMatches f [-1    ] t = checkAltMatches f (HM.keys t) t
checkAltMatches f (x : xs) t = (x, matches) : checkAltMatches f xs t
 where
  v       = HM.findWithDefault [] x t
  vs      = concat . HM.elems . HM.delete x $ t
  matches = map (`f` vs) v

getCorners :: AltTileSet -> [TileID]
getCorners =
  map fst . filter (all (== 2) . snd) . checkAltMatches (matchCount 'A') [-1]

topLeftHelper :: [Tile] -> [Tile] -> Tile
topLeftHelper [] _ = []
topLeftHelper (x : xs) y | (t, l, b, r) == (0, 0, 1, 1) = x
                         | otherwise                    = topLeftHelper xs y
 where
  t = matchCount 'T' x y
  l = matchCount 'L' x y
  b = matchCount 'B' x y
  r = matchCount 'R' x y

initTopLeft :: AltTileSet -> TileID -> (TileID, Tile)
initTopLeft t x = (x, matches)
 where
  v       = HM.findWithDefault [] x t
  vs      = concat . HM.elems . HM.delete x $ t
  matches = topLeftHelper v vs

removeBorders :: [a] -> [a]
removeBorders = init . drop 1

attachRight :: [[a]] -> [[a]] -> [[a]]
attachRight = zipWith (\x y -> x ++ removeBorders y)

attachBottom :: [[a]] -> [[a]] -> [[a]]
attachBottom x y = y ++ x

generateRow :: [[String]] -> Tile -> AltTileSet -> ([[String]], AltTileSet)
generateRow r t a | null matches = (r, a)
                  | otherwise    = generateRow r' v a'
 where
  matches =
    filter (not . null . snd)
      . HM.toList
      . HM.map (filter (directionalMatch 'R' t))
      $ a
  [(k, [v])] = matches
  a'         = HM.delete k a
  r'         = r ++ [v]

generateRows :: [[String]] -> Tile -> AltTileSet -> [[String]]
generateRows rs t a0 | null matches = rs ++ [removeBorders r']
                     | otherwise    = generateRows rs' v a2
 where
  (r, a1) = generateRow [] t a0
  r'      = foldl1 attachRight . (map removeBorders t :) $ r
  matches =
    filter (not . null . snd)
      . HM.toList
      . HM.map (filter (directionalMatch 'B' t))
      $ a1
  [(k, [v])] = matches
  a2         = HM.delete k a1
  rs'        = rs ++ [removeBorders r']

monster :: [String]
monster =
  ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]

findMonsterHelper :: [String] -> Int
findMonsterHelper x@[_ : as, _ : bs, _ : cs]
  | length (head x) < l = 0
  | found monster       = 1 + findMonsterHelper [as, bs, cs]
  | otherwise           = findMonsterHelper [as, bs, cs]
 where
  l     = length . head $ monster
  m     = map (take l) x
  found = all (all (uncurry (==)) . filter ((== '#') . snd)) . zipWith zip m
findMonsterHelper _ = 0

findMonsters :: [String] -> Int
findMonsters x | length x < length monster = 0
               | otherwise                 = count + findMonsters (b : c : ds)
 where
  (a : b : c : ds) = x
  count            = findMonsterHelper [a, b, c]

part1 :: [String] -> Int
part1 = product . getCorners . initAltTiles . initInput

part2 :: [String] -> Int
part2 x = countGrid - number * countMonster
 where
  altTiles     = initAltTiles . initInput $ x
  corners      = getCorners altTiles
  (k, v)       = initTopLeft altTiles . head $ corners
  remaining    = HM.delete k altTiles
  rows         = generateRows [] v remaining
  grid         = foldl1 attachBottom rows
  count        = length . filter (== '#') . concat
  countGrid    = count grid
  countMonster = count monster
  transforms   = applyTransforms grid
  monsters     = map findMonsters transforms
  number       = maximum monsters

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . part1 $ input
  printf "  Part 2: %d\n" . part2 $ input

main :: IO ()
main = getArgs >>= mapM_ output
