module Part2 where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List

type Point = (Int, Int, Int, Int)
type HyperCube = HashMap Point Char

initHyperCube :: Point -> HyperCube -> String -> HyperCube
initHyperCube _ c [] = c
initHyperCube p c (x : xs)
  | x == '\n' = initHyperCube (px + 1, 0, pz, pw) c xs
  | otherwise = initHyperCube (px, py + 1, pz, pw) c' xs
 where
  (px, py, pz, pw) = p
  c'               = HM.insert p x c

hyperCubeLookup :: Point -> HyperCube -> Char
hyperCubeLookup = HM.findWithDefault '.'

getNeighbours :: Point -> [Point]
getNeighbours (x, y, z, w) =
  [ (a, b, c, d)
  | d <- w'
  , c <- z'
  , b <- y'
  , a <- x'
  , (a, b, c, d) /= (x, y, z, w)
  ]
 where
  f t = [t - 1 .. t + 1]
  (x', y', z', w') = (f x, f y, f z, f w)

getUniqueNeighbours :: [Point] -> [Point]
getUniqueNeighbours = map head . group . sort . concatMap getNeighbours

countActiveNeighbours :: HyperCube -> [Point] -> Int
countActiveNeighbours c = length . filter ((== '#') . (`hyperCubeLookup` c))

stepHyperCube :: HyperCube -> HyperCube -> [Point] -> HyperCube
stepHyperCube _  c []       = c
stepHyperCube c0 c (p : ps) = stepHyperCube c0 c' ps
 where
  this       = hyperCubeLookup p c0
  neighbours = getNeighbours p
  active     = countActiveNeighbours c0 neighbours
  c' | this == '#' && active `notElem` [2, 3] = HM.insert p '.' c
     | this == '.' && active == 3             = HM.insert p '#' c
     | otherwise                              = c

cycleHyperCube :: Int -> Int -> HyperCube -> HyperCube
cycleHyperCube n0 n c | n == n0   = c
                      | otherwise = cycleHyperCube n0 (n + 1) c'
 where
  k  = HM.keys c
  k' = k ++ getUniqueNeighbours k
  c' = stepHyperCube c c k'

part2 :: String -> Int
part2 =
  length
    . filter (== '#')
    . HM.elems
    . cycleHyperCube 6 0
    . initHyperCube (0, 0, 0, 0) HM.empty
