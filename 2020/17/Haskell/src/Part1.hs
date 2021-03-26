module Part1 where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List

type Point = (Int, Int, Int)
type Cube = HashMap Point Char

initCube :: Point -> Cube -> String -> Cube
initCube _ c [] = c
initCube p c (x : xs) | x == '\n' = initCube (px + 1, 0, pz) c xs
                      | otherwise = initCube (px, py + 1, pz) c' xs
 where
  (px, py, pz) = p
  c'           = HM.insert p x c

cubeLookup :: Point -> Cube -> Char
cubeLookup = HM.findWithDefault '.'

getNeighbours :: Point -> [Point]
getNeighbours (x, y, z) =
  [ (a, b, c) | c <- z', b <- y', a <- x', (a, b, c) /= (x, y, z) ]
 where
  f t = [t - 1 .. t + 1]
  (x', y', z') = (f x, f y, f z)

getUniqueNeighbours :: [Point] -> [Point]
getUniqueNeighbours = map head . group . sort . concatMap getNeighbours

countActiveNeighbours :: Cube -> [Point] -> Int
countActiveNeighbours c = length . filter ((== '#') . (`cubeLookup` c))

stepCube :: Cube -> Cube -> [Point] -> Cube
stepCube _  c []       = c
stepCube c0 c (p : ps) = stepCube c0 c' ps
 where
  this       = cubeLookup p c0
  neighbours = getNeighbours p
  active     = countActiveNeighbours c0 neighbours
  c' | this == '#' && active `notElem` [2, 3] = HM.insert p '.' c
     | this == '.' && active == 3             = HM.insert p '#' c
     | otherwise                              = c

cycleCube :: Int -> Int -> Cube -> Cube
cycleCube n0 n c | n == n0   = c
                 | otherwise = cycleCube n0 (n + 1) c'
 where
  k  = HM.keys c
  k' = k ++ getUniqueNeighbours k
  c' = stepCube c c k'

part1 :: String -> Int
part1 =
  length
    . filter (== '#')
    . HM.elems
    . cycleCube 6 0
    . initCube (0, 0, 0) HM.empty
