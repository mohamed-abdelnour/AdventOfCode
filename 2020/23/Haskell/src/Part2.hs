module Part2 where

import           Data.List
import qualified Data.Vector.Unboxed.Mutable   as MV
import           Data.Vector.Unboxed.Mutable    ( IOVector )

type Game = IOVector Int

maxCups :: Int
maxCups = 1000000

iterations :: Int
iterations = 10000000

readInput :: String -> [Int]
readInput = map (\x -> read [x])

initInput :: [Int] -> [(Int, Int)]
initInput [_         ] = []
initInput (x : y : zs) = (x, y) : initInput (y : zs)
initInput []           = []

initCups :: (Int, Int, Int) -> [(Int, Int)] -> [Int]
initCups (_, b, m) =
  map snd . sortBy (\(x, _) (y, _) -> compare x y) . ((b, m) :)

initGame :: Int -> [Int] -> Game -> IO ()
initGame i [] g | i == maxCups - 1 = return ()
                | otherwise = MV.write g i (i + 2) >> initGame (i + 1) [] g
initGame i (x : xs) g | i == maxCups - 1 = return ()
                      | otherwise = MV.write g i x >> initGame (i + 1) xs g

stepGame :: Int -> Int -> Game -> IO ()
stepGame i n0 g
  | i == iterations = return ()
  | otherwise = do
    p1 <- MV.read g (n0 - 1)
    p2 <- MV.read g (p1 - 1)
    p3 <- MV.read g (p2 - 1)
    let ps = [p1, p2, p3]
    n1 <- MV.read g (p3 - 1)
    MV.write g (n0 - 1) n1
    let c | (n0 - 1) `notElem` ps = n0 - 1
          | (n0 - 2) `notElem` ps = n0 - 2
          | (n0 - 3) `notElem` ps = n0 - 3
          | otherwise             = n0 - 4
    let d0 | c < 1     = maxCups
           | otherwise = c
    d1 <- MV.read g (d0 - 1)
    MV.write g (d0 - 1) p1
    MV.write g (p3 - 1) d1
    stepGame (i + 1) n1 g

solve :: String -> IO (Int, Int, Int)
solve x = do
  let parsed      = readInput x
  let parseInit   = initInput parsed
  let a@(n, _, _) = (head parsed, last parsed, maximum parsed + 1)
  let gameInit    = initCups a parseInit
  gameVec <- MV.new maxCups
  initGame 0 gameInit gameVec
  MV.write gameVec (maxCups - 1) n
  stepGame 0 n gameVec
  r1 <- MV.read gameVec 0
  r2 <- MV.read gameVec (r1 - 1)
  return (r1, r2, r1 * r2)
