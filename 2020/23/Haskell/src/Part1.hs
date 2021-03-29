module Part1 where

readInput :: String -> [Int]
readInput = map (\x -> read [x])

findDestination :: Int -> [Int] -> Int
findDestination n x = destination
 where
  destination | null candidates = largest
              | otherwise       = fst . head $ candidates
  smallest     = minimum x
  largest      = maximum x
  destinations = takeWhile (>= smallest) . iterate (-1 +) $ n
  candidates   = filter snd . map (\a -> (a, a `elem` x)) $ destinations

move :: [Int] -> [Int]
move x0 = x5
 where
  current     = head x0
  x1          = drop 1 x0
  picked      = take 3 x1
  x2          = drop 3 x1
  destination = findDestination current x2
  x3          = takeWhile (/= destination) x2
  x4          = drop 1 . dropWhile (/= destination) $ x2
  x5          = x3 ++ [destination] ++ picked ++ x4 ++ [current]

solve :: Int -> [Int] -> [Int]
solve p =
  takeWhile (/= 1)
    . drop 1
    . dropWhile (/= 1)
    . cycle
    . last
    . take (p + 1)
    . iterate move

part1 :: String -> String
part1 = concatMap show . solve 100 . readInput
