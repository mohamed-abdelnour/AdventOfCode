import           Data.Bifunctor
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           System.Environment
import           Text.Printf

type Food = Int
type Ingredient = String
type Allergen = String
type FoodMap = HashMap Food [Ingredient]
type AllergenMap = HashMap Allergen [Food]

updateMap :: AllergenMap -> Food -> [Allergen] -> AllergenMap
updateMap a _ []       = a
updateMap a n (x : xs) = updateMap a' n xs
  where a' = HM.insertWith (flip (++)) x [n] a

parseInput
  :: (FoodMap, AllergenMap) -> Int -> [String] -> (FoodMap, AllergenMap)
parseInput (f, a) _ []        = (f, a)
parseInput (f, a) n ([] : xs) = parseInput (f, a) n xs
parseInput (f, a) n (x  : xs) = parseInput (f', a') (n + 1) xs
 where
  x'          = words x
  ingredients = takeWhile (/= "(contains") x'
  allergens   = map init . drop 1 . dropWhile (/= "(contains") $ x'
  f'          = HM.insert n ingredients f
  a'          = updateMap a n allergens

toRemove :: FoodMap -> [Food] -> [Ingredient]
toRemove f n =
  map head
    . filter ((== length n) . length)
    . group
    . sort
    . concatMap (\x -> HM.findWithDefault [] x f)
    $ n

match :: [(Allergen, [Ingredient])] -> [(Allergen, [Ingredient])]
match x | f x       = x
        | otherwise = match (alreadyMatching ++ needMatching')
 where
  alreadyMatching  = filter ((== 1) . length . snd) x
  alreadyMatching' = concatMap snd alreadyMatching
  needMatching     = filter ((> 1) . length . snd) x
  needMatching' =
    map (second (filter (`notElem` alreadyMatching'))) needMatching
  f = all ((== 1) . length . snd)

solve :: [String] -> (Int, String)
solve x = (part1, part2)
 where
  (f, a)    = parseInput (HM.empty, HM.empty) 0 x
  as        = HM.elems a
  remove    = map head . group . sort . concatMap (toRemove f) $ as
  allergens = HM.keys a
  remove'   = map (toRemove f) as
  possibleMatches =
    sortBy (\(_, p) (_, q) -> compare (length p) (length q))
      . zip allergens
      $ remove'
  part1 = length . concatMap (filter (`notElem` remove)) . HM.elems $ f
  part2 =
    init
      . concatMap (++ ",")
      . concatMap snd
      . sortBy (\(p, _) (q, _) -> compare p q)
      . match
      $ possibleMatches

output :: String -> IO ()
output path = do
  input <- lines <$> readFile path
  let (p1, p2) = solve input
  printf "File: %s\n"     path
  printf "  Part 1: %d\n" p1
  printf "  Part 2: %s\n" p2

main :: IO ()
main = getArgs >>= mapM_ output
