import Data.List (intersect)
import Data.Map.Strict qualified as Map
import Data.Text (empty, pack, splitOn, unpack)

parseCard :: String -> [[Int]]
parseCard =
  map (map (read . unpack) . filter (/= empty) . splitOn (pack " "))
    . splitOn (pack " | ")
    . last
    . splitOn (pack ": ")
    . pack

scoreCard :: [[Int]] -> Int
scoreCard [winning, picks] =
  case length $ winning `intersect` picks of
    x | x > 0 -> 2 ^ (x - 1)
    _ -> 0

playCards :: [[[Int]]] -> [Int]
playCards cards = iter 1 (Map.fromList $ zip [1 .. length cards] [1, 1 ..]) cards
  where
    iter _ instances [] = Map.elems instances
    iter i instances ([winning, picks] : rest) = iter (i + 1) newInstances rest
      where
        dupes = length $ winning `intersect` picks
        Just inc = Map.lookup i instances
        newInstances = foldl (flip (Map.update (Just . (+) inc))) instances [(i + 1) .. (i + dupes)]

main = do
  content <- readFile "04.txt"
  let cards = map parseCard $ lines content
      res1 = sum $ map scoreCard cards
      res2 = sum $ playCards cards
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
