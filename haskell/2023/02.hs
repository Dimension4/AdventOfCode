import Data.List (transpose)
import Data.Text (pack, splitOn, unpack)

parseGame :: [Char] -> [[Int]]
parseGame input = parseCubes cubeSets
  where
    [_, cubeSets] = splitOn (pack ": ") $ pack input
    parseCubes = map parseCubeSet . splitOn (pack "; ")
      where
        parseCubeSet s = foldl buildCubeSet [0, 0, 0] $ colorValues s
        colorValues s = map (map unpack . splitOn (pack " ")) $ splitOn (pack ", ") s
        buildCubeSet [r, g, b] [x, "red"] = [r + read x, g, b]
        buildCubeSet [r, g, b] [x, "green"] = [r, g + read x, b]
        buildCubeSet [r, g, b] [x, "blue"] = [r, g, b + read x]

canBePlayed :: [Int] -> [[Int]] -> Bool
canBePlayed config game = and $ zipWith (<=) (minPlayableConfig game) config

minPlayableConfig :: [[Int]] -> [Int]
minPlayableConfig = map maximum . transpose

main = do
  content <- readFile "02.txt"
  let
    games = map parseGame $ lines content
    playable = filter (canBePlayed [12, 13, 14] . snd) $ zip [1 ..] games
    res1 = sum $ map fst playable
    res2 = sum $ map (product . minPlayableConfig) games
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
