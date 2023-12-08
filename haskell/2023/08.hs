import Data.Char (ord)
import Data.IntMap qualified as IntMap
import Data.IntMap.Strict (IntMap)
import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (pack, splitOn, unpack)
import Text.Parsec (letter, lookAhead, manyTill, parse, string)
import Text.Parsec.Char (anyChar, char, digit, endOfLine)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.String (Parser)

encode :: String -> Int
encode [a, b, c] = ord a * 256 ^ 2 + ord b * 256 + ord c

parseInput :: String -> (String, IntMap (Int, Int))
parseInput s =
  case parse inputParser "" s of
    Left err -> error $ show err
    Right x -> x
  where
    node = encode <$> many1 letter
    definition = do
      key <- node
      string " = ("
      left <- node
      string ", "
      right <- node
      char ')'
      return (key, (left, right))
    inputParser = do
      steps <- many1 letter
      endOfLine
      endOfLine
      definitions <- definition `sepBy1` endOfLine
      return (steps, IntMap.fromList definitions)

traverseMap :: [Char] -> IntMap (Int, Int) -> Int
traverseMap instructions graph = iter (encode "AAA") (cycle instructions) 0
  where
    goal = encode "ZZZ"
    iter node _ steps | node == goal = steps
    iter node (path : tl) steps =
      case (IntMap.lookup node graph, path) of
        (Just (left, _), 'L') -> iter left tl (steps + 1)
        (Just (_, right), 'R') -> iter right tl (steps + 1)

main = do
  content <- readFile "08.txt"
  let (instructions, graph) = parseInput content
      res1 = traverseMap instructions graph
  -- res2 = winnings $ rankHands joker hands
  putStrLn ("part 1: " ++ show res1)

-- putStrLn ("part 2: " ++ show res2)
