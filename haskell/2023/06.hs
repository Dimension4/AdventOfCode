import Text.Parsec (manyTill, parse, lookAhead)
import Text.Parsec.Char (anyChar, char, digit, endOfLine)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.String (Parser)

parseInput :: String -> ([Int], [Int])
parseInput s =
  case parse inputParser "" s of
    Left err -> error $ show err
    Right x -> x
  where
    integer = read <$> many1 digit :: Parser Int
    integerList = integer `sepBy1` many1 (char ' ')
    skipToDigit = manyTill anyChar (lookAhead digit)
    inputParser = (,) <$> (skipToDigit >> integerList) <*> (skipToDigit >> integerList)

findBeatingConfigs :: Int -> Int -> Int
findBeatingConfigs time distance =
  length $ filter (\s -> s * (time - s) > distance) [1 .. time - 1]

fuse :: [Int] -> Int
fuse = read . concatMap show

main = do
  content <- readFile "06.txt"
  let (time, distance) = parseInput content
      res1 = product $ zipWith findBeatingConfigs time distance
      res2 = findBeatingConfigs (fuse time) (fuse distance)
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
