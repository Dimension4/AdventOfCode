{-# LANGUAGE RecordWildCards #-}

import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Text.Parsec (manyTill, parse, skipMany, skipMany1)
import Text.Parsec.Char (anyChar, char, digit, endOfLine, letter, spaces, string)
import Text.Parsec.Combinator (many1, sepBy1, sepEndBy1)
import Text.Parsec.Error (ParseError, errorMessages)
import Text.Parsec.String (Parser)

data Range = Range
  { dst :: Int,
    src :: Int,
    len :: Int
  }
  deriving (Show)

data Almanac = Almanac
  { seeds :: [Int],
    categories :: [[Range]]
  }
  deriving (Show)

createAlmanac :: String -> Either ParseError Almanac
createAlmanac = parse almanac ""
  where
    integer = read <$> many1 digit :: Parser Int
    integerList = integer `sepBy1` char ' '
    skipLine = manyTill anyChar endOfLine
    seedsParser = do
      string "seeds: "
      x <- integerList
      endOfLine
      return x
    categoryParser = do
      skipLine
      lists <- sepEndBy1 integerList endOfLine
      return $ map (\[a, b, c] -> Range {dst = a, src = b, len = c}) lists
    almanac = do
      seeds <- seedsParser
      endOfLine
      categories <- sepEndBy1 categoryParser endOfLine
      return Almanac {seeds = seeds, categories = categories}

getSeedLocations :: Almanac -> [Int]
getSeedLocations almanac = map (iter (categories almanac) []) $ seeds almanac
  where
    iter [] [] val = val
    iter (cat : tail) [] val = iter tail cat val
    iter cats (Range {..} : rest) val =
      if src <= val && val < src + len
        then iter cats [] $ val - src + dst
        else iter cats rest val

main = do
  content <- readFile "05.txt"
  let almanac =
        case createAlmanac content of
          Left e -> error $ show e
          Right p -> p
      res1 = minimum $ getSeedLocations almanac
      toRanges (x : y : xs) = [x .. x + y - 1] : toRanges xs
      toRanges [] = []
      newAlmanac = almanac {seeds = concat $ toRanges $ seeds almanac}
      res2 = minimum $ getSeedLocations newAlmanac
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
