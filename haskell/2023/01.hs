import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, find)
import Data.Bifunctor (first)

findDigits mapping s = fd * 10 + ld
  where
    findFirst s mapping = 
      case find ((`isPrefixOf` s) . fst) mapping of
        Just (_, x) -> x
        Nothing -> findFirst (tail s) mapping
    fd = findFirst s mapping
    ld = findFirst (reverse s) (map (first reverse) mapping)

main = do
  content <- readFile "01.txt"
  let
    input = lines content
    pattern1 = zip (map show [1..9]) [1..]
    res1 = sum $ map (findDigits pattern1) input
    pattern2 = pattern1 ++ zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..]
    res2 = sum $ map (findDigits pattern2) input
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
