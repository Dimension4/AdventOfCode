import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (pack, splitOn, unpack)

data Hand = Hand {cards :: [Int], bid :: Int} deriving (Show)

possibleCards = "23456789TJQKA"

parseInput :: String -> [Hand]
parseInput = map (parseSet . map unpack . splitOn (pack " ") . pack) . lines
  where
    parseSet [cards, bid] = Hand {cards = mapMaybe (`elemIndex` possibleCards) cards, bid = read bid}

scoreHand :: Int -> [Int] -> Int
scoreHand joker cards =
  case fixed of
    [5] -> 6
    [4, 1] -> 5
    [3, 2] -> 4
    3 : _ -> 3
    [2, 2, 1] -> 2
    2 : _ -> 1
    _ -> 0
  where
    jokerLess = filter (/= joker) cards
    sortCards = sortBy (comparing Down) . map length . group . sort
    sorted = sortCards jokerLess
    fixed =
      case sorted of
        [] -> [5]
        x:xs -> x + (length cards - length jokerLess) : xs

rankHands :: Int -> [Hand] -> [Hand]
rankHands joker = sortBy rank
  where
    valueJoker x = if x == joker then -1 else x
    rank a b =
      case comparing (scoreHand joker . cards) a b of
        EQ -> comparing (map valueJoker . cards) a b
        x -> x

winnings :: [Hand] -> Int
winnings hands = sum $ zipWith (*) [1 ..] $ map bid hands

main = do
  content <- readFile "07.txt"
  let hands = parseInput content
      res1 = winnings $ rankHands (-1) hands
      Just joker = 'J' `elemIndex` possibleCards
      res2 = winnings $ rankHands joker hands
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
