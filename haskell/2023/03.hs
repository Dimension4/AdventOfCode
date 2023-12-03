import Data.Array (Array, array, bounds, elems)
import Data.Array.Base ((!), (//))
import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.List (groupBy, sortOn)

data Symbol = None | Some | MaybeGear Int
  deriving (Eq, Ord)

type CollisionMap = Array (Int, Int) Symbol

type Schematic = [[Char]]

data Part = Part {partId :: Int, symId :: Int}

createCollisionMap :: Schematic -> CollisionMap
createCollisionMap schematic = dilate 0 0 symbolMap
  where
    yMax = length schematic - 1
    xMax = length (head schematic) - 1
    dilate y x acc
      | y > yMax = acc
      | x > xMax = dilate (y + 1) 0 acc
      | otherwise =
          dilate y (x + 1) $
            case symbolMap ! (y, x) of
              None -> acc
              sym -> acc // [((y + dy, x + dx), sym) | dy <- [-1 .. 1], dx <- [-1 .. 1]]
    symbolMap =
      let elements =
            [ ((y, x), getSymbol y x $ schematic !! y !! x)
              | y <- [0 .. yMax],
                x <- [0 .. xMax]
            ]
       in array ((0, 0), (yMax, xMax)) elements
    getSymbol y x '*' = MaybeGear $ (yMax + 1) * x + y
    getSymbol _ _ x | isDigit x = None
    getSymbol _ _ '.' = None
    getSymbol _ _ _ = Some

findPartNumbers :: Schematic -> CollisionMap -> [Part]
findPartNumbers schematic collisionMap =
  iter (concat schematic) (elems collisionMap) 0 None []
  where
    iter [] [] _ _ acc = acc
    iter (x : xs) (y : ys) num symId acc
      | isDigit x = iter xs ys (digitToInt x + 10 * num) (max symId y) acc
      | otherwise = iter xs ys 0 None $
          case symId of
            MaybeGear g -> Part {partId = num, symId = g} : acc
            Some -> Part {partId = num, symId = 0} : acc
            _ -> acc

-- surrounds the whole schematic with a rectangle of dots (.)
-- this allows us to skip certain bounds checks and patterns later
extendSchematic :: Schematic -> Schematic
extendSchematic schematic = row : widened ++ [row]
  where
    widened = map (\l -> '.' : l ++ ['.']) schematic
    row = replicate (length $ head widened) '.'

findGearRatios :: [Part] -> [Int]
findGearRatios parts = map product gears
  where
    potentialGears = filter ((> 0) . symId) parts
    connectedParts = groupBy ((==) `on` symId) $ sortOn symId potentialGears
    gears = filter ((2 ==) . length) $ map (map partId) connectedParts

main = do
  content <- readFile "03.txt"
  let schematic = extendSchematic $ lines content
      collisionMap = createCollisionMap schematic
      parts = findPartNumbers schematic collisionMap
      res1 = sum $ map partId parts
      res2 = sum $ findGearRatios parts
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
