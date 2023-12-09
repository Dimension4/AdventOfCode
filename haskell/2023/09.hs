parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

window :: [a] -> [(a, a)]
window (x : y : xs) = (x, y) : window (y : xs)
window _ = []

gradients :: [Int] -> [[Int]]
gradients xs
  | all (== 0) xs = []
  | otherwise = xs : gradients (grads xs)
  where
    grads = map (uncurry (flip (-))) . window

extrapolateForwards :: [Int] -> Int
extrapolateForwards xs = sum $ map last $ gradients xs

extrapolateBackwards :: [Int] -> Int
extrapolateBackwards xs = foldr ((-) . head) 0 (gradients xs)

main = do
  content <- readFile "09.txt"
  let history = parseInput content
      res1 = sum $ map extrapolateForwards history
      res2 = sum $ map extrapolateBackwards history
  putStrLn ("part 1: " ++ show res1)
  putStrLn ("part 2: " ++ show res2)
