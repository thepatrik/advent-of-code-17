{-
  http://adventofcode.com/2017/day/4

  17  16  15  14  13
  18   5   4   3  12
  19   6   1*  2  11
  20   7   8   9* 10
  21  22  23  24  25*

  Solution is based on the fact that each bottom right cell
  an odd perfect square (1, 9, 25, 49, 81).

  Test values: 12, 23, 1024, 347991
  Should yield: 3, 2, 31, 480
-}

prompt x = do
  putStrLn x
  number <- getLine
  return number

main = do
  number <- prompt "Input number: "
  let num = read number :: Int
  let sq = getOddSquare num 1
  let corners = getCorners sq 0
  print $ getDistance corners 0 sq num

getOddSquare :: Int -> Int -> Int
getOddSquare n sq
  | sq * sq < n = getOddSquare n (sq + 2)
  | otherwise = sq

getCorners :: Int -> Int -> [Int]
getCorners sq i
  | i == 5 = []
  | otherwise = do
    let xs = getCorners sq (i + 1)
    (sq * sq - i * (sq - 1)) : xs

getDistance :: [Int] -> Int -> Int -> Int -> Int
getDistance corners count sq n
  | dist <= (sq - 1) `div` 2 = sq - 1 - dist
  | otherwise = getDistance corners (count + 1) sq n
  where dist = abs ((corners !! count) - n)