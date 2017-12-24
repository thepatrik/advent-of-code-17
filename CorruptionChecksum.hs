{-
  http://adventofcode.com/2017/day/2
-}

main :: IO ()
main = do
  contents <- readFile "data/chksum.txt"
  let fileLines = lines contents
  print $ itFile 0 fileLines

itFile :: Int -> [String] -> Int
itFile n fileLines
  | n == length fileLines - 1 = diff $ toIntList (fileLines !! n)
  | otherwise = do
    let count = itFile (n + 1) fileLines
    count + (diff $ toIntList (fileLines !! n))

diff :: [Int] -> Int
diff nums = do
  let res = hiLo nums 0
  fst res - snd res

hiLo :: [Int] -> Int -> (Int, Int)
hiLo nums n
  | n == length nums - 1 = (curr, curr)
  | otherwise = do
    let res = hiLo nums (n + 1)
    let hi = if curr > fst res
             then curr
             else fst res
    let lo = if curr < snd res
             then curr
             else snd res
    (hi, lo)
    where curr = nums !! n

toIntList :: String -> [Int]
toIntList s = map (read::String->Int) $ words $ s