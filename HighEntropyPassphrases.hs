{-
  http://adventofcode.com/2017/day/4
-}

import Data.List

main = do
  putStrLn "Input passphrase: "
  passphrase <- getLine
  print $ isUnique $ words passphrase

isUnique :: [String] -> Bool
isUnique passphrase = length passphrase == length (nub passphrase)