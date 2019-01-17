{-
  http://adventofcode.com/2017/day/1
-}

import System.Environment

main = do
  num <- getIntegerArg
  print $ snd (count num 0)

getIntegerArg :: IO Integer
getIntegerArg = fmap (read . head) getArgs

countDigits :: (Integral a) => a -> a -> a
countDigits num count
    | num == 0 = count
    | otherwise = countDigits (num `div` 10) count+1

getDigit :: (Integral a) => a -> a -> a
getDigit num n
    | n == 0 = snd (divMod num 10)
    | otherwise = snd (divMod num (10 ^ (n+1))) `div` (10 ^ n)

count :: (Integral a) => a -> a -> (a, a)
count num n
    | n == countDigits num 0 = ((getDigit num 0), 0)
    | otherwise = do
      let c = count num (n+1)
      if fst c == getDigit num n
      then ((getDigit num n), (snd c + fst c))
      else ((getDigit num n), snd c)
