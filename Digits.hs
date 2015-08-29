module Digits where
import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

digitsLong :: Integer -> [Integer]
digitsLong = map (toInteger . digitToInt) . show

--- this is slightly slower for me
digits' :: Int -> [Int]
digits' = 
  map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)

undigitsLong :: [Integer] -> Integer
undigitsLong = 
    foldl addDigit 0
   where 
    addDigit num d = 10*num + d

undigits' :: [Int] -> Int
undigits' = fromIntegral . undigits

undigits :: [Int] -> Int
undigits = read . map intToDigit
