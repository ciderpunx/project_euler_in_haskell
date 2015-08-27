module Digits where
import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

undigits :: [Int] -> Int
undigits = read . map intToDigit
