module Main where

import Data.Char

fac :: Integer -> Integer
fac n = product [1..n]

sumFac100 :: Int
sumFac100 = sum $ map digitToInt $ show $ fac 100

main :: IO ()
main = print $ show sumFac100
