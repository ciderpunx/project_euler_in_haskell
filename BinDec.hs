module BinDec where
-- Tools for converting between decimal and binary
import Digits

binToDec :: String -> Int
binToDec = bin2Dec . read

bin2Dec :: Int -> Int
bin2Dec = bd . digits

bd :: [Int] -> Int
bd b = 
  sum $ zipWith (\d o -> 2^o * d) b (reverse $ take (length b) [0..]::[Int])

decToBin :: Int -> String
decToBin = show . dec2Bin

dec2Bin :: Int -> Int
dec2Bin = undigits . db

db :: Int -> [Int]
db x = 
    reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in b : decToBin' a
