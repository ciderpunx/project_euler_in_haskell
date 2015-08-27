-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
-- 
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2
-- 
-- (Please note that the palindromic number, in either base, may not include leading zeros.)
import Digits

main :: IO ()
main = print answer

answer :: Int
answer = sum $ allPals [1,3..1000000]

-- Given a list of numbers in base 10, find the ones that are palindromes in both
-- base 2 and base 10
allPals :: [Int] -> [Int]
allPals = filter bothPal 

-- Are an int and its binary representation palindromic
bothPal :: Int -> Bool
bothPal x = decPal x && binPal x 

-- Is the binary representation of an int palindromic
binPal :: Int -> Bool
binPal = isPal . decToBin

-- Is an int palindromic
decPal :: Int -> Bool
decPal = isPal . digits

-- Is a list the same as the reverse of itself (palindromic)
isPal :: Eq a => [a] -> Bool
isPal x = x == reverse x

-- making this reasonably fast helps us a lot
decToBin :: Int -> [Int]
decToBin x = reverse $ decToBin' x
    where
      decToBin' 0 = []
      decToBin' y = let (a,b) = quotRem y 2 in b : decToBin' a
