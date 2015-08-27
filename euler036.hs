-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
-- 
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2
-- 
-- (Please note that the palindromic number, in either base, may not include leading zeros.)
import Digits
import Control.Monad

main :: IO ()
main = print answer

answer :: Int
answer = sum $ map binToDec $ filter (decPal . binToDec) binPals

--- Get all the binary palindromes first. There are 2046 of them, rather than
-- 1110 decimal ones, however you can do this:
-- This nearly halves the amount of time it takes to get our answer
-- (.338s instead of .521 on my laptop, using runghc) compared to previous approach
binPals :: [String]
binPals =
    concatMap (revpend . dropWhile (=='0') ) $ replicateM 10 "10"
  where
    revpend x = if null x 
                then []
                else [x ++ reverse x, x ++ tail (reverse x)]

-- Here's what goes on:
-- make lists of length 10 where all the digits are 1 or 0 (1024 of them)
-- then drop any leading zeros
-- then for lists of length < 10
--    append each list to its reverse 
--    and the tail of its reverse
-- we know that palindromes in binary must be odd (see below) and 
-- since we are looking for ones under a million we know the largest is 20 bits 
-- long (i.e. 999999):
-- *Main Data.List Control.Monad> decToBin 999999
-- [1,1,1,1,0,1,0,0,0,0,1,0,0,0,1,1,1,1,1,1]
-- *Main Data.List Control.Monad> length $ decToBin 999999
-- 20
--

-- Take a string of 1s and 0s and convert into its integer representation
binToDec :: String -> Int
binToDec "" = 0
binToDec (x:xs) = binToDec xs + (n * 2 ^length xs)
  where
    n = read [x]

-- Is an int palindromic
decPal :: Int -> Bool
decPal = isPal . digits

-- Is a list the same as the reverse of itself (palindromic)
isPal :: Eq a => [a] -> Bool
isPal x = x == reverse x

------------------------------------------
-- An older, slower approach
-- Cut the problem space in half -- binary palindromes must begin and end in 1
-- because 2^1 is 1 and so all our palindromes have to end 1 (remember leading 1 rule)
-- answer' :: Int
-- answer' = sum $ allPals [1,3..999999]
-- 
-- -- Given a list of numbers in base 10, find the ones that are palindromes in both
-- -- base 2 and base 10
-- allPals :: [Int] -> [Int]
-- allPals = filter bothPal 
-- 
-- -- Are an int and its binary representation palindromic
-- bothPal :: Int -> Bool
-- bothPal x = decPal x && binPal x 
-- 
-- -- Is the binary representation of an int palindromic
-- binPal :: Int -> Bool
-- binPal = isPal . decToBin
-- 
-- -- making this reasonably fast helps us a lot
-- decToBin :: Int -> [Int]
-- decToBin x = reverse $ decToBin' x
--     where
--       decToBin' 0 = []
--       decToBin' y = let (a,b) = quotRem y 2 in b : decToBin' a
-- 
-- An experiment, turns out it is no faster to only apply the binToDec once
-- -- Given a list of binary palindromes, 
-- --    take the first 
-- --    convert to decimal
-- --      if it's palindromic in base 10 return it plus the palindromic ones from the tail, 
-- --      if not just return the decimally palindromic ones from the tail
-- decPalsOf :: [String] -> [Int]
-- decPalsOf [] = []
-- decPalsOf (b:bs) =
--     if decPal d
--     then d : decPalsOf bs
--     else decPalsOf bs
--   where
--     d = binToDec b

