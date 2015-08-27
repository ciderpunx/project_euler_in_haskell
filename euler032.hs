--We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
--
--The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
--
--Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
--HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
import Data.List

main :: IO ()
main =
    print answer

-- The trick here is to reduce the search space to something more manageable 
-- than two lists of 1..987654321. Avoid dupes by keeping a<=b, make sure c
-- never goes over 9876 and remember that a will only need to go up to 99 
-- (highest 2 digit number). Could optimize further but it works quickly 
-- enough as is.
-- Also, I forgot to nub the list first time. Doh!
answer :: Int
answer =
    sum $ nub [c | a <- [0..99], b <- [a..9876], let c = a*b, c<=9876 && isPandigitalCombo a b c]

-- Do a b and c contain the digits 1..9
isPandigitalCombo :: Int -> Int -> Int -> Bool
isPandigitalCombo a b c =
    sort(show a ++ show b ++ show c) == "123456789"
