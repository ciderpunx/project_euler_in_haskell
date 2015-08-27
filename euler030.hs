--Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
--
--    1634 = 14 + 64 + 34 + 44
--    8208 = 84 + 24 + 04 + 84
--    9474 = 94 + 44 + 74 + 44
--
--As 1 = 14 is not a sum it is not included.
--
--The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
--Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
--
-- I wrote a generalized powSum function to sum the digits (to the nth power) of an integer x
-- Then I use a list comprehension to build a list of all Ints within my bounds. This can
-- be summed trivially. The lower bound is the lowest 2 digit number. The upper is more 
-- complicated
--
-- The upper bound works like this: 
--  * for the biggest 7 digit number (9999999), the sum of all the 5th powers has 6 digits i.e. 
--    > sum $ take 7 $ repeat (9^5) == 413343
--    Or we actually care about the length
--    > length . show . sum $ take 8 $ repeat (9^5) == 6 
--  * So no 7 digit number could be equal to the sum of its digits to the fifth power
--  * We thus know that we are dealing with a 6 or fewer digits and so we choose the largest 
--    6 digit number as our upper bound -- 999999
--  * But we also know that
--    > sum $ take 6 $ repeat (9^5) == 354294
--  * So we take that as our upper bound.
--
--  Wonder if we can go any lower?
import Data.Char

sumPS5s :: Int
sumPS5s = sum powSum5s

powSum5s :: [Int]
powSum5s = [x | x <- [10..354294], x == powSum 5 x]

powSum :: Int -> Int -> Int
powSum n x = sum $ map (flip (^) n . digitToInt) $ show x
