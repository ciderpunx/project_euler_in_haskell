-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.
--
--
-- Notes: These are called factorians, and there are only 4 1,2,145 and 40585
-- We ignore 1,2 (as they aren't sums) and can deduce that the lower bound is therefore 10 -- 
-- the first number that has 2 digits to sum.
--
-- Now we have to work out the upper bound. 
--  From wikipedia: 
--    If n is a natural number of d digits that is a factorion, then 10d − 1 ≤ n ≤ 9!d. 
--    This fails to hold for d ≥ 8 thus n has at most 7 digits, and the first upper bound 
--    is 9,999,999. But the maximum sum of factorials of digits for a 7 digit number is 
--    9!*7 = 2,540,160 establishing the second upper bound. 
--
--    Going further, 9!6 is 2,177,280, and the only 7 digit number not larger than 
--    2,540,160 containing six 9′s is 1,999,999, which is not a factorion by inspection. 
--    The next highest sum would be given by 1,999,998, yielding a third upper 
--    bound of 1,854,721.
--
--  So, it sounds like our upper bound ought to be 1,854,721?
--
--  I actually approached this in a general way to start off with, ignoring the
--  upper bound problem. 
import Data.Char

main :: IO ()
main = print $ sumFactoriansTo 1854721

sumFactoriansTo :: Int -> Int
sumFactoriansTo = sum . factoriansTo

factoriansTo :: Int -> [Int]
factoriansTo n = [x | x <- [10..n], isFactorian x]

isFactorian :: Int -> Bool
isFactorian x = x == facSum x 

facSum :: Int -> Int
facSum = sum . map (fac . digitToInt) . show 

fac :: Int -> Int
fac n = product [1..n]
