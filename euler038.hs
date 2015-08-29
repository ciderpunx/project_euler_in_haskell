-- Take the number 192 and multiply it by each of 1, 2, and 3:
-- 
--     192 × 1 = 192
--     192 × 2 = 384
--     192 × 3 = 576
-- 
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
-- 
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
-- 
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
import Data.List
import Data.Ord

main :: IO ()
main = print answer

-- This is probably way more complex than it needs to be 
-- * map answerFor over 1..9 getting the pandigital series for [n*1], [n*1,n*2]
-- * filter out the ns for which no series exist
-- * concat the lists 
-- * get maximum 'n' from them
-- * and show the pandigital it generates
answer :: Int
answer = read . snd . maximumBy (comparing fst) . concat . filter (not . null) $ map answerFor [1..9]

-- Given an n find all the 9 pandigitals for 9..9999 * [1..n]
answerFor :: Int -> [(Int,String)]
answerFor n = filter ((""/=) . snd) $ map (try n) [9..9999]

-- Try an x and see if 1x,2x..nx is nine digit pandigital
-- If it is return a pair of it and the pandigital it generates
-- Otherwise it and ""
try :: Int -> Int -> (Int, String)
try n x =
    if ninePan q 
      then (x,q)
      else (x,"")
  where
    q = dropWhile (=='0') $ concatMap (show . (x*)) [1..n]
    ninePan r = length r == 9 && pandigital '9' r

pandigital :: Char -> String -> Bool
pandigital n xs = sort xs == ['1'..n]
