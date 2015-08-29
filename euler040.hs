-- An irrational decimal fraction is created by concatenating the positive integers:
-- 
-- 0.123456789101112131415161718192021...
-- 
-- It can be seen that the 12th digit of the fractional part is 1.
-- 
-- If dn represents the nth digit of the fractional part, find the value of the following expression.
-- 
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
import Data.Char

-- let's try the simplest thing that could possibly work. no maths involved!

main :: IO ()
main = print answer

-- pull out the digits from the string at the right indexes (start at highest index as we memoize 
-- once we have seen it)
answer :: Int
answer = product $ map (digitToInt . (irrDec!!)) [999999,99999,9999,999,99,9,0]

-- build a string by concatenating the digits of all numbers to a million
irrDec :: String
irrDec = concatMap show [1..1000000]
