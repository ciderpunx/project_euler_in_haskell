-- A unit fraction contains 1 in the numerator. 
-- The decimal representation of the unit fractions with denominators 2 to 10 are given:

--    1/2  =   0.5
--    1/3  =   0.(3)
--    1/4  =   0.25
--    1/5  =   0.2
--    1/6  =   0.1(6)
--    1/7  =   0.(142857)
--    1/8  =   0.125
--    1/9  =   0.(1)
--    1/10  =  0.1 
--
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit 
-- recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
import Data.Ratio

main :: IO ()
main = print answer

-- Return a tuple of: 
-- * The unit denominator of the decimal expansion of a unit fraction with longest 
--   period length (recurring cycle/reperand)
-- * The period length itself
answer :: (Integer, Integer)
answer = 
    foldl (\xs@(_,x) ys@(_,y) -> if x>y then xs else ys) (1,0) tuples 
  where
    tuples   = [ (p,per) | p <- [1,3..1000], let per = period p ]

-- For xs divisible by 5 or 2, first construct a ratio and call rationalize
-- to get the repeating cycle part of the number
-- Then do:
--  1. For x from 1 to p-1 and build an x long list of 9s
--  2. Take the first list of nines that p divides and return its length
--  3. If p divides none of them, then there is not a cycle
period :: Integer -> Integer
period p =
    ends1379 $ if divisibleBy5or2 p then rationalize (1 % p) 2 else p

-- For a denominator ending 1,3,7, or 9 call cyc on it
-- if cyc is empty then return 0
-- otherwise return the length of the head of cyc p
-- which is the length of the cyclic period
ends1379 :: Integer -> Integer
ends1379 p =
    if null cp then 0 else toInteger . length . show $ head cp
  where 
    cp = cyc p

-- If numerator of x is divisible for 2 or 5, we know it terminates, but that it will start with
-- some non-cyclical part. We can get rid of that part by multiplying the numerator
-- by increasing powers of ten until we get a denominator that is not divisible 
-- by 5 or 2
-- method due to: http://hr.userweb.mwn.de/numb/period.html
rationalize :: Ratio Integer -> Integer -> Integer
rationalize x t =
    if divisibleBy5or2 dx then rationalize (x * (10^t)) (t+1) else dx
  where
    dx = denominator x

-- Given a denominator p returns the first 10^x-1 which p divides
cyc :: Integer -> [Integer]
cyc p = dropWhile (\x -> x `mod` p /= 0 ) $ nines p

-- Construct a list of 10^x-1 for xs 1..p-1
nines :: Integer -> [Integer]
nines p = map (\n -> 10^n-1) [1..p-1]

-- Save some typing return true if n is divisible by 5 or 2
divisibleBy5or2 :: Integer -> Bool
divisibleBy5or2 n = n `mod` 5 == 0 || n `mod` 2 == 0

---- test data cf: https://oeis.org/A051626
--a051626 :: [Integer]
--a051626 = [0, 0, 1, 0, 0, 1, 6, 0, 1, 0, 2, 1, 6, 6, 1, 0, 16, 1, 18, 0, 6, 2, 22, 1, 0, 6, 3, 6, 28, 1, 15, 0, 2, 16, 6, 1, 3, 18, 6, 0, 5, 6, 21, 2, 1, 22, 46, 1, 42, 0, 16, 6, 13, 3, 2, 6, 18, 28, 58, 1, 60, 15, 6, 0, 6, 2, 33, 16, 22, 6, 35, 1, 8, 3, 1, 18, 6, 6, 13, 0, 9, 5, 41, 6, 16, 21, 28, 2, 44, 1]
--
--testP :: [(Int, Bool, Integer, Integer)]
--testP = map (\n -> (n+1, dat !! n == a051626 !! n, dat !! n, a051626 !!n)) [0..89]
--  where
--    dat = map period [1..90]
--
--testFailures :: [(Int, Bool, Integer, Integer)]
--testFailures = filter (\(_,b,_,_)->not b) testP
--
---- Implementing a thing from wikipedia to show the cyclic part 
---- but does not calculate its length
----periodOf :: Int -> String
--periodOf p =
--    show o
--  where
--    o = (b^(p - 1))`div`p
--    b = 10
