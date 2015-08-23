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
import Data.List


-- for x from 1 to p-1 and build an x long list of 9s
-- for each of these, throw it away if it cannot be divided by p
-- take the first list of nines that p divides and return its length
-- if p divides none of them, then there is not a cycle
-- this method works, mostly, except for some multiples of 3 and 7 
period p =
    rep
  where
    rep   = if length cyc == 0
            then 0
            else length . show $ head cyc
    cyc   = dropWhile (\x -> gcd x p == 1) nines
    nines = map (\n -> (base^n)-1) [1..p-1]
    base  = 10

answer = 
    foldl (\xs@(_,x) ys@(_,y) -> if x>y then xs else ys) (1,0) tuples 
  where
    tuples   = [ (p,per) | p <- [1,3..1000], let per = period p ]


-- test data cf: https://oeis.org/A051626
a051626 = [0, 0, 1, 0, 0, 1, 6, 0, 1, 0, 2, 1, 6, 6, 1, 0, 16, 1, 18, 0, 6, 2, 22, 1, 0, 6, 3, 6, 28, 1, 15, 0, 2, 16, 6, 1, 3, 18, 6, 0, 5, 6, 21, 2, 1, 22, 46, 1, 42, 0, 16, 6, 13, 3, 2, 6, 18, 28, 58, 1, 60, 15, 6, 0, 6, 2, 33, 16, 22, 6, 35, 1, 8, 3, 1, 18, 6, 6, 13, 0, 9, 5, 41, 6, 16, 21, 28, 2, 44, 1]


testP = map (\n -> (n+1, dat !! n == a051626 !! n, dat !! n, a051626 !!n)) [0..89]
  where
    dat = map period [1..90]

testFailures = filter (\(_,b,_,_)->not b) testP

-- early attempt, wrong answer for multiples of 5, some other numbers
period' p = 
  if even p
    then 0
    else if p`mod`5 == 0
      then 0
      else head [q | q <- [1..], (10^q - 1)`mod`p==0]

-- implementing a thing from wikipedia to show the cyclic part but does not calculate its length
periodOf p =
    show o
  where
    o = (b^(p - 1))`div`p
    b = 10
--
---- all decimals for d 2..999 2 is at index 0 etc
--decimals :: [Decimal]
--decimals = zipWith (/) [1,1..] [2..5]
--
---- string of just the denominator
--denom = drop 2 . show 
--
---- get denominator strings for a list
--denoms = map denom 
--
--groupedDenoms = group . init . tails $ denoms decimals
--
--cycles xs = filter (iscycle) xs
--  where
--    cycle xs' = case length xs' of 
--                  0          -> False
--                  1          -> xs' == reverse xs'
--                  otherwise  -> True
--
--iscycle xs = 
--    y == z 
--  where 
--    (y,z) = splitAt (length xs `div` 2) xs
