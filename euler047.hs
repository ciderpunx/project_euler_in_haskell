-- The first two consecutive numbers to have two distinct prime factors are:
-- 
-- 14 = 2 × 7
-- 15 = 3 × 5
-- 
-- The first three consecutive numbers to have three distinct prime factors are:
-- 
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
-- 
-- Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
import Data.Numbers.Primes
import Data.Set (fromList, size, Set)

-- A couple of type synonyms for convenience
type Num3Tuple = (Integer,Set Integer, Int)
type Num3Tuple4Tuple = (Num3Tuple, Num3Tuple, Num3Tuple, Num3Tuple)

main :: IO ()
main =
  print answer

answer :: Integer
answer =
    first3 . first4 . head $ consec4dpfs allDPFS

-- Upper bound on the ints to search for their distinct prime factors (DPFs)
maxI :: Integer
maxI = 140000

-- Get 3 tuples of:
--   a number,
--   its distinct prime factors,
--   and the number of DPFs
allDPFS :: [Num3Tuple]
allDPFS =
    map (\n -> (n, dpfs n, size $ dpfs n)) [1..maxI]

-- Find the distinct prime factors for a number
dpfs :: Integer -> Set Integer
dpfs =
    fromList . primeFactors

-- Given a list of Nu3Tuples, go through in groups of 4
-- and return those that have 4 DPFs
consec4dpfs :: [Num3Tuple] -> [Num3Tuple4Tuple]
consec4dpfs (a:b:c:d:xs) = 
  if all (==4) $ map third3 (a:b:c:[d])
    then (a,b,c,d) : consec4dpfs (b:c:d:xs)
    else consec4dpfs $ b:c:d:xs
consec4dpfs _ = []

-- return 3rd elt of a 3 tuple
third3  :: (a,b,c) -> c
third3 (_,_,x) = x

-- return 1st elt of a 3 tuple
first3 :: (a,b,c) -> a
first3 (x,_,_) = x

-- return 1st elt of a 4 tuple
first4 :: (a,b,c,d) -> a
first4 (x,_,_,_) = x
