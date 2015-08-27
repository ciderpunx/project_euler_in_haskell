-- Euler discovered the remarkable quadratic formula:
-- 
-- n² + n + 41
-- 
-- It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
-- 
-- The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
-- 
-- Considering quadratics of the form:
-- 
--     n² + an + b, where |a| < 1000 and |b| < 1000
-- 
--     where |n| is the modulus/absolute value of n
--     e.g. |11| = 11 and |−4| = 4
-- 
-- Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
import Prime
import Data.Ord
import Data.List

main :: IO ()
main = print answer

-- Pull out the third elt of the answer with the longest sequence -- that should 
-- be the answer
answer :: Integer
answer = third $ maximumBy (comparing fourth) answers

-- Give a list of a,b,product of a & b and length of the longest sequence of primes 
answers :: [(Integer, Integer, Integer, Integer)]
answers = 
    [(a,b,a*b,toInteger $ length ps) | a <- [-1000..1000], b <- primeList, let ps = primesFor a b]
  where
    primeList = 0 : 1 : (primesTo 1000)

-- Build a list of primes for n² + an + b
primesFor :: Integer -> Integer -> [Integer]
primesFor a b = 
    takeWhile primeOrZero $ map (\n -> (n^2) + (a*n) + b) [1..max a b]
  where
    primeOrZero n = (n==0) || (prime $ abs n)

-- Convenience functions for grabbng bits from tuples
third :: (b,b,a,b) -> a
third (_,_,x,_)  = x

fourth :: (b,b,b,a) -> a
fourth (_,_,_,x) = x
