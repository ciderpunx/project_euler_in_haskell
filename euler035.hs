-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
-- 
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- 
-- How many circular primes are there below one million?
import Data.List -- for tails, inits
import Prime

-- This solution is workmanlike but not especially fast.

main :: IO () 
main = print answer

answer :: Integer
answer = howManyCircPrimesBelow 1000000

-- How many circular numbers are there less than n.
howManyCircPrimesBelow :: Integer -> Integer
howManyCircPrimesBelow n = toInteger . length $ circPrimesLT n

-- Find all circular primes less than n
circPrimesLT :: Integer -> [Integer]
circPrimesLT n = filter allRotsPrime $ primesTo n

-- Are all rotations of this Integer prime?
allRotsPrime :: Integer -> Bool
allRotsPrime n = all prime $ map toI $ rots $ show n

-- Just for convenience, convert string to integer
toI :: String -> Integer
toI x = read x::Integer

-- Get all rotations of a list (a string usually) eg ["197"] -> ["971", "719", "197"]
-- I /was/ doing a permute, but actually only need a rotate. 
rots :: [a] -> [[a]]
rots xs = tail $ zipWith (++) (tails xs) (inits xs)
