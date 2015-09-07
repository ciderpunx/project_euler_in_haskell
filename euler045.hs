-- Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
-- Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
-- Pentagonal 	  	Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
-- Hexagonal 	  	Hn=n(2n−1) 	  	1, 6, 15, 28, 45, ...
-- 
-- It can be verified that T285 = P165 = H143 = 40755.
-- 
-- Find the next triangle number that is also pentagonal and hexagonal.
import Data.Set (Set, member, fromList)

main :: IO ()
main = 
    print answer

answer :: Int
answer =
    head $ dropWhile (<=40755) candidates 

-- take hexagonal/pentagonal numbers from 1 to this number.
-- found by tweaking!
maxN :: Int
maxN = 32000

candidates :: [Int]
candidates = 
    [h | h <- hexagonals, pentagonal h && h > 1]


pentagonal :: Int -> Bool
pentagonal n =
    n `member` pentagonalsS

pentagonalsS :: Set Int
pentagonalsS =
    fromList pentagonals

pentagonals :: [Int]
pentagonals =
    map (\n -> (n*(3*n-1))`div`2) [1..maxN]

hexagonals :: [Int]
hexagonals =
    map (\n -> n*(2*n-1)) [1..maxN]

-- We only generate our hexagonals, so the instrumentation for testing if a number is xexagonal is not needed.
-- hexagonal :: Int -> Bool
-- hexagonal n =
--     n `member` hexagonalsS
-- 
-- hexagonalsS :: Set Int
-- hexagonalsS =
--     fromList hexagonals
-- 

-- All hexagonal numbers are also triangular so no need for this part
--
-- triangular :: Int -> Bool
-- triangular n =
--     n `member` trianglesS
-- 
-- trianglesS :: Set Int
-- trianglesS =
--     fromList triangles
-- 
-- triangles :: [Int]
-- triangles =
--     map (\n -> (n*(n+1))`div`2) [1..maxN]
-- 
