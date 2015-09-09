-- There are exactly ten ways of selecting three from five, 12345:
-- 
-- 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
-- 
-- In combinatorics, we use the notation, 5C3 = 10.
-- 
-- In general,
-- nCr = 	
-- n!
-- r!(n−r)!
-- 	,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
-- 
-- It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
-- 
-- How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
main :: IO ()
main =
    print answer

answer :: Int
answer =
    length candidates

candidates :: [(Double, Double, Double)]
candidates =
    [(n,r,x) | n <- [1..100], r <- [1..n], let x = nChooseR n r, x > 1000000]

nChooseR :: Double -> Double -> Double
nChooseR n r =
    fac n / (fac r * fac (n - r))

fac :: Double -> Double
fac n =
    product [1..n]
