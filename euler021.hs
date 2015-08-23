import Data.Array

-- based on previous work on q12
-- can go faster if you use a quick prime factorization method for figuring out
-- divisors
divisors :: Integer -> [Integer]
divisors n = 
		filter ((==0) . mod n) [1..limit]
	where 
		limit					= n `div` 2 



-- work out 'd' the sum of the divisors of n
d n = sum $ divisors n

ds = array (1,9999) [(i,d i) | i <- [1..9999]] 

amicable m n = m < n && n < 10000 && ds ! n == m

allAmicables = [(m,n) | m <- [2..9999], let n = ds ! m, amicable m n]

main = print $ sum (map (\(x,y) -> x+y) allAmicables)
