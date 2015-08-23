import Data.List

main = putStrLn . show $ head answer

answer :: [Integer]
answer = [ read $ (show a ++ show b ++ show c) | 
           a <- cs, 
           b <- dropWhile (<=a) cs, 
           sort (show a) == sort (show b), 
           let c = 2*b-a, -- mathematically we know that this must be true, for 
                          -- the next permutation
           prime c && sort (show a) == sort (show c)
         ]

cs = filter (>1487) $ primesTo 9999

primesTo :: Integer -> [Integer]
primesTo n = takeWhile ( < n) primes

primes :: [Integer]
primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | otherwise = sieve [y | y <- xs, y `mod` p > 0] (head t^2) t

prime n = 
  n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
