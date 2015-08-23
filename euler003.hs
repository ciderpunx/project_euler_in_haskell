-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

-- A nice wrapper function that returns only the largest prime factor (the list is always sorted, so lasting it is fine)
getLargestPrimeFactorOf n = last $ getPrimeFactors n 2 []

-- Returns a sorted list of prime factors for a number
getPrimeFactors n p factors
    | n `mod` p == 0 = if n `div` p == 1          -- if our prime divides our remaining n then its the last factor
                       then reverse $ p:factors
                       else getPrimeFactors (n `div` p) 2 (p:factors)
    | otherwise      = getPrimeFactors n (nextPrime p) factors

-- get the next highest prime from primes
nextPrime n = head . dropWhile ( <= n) $ primes

-- source of primes to try, from the haskell wiki :-)
primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | True    =     sieve [x | x <- xs, x `mod` p > 0] (head t^2) t
