primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | True    =     sieve [x | x <- xs, x `mod` p > 0] (head t^2) t

-- Its massively slow, like 30 seconds, but it works
sumOfPrimesLessThan2Million = sum . takeWhile (<2000000) $ primes

-- And the answer is:
-- 142913828922
