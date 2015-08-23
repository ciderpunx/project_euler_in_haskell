prime :: Integer -> Bool
prime n | n < 1 = error "not a positive integer"
				| n == 1 = False
				| otherwise = ldp n == n

ldp :: Integer -> Integer
ldp n = ldpf (primesTo n) n

ldpf :: [Integer] -> Integer -> Integer
ldpf [] n = 0
ldpf (p:ps) n | rem n p == 0 = p
							| p^2 > n			 = n
							| otherwise		 = ldpf ps n

primesTo :: Integer -> [Integer]
primesTo n = takeWhile ( < n) primes

primes :: [Integer]
primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | otherwise    =     sieve [x | x <- xs, x `mod` p > 0] (head t^2) t
