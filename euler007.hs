-- 	Find the 10001st prime.

-- I strted out with the classic list comprehension version of the net of
-- eratosthenes:
primes = 2 : sieve [3..]
  where sieve (p:ns) = p : sieve [x | x <- ns, x `mod` p > 0]

-- As it turns out that is slooow (think about it), so I worked through some of the 
-- wiki options on prime finding from http://www.haskell.org/haskellwiki/Prime_numbers
primes' = 2 : sieve [3,5..]
  where  sieve (p:ns) = p : sieve (ns `minus` [p*p, p*p+2*p..])

primesPT = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | True    =     sieve [x | x <- xs, x `mod` p > 0] (head t^2) t

primesPE = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t


-- Now I can state my operation in terms of one of those prime finders:
nthPrime n = last . take n $ primesPT

-- I call this with 10001 and get 
-- *Main Data.List> nthPrime 10001
-- 104759


-- Borrowed from Data.List.Ordered, used for list filtering...
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
