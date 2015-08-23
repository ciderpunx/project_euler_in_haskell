import Data.List (maximumBy, tails, inits)
import Data.Foldable (foldr') -- we use strict version to be kind to the stack and 
                              -- get the answer quickly

main = putStrLn . show $ consecutivePrimeSum' 1000000

-- Eventually came up with this gnarly one-liner
-- 4000 is rather large and has to be processed; could have a nicer way to 
-- guess that bit, or make it lazy somehow -- originally just started using 
-- powers of 10
consecutivePrimeSum limit = 
    maximumBy (\a b -> compare (fst $ snd a) (fst $ snd b) ) $ 
    filter (\ys -> (fst ys) < limit && (prime $ fst ys)) $ 
    map (\xs -> (sum xs, (length xs, xs))) $ filter (/= []) $ 
    concatMap tails $ inits $ primesTo 4000

-- However, it'd be much nicer to break this out into functions, reduce the number of 
-- list traversals and get rid of intermediary tuples 
-- Also uses a strict fold, meaning using less than half the memory and getting 
-- the answer in 0.5 seconds rather than 1.2 seconds
consecutivePrimeSum' limit =
   (sum answer, length answer, answer) 
  where
    answer = getBestist limit candidates
    getBestist limit xs = 
      foldr' (\x y -> if length x > length y
                     then if (sum x) < limit && prime (sum x) 
                          then x
                          else y
                     else y
      ) [] xs
    candidates = concatMap tails $ inits pt
    pt = primesTo 4000

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
