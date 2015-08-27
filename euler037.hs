-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
import Prime

main :: IO ()
main = print answer 

-- get the sum of the first 11 trunctable primes
answer :: Integer
answer =
    sum $ take 11 $ filter (\n -> n>7 && trunctablePrime 10 n) $ primes

-- Given a power of ten and a prime, if the power of ten is bigger than the prime
-- we got bigger than the prime and thus all bits are prime
-- otherwise, divide the prime by the power of ten
-- and check both the quotient and remainder are prime then raise the power of ten and
-- go back round...
trunctablePrime :: Integer -> Integer -> Bool
trunctablePrime d p =
    d > p || (prime q && prime r && (trunctablePrime (10*d) p))
  where
    (q,r) = p `quotRem` d
