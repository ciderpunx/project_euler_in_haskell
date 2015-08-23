import Data.List
import Data.Array
import qualified Data.Map  as M

-- based on previous work on q12
primes :: [Integer]
primes = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | otherwise = sieve [y | y <- xs, y `mod` p > 0] (head t^2) t

primeFactors :: Integer -> [Integer]
primeFactors n = 
		factor n primes
  where
    factor n (p:ps) 
        | p * p > n      = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps


d n = product [(p * product g - 1) `div` (p - 1) | g <- group $ primeFactors n, let p = head g] - n

isAbundant n  = n /= 1 && d n > n
isPerfect  n  = n /= 1 && d n == n
isDeficient n = n /= 1 && d n < n


mx = 28123

absLookup          = listArray (1,mx) $ map isAbundant [1..mx]
abundants          = filter (absLookup !) [1..mx]
possibleAbsFor x   = takeWhile (<= x `div` 2) abundants
otherAbsFor x      = map (x-) $ possibleAbsFor x 
canBeMadeFromAbs x = any (absLookup !) $ otherAbsFor x
answer             = sum $ filter (not . canBeMadeFromAbs) $ [1..mx]

-- My original implementation runs like a dog
abundantsTo n = filter (isAbundant) [1..n]
sums []     = []
sums (x:xs) = (map (\y -> ((y+x),True)) xs) : [(x+x,True)] : sums xs  
asums = M.fromList . concat . sums $ abundantsTo mx 
notAsums = filter (\x -> not $ M.member x asums) [1..mx]
hashAnswer =  sum notAsums
